#!/usr/bin/perl

use strict;
use warnings;

use Hash::Util 'lock_keys';
use Carp 'confess';
use Scalar::Util 'blessed';

{
    # Every instance should have a unique ID
    my $instance_counter = 0;

    # Input: reference to class and a slurpy attr hash
    sub ::create_opaque_instance ($%) {
        my ($class, %attrs) = @_;
        my $instance = bless {
            'id'    => ++$instance_counter,
            'class' => $class,
            'attrs' => \%attrs,
        }, 'Dispatchable';
        lock_keys(%{$instance});
        return $instance;
    }

    # Accessors for the inside of the opaque structure
    sub ::opaque_instance_id    ($) : lvalue { shift->{id}       }
    sub ::opaque_instance_class ($) : lvalue { ${shift->{class}} }
    sub ::opaque_instance_attrs ($) : lvalue { shift->{attrs}    }
}

{
    package Dispatchable;
    
    use strict;
    use warnings;
    
    use Carp 'confess';

    sub isa { our $AUTOLOAD = 'isa'; goto &AUTOLOAD; }
    sub can { our $AUTOLOAD = 'can'; goto &AUTOLOAD; }

    ## XXX -
    ## this should eventually use a ::dispatch 
    ## function instead of keeping the dispatch
    ## code here ...
    sub AUTOLOAD {
        my $label = (split '::', our $AUTOLOAD)[-1];
        return if $label eq 'DESTROY';

        my $class = ::opaque_instance_class($_[0]);

        while (defined $class) {
            my $method = ::opaque_instance_attrs($class)->{'%:methods'}{$label};
            goto &$method if $method;

            # try again in the superclass
            $class = ::opaque_instance_attrs($class)->{'@:superclasses'}->[0];
        }

        confess "No method found for $label";
    }
}

{ ## METHOD closure factory
    
    # This is a set of methods to implement the
    # basic method types and functionality.
    
    # It also includes associated constructs, 
    # such as $?SELF, $?CLASS and next METHOD

    my @CURRENT_CLASS_STACK;
    my @CURRENT_INVOCANT_STACK;
    
    sub ::SELF  {}
    sub ::CLASS {}
    
    sub ::next_METHOD {}

    # private method closure factory
    my $_create = sub {
        my ($code, $associated_with, $type) = @_;
        (defined $associated_with && defined $type && defined $code) 
            || confess "Insufficient Arguments : You must provide a class this is associated with and code";    
        (ref($code) eq 'CODE') 
            || confess "Incorrect Object Type : The code arguments must be a CODE reference";
            
        # normalize the type
        $type = lc($type);

        my $method = sub {
            my ($args) = @_;
            push @CURRENT_CLASS_STACK => $associated_with;
            push @CURRENT_INVOCANT_STACK => $args->[0];    
            my @rval = $code->(@$args); 
            pop @CURRENT_INVOCANT_STACK;
            pop @CURRENT_CLASS_STACK;
            return wantarray ? @rval : $rval[0];            
        };

        $method = bless($method, 'Perl6::Class::Method')    if $type eq 'class';
        $method = bless($method, 'Perl6::Instance::Method') if $type eq 'instance';  
        $method = bless($method, 'Perl6::Role::Method')     if $type eq 'role';        

        if ($type eq 'submethod') {
            my $old = $method;
            $method = bless sub { 
                if (ref($_[0]) ne 'FORCE') {
                    return ::next_METHOD() 
                        ## XXX
                        # this should not be accessing either the 
                        # instance_data->name slot, but we cannot
                        # do anything about it for now ...
                        if ::opaque_instance_attrs(::opaque_instance_class($_[0]->[0]))->{'$:name'} 
                           ne 
                           ::opaque_instance_attrs($associated_with)->{'$:name'}; 
                }
                $old->($_[1]); 
            }, 'Perl6::SubMethod';
        }
        if ($type eq 'private') {
            my $old = $method;
            $method = bless sub {
                (::CLASS() eq $associated_with)
                    || confess "Cannot call private method from different class";
                $old->($_[0]); 
            }, 'Perl6::PrivateMethod';
        }    

        return $method;
    };
    
    # method constructors
    sub ::instance_method (&$) { $_create->(@_, 'instance')  }
    sub ::class_method    (&$) { $_create->(@_, 'class')     }
    sub ::role_method     (&$) { $_create->(@_, 'role')      }
    sub ::submethod       (&$) { $_create->(@_, 'submethod') }
    sub ::private_method  (&$) { $_create->(@_, 'private')   }   
     
    # Package types defined mostly for "tagging"
    {
        package Perl6::Method;
        
        sub do { 
            my ($self, @args) = @_;   
            return $self->(\@args);         
        }

        sub force_call { 
            my ($self, @args) = @_;   
            return $self->(bless({} => 'FORCE'), [ @args ]);         
        }        
        
        package Perl6::Class::Method;
        use base 'Perl6::Method';

        package Perl6::Instance::Method;
        use base 'Perl6::Method';

        package Perl6::Role::Method;
        use base 'Perl6::Instance::Method';

        package Perl6::PrivateMethod;
        use base 'Perl6::Method';

        package Perl6::SubMethod;
        use base 'Perl6::Method';    
    }     
}
