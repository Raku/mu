#!/usr/bin/perl

use strict;
use warnings;

use Carp 'confess';
use Hash::Util 'lock_keys';
use Scalar::Util 'blessed';

{ ## Opaque instance primitives
    
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
    sub ::opaque_instance_id    ($) { shift->{id}       }
    sub ::opaque_instance_class ($) { ${shift->{class}} }
    sub ::opaque_instance_attrs ($) { shift->{attrs}    }
}

{ ## Perl 6 Global Functions

    # $?SELF
    my @SELF = ();
    sub ::SELF {
        confess "Cannot access $?SELF outside of valid context"
            unless @SELF;
        $SELF[-1];
    }

    # $?CLASS
    my @CLASS = ();
    sub ::CLASS {
        confess "Cannot access $?CLASS outside of valid context"
            unless @CLASS;
        $CLASS[-1];        
    }

    # next METHOD;
    sub ::next_METHOD {}

    sub ::WALKMETH {}
    sub ::WALKCLASS {}
 
   ## Method primitives
    
    sub ::make_method ($$) {
        my $method = shift;
        my $associated_with = shift;
        return bless sub {
            my $invocant = shift;
            push @CLASS => $associated_with;            
            push @SELF => $invocant;
            my @rval = $method->($invocant, @_);
            pop @SELF;
            pop @CLASS;
            return wantarray ? @rval : $rval[0];            
        } => 'Perl6::Method';
    }
    
    sub ::make_classmethod ($$) {    
        return bless ::make_method($_[0], $_[1]) => 'Perl6::ClassMethod';
    } 
    
    sub ::make_submethod ($$) {
        my $method = shift;
        my $associated_with = shift;
        return bless ::make_method(sub {
            if (!$_[0] || $_[0] ne $Perl6::Submethod::FORCE) {
                return ::next_METHOD()
                    if ::opaque_instance_id(::opaque_instance_class(::SELF())) 
                       != 
                       ::opaque_instance_id(::CLASS()); 
            }
            return $method->(@_);
        }, $associated_with) => 'Perl6::Submethod';
    }   
    
    {
        package Perl6::Method;
        use strict;
        
        package Perl6::ClassMethod;        
        use base 'Perl6::Method';
        
        package Perl6::Submethod;
        use base 'Perl6::Method';
        $Perl6::Submethod::FORCE = 'FORCE';
    }       
}

###############################################
## Perl 5 magic sugar down here ...

{ ## META re-dispatcher
    package META;
    
    sub AUTOLOAD { }
}

{ ## Perl 5 dispatcher magic
    package Dispatchable;
    
    use strict;
    use warnings;
    
    use Carp 'confess';

    sub isa { our $AUTOLOAD = 'isa'; goto &AUTOLOAD; }
    sub can { our $AUTOLOAD = 'can'; goto &AUTOLOAD; }

    sub AUTOLOAD {
        my $label = (split '::', our $AUTOLOAD)[-1];
        return if $label eq 'DESTROY';

        # ...
        
        confess "No method found for $label";
    }
}

1;