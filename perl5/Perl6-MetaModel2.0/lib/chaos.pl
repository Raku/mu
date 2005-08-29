#!/usr/bin/perl

use strict;
use warnings;

use Carp 'confess';
use Hash::Util 'lock_keys';
use Scalar::Util 'blessed';

our $DISPATCH_TRACE = 0;

{   ## Opaque instances     
    # These functions build opaque instance
    # variables and acces their internal data
    # this is basically the implementation of
    # the p6opaque canidate as described in A12 
    
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

{   ## Perl 6 Global Functions
    # This is a set of global functions
    # which implement certain global 
    # characteristics of the Perl 6 object 
    # space. 

    ## $?SELF
    # this mimics the $?SELF variable
    my @SELF = ();
    sub ::SELF {
        confess 'Cannot access $?SELF outside of valid context'
            unless @SELF;
        $SELF[-1];
    }

    ## $?CLASS
    # this mimics the $?CLASS variable    
    my @CLASS = ();
    sub ::CLASS {
        confess 'Cannot access $?CLASS outside of valid context'
            unless @CLASS;
        $CLASS[-1];        
    }

    # these are actually two functiosn
    # which are detailed in A12 and are
    # called 'iterators', they basically
    # are ways to walk a dispatcher() 
    # object. They are very useful :)
    sub ::WALKMETH {
        my ($dispatcher, $label, %opts) = @_;
        warn "::WALKMETH called for ($label) ... for " . $opts{for} if $DISPATCH_TRACE;
        while (my $current = $dispatcher->()) {
            warn "\t> Currently looking in id(" . ::opaque_instance_id($current) . ") for ($label)" if $DISPATCH_TRACE;
            if ($current->has_method($label, %opts)) {
                return $current->get_method($label, %opts);
            }
        }
        return undef;        
    }
    
    sub ::WALKCLASS {
        my ($dispatcher, %opts) = @_;
        return $dispatcher->();        
    }
 
    ## Attribute Primatives
    # this is hack currently ..
    
    sub ::make_attribute {
        my ($name) = @_;
        return bless \$name => 'Perl6::Attribute';
    }
    
    sub ::make_class_attribute {
        my ($name) = @_;
        return bless \$name => 'Perl6::ClassAttribute';
    }     
    
    sub ::instantiate_attribute_container {
        my ($attr) = @_;
        return [] if ${$attr} =~ /^@/;
        return {} if ${$attr} =~ /^%/;        
        return undef;
    }
    
    {   ## Attribute "Types"
        # these are just packages
        # so that I can 'tag' the
        # types of the attributes

        package Perl6::Attribute;

        package Perl6::ClassAttribute;        
        @Perl6::ClassAttribute::ISA = ('Perl6::Attribute');     
    }     
 
    ## Method primitives
    # since Perl 5 does not have method
    # primatives, we have to make them.

    # a method is basically a subroutine
    # in which $?SELF and $?CLASS are bound
    # values. This method builder will wrap
    # a sub to 'bind' those two values, 
    # and 'tag' the type of the sub.
    sub ::make_method ($$) {
        my $method = shift;
        my $associated_with = shift;
        return bless sub {
            my $invocant = shift;
            # XXX - 
            # should we die if we 
            # have no invocant? we always
            # need an invocant in a method
            push @CLASS => $associated_with;            
            push @SELF => $invocant;
            my @rval = $method->($invocant, @_);
            pop @SELF;
            pop @CLASS;
            return wantarray ? @rval : $rval[0];            
        } => 'Perl6::Method';
    }
    
    # a class method is the same as a regular 
    # method, it just has a class as an invocant
    sub ::make_class_method ($$) {    
        return bless ::make_method($_[0], $_[1]) => 'Perl6::ClassMethod';
    } 
    
    # this is a private method
    sub ::make_private_method ($$) {
        # we create the basic method wrapper first
        my $method = ::make_method($_[0], $_[1]);
        my $associated_with = $_[1];        
        # then the private method wrapper is wrapped
        # around the wrapped basic method. This allows
        # us to see the $?CLASS of the previous call
        # and know if we are being called from within
        # our own class or not.
        return bless sub {
            confess "Cannot call private method from different class"
                unless ::opaque_instance_id(::CLASS()) 
                    eq ::opaque_instance_id($associated_with);   
            return $method->(@_);
        } => 'Perl6::PrivateMethod';
    }
    
    # a submethod is a method which has an 
    # implicit:
    #    next METHOD unless $?SELF.class =:= $?CLASS
    # in it. So first we wrap the sub in that test
    # and then we make it a regular method ($?SELF and 
    # $?CLASS aware). Submethod are also special in that
    # this implcit test can be overridden.
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
            elsif ($_[0] eq $Perl6::Submethod::FORCE) {
                shift(@_);
            }
            return $method->(@_);
        }, $associated_with) => 'Perl6::Submethod';
    }   
    
    {   ## Method "Types"
        # these are just packages
        # so that I can 'tag' the
        # types of the methods
        
        package Perl6::Method;

        package Perl6::ClassMethod;        
        @Perl6::ClassMethod::ISA = ('Perl6::Method');
        
        package Perl6::PrivateMethod;        
        @Perl6::PrivateMethod::ISA = ('Perl6::Method');        
        
        package Perl6::Submethod;
        @Perl6::Submethod::ISA   = ('Perl6::Method');
        $Perl6::Submethod::FORCE = bless \(my $var) => 'FORCE';
    }       
}

{
    
    ## dispatcher
    # the Perl 6 method dispatcher needs to
    # have some basic abilities, one of which 
    # is to be able to stash the current dispatcher
    # for use by 'next METHOD', the other of which 
    # is to deal with the special case of the
    # early $::Class object.
    my @DISPATCHER = ();
    sub ::dispatcher {
        # deal with the $::Class special case ...
        # NOTE: 
        # we refer to $::Class here even though
        # it will not be defined yet, however, 
        # we know this wont be used anyway 
        if ($_[0] == $::Class) {
             warn "got the ::Class, going to &_class_dispatch" if $DISPATCH_TRACE;
             goto &_class_dispatch;
        }
        else {
             warn "Not a ::Class, going to &_normal_dispatch" if $DISPATCH_TRACE;            
             goto &_normal_dispatch;
        }
    }

    # NOTE:
    # since $::Class is an instance of itself, we
    # can safely ignore the issue of class methods
    # in this dispatcher, since there will be none
    # because the class it an instance of itself :)
    sub _class_dispatch {
        my ($self, $label) = (shift, shift);  
        warn "... entering _class_dispatch with label($label)" if $DISPATCH_TRACE;  
        my $method_table_name;
        # check the private methods
        if ($label =~ /^_/) {
            $method_table_name = '%:private_methods';
        }
        else {
            $method_table_name = '%:methods';
        }
        # NOTE:
        # we need to just access stuff directly here
        # so as to avoid the method call, this
        # is needed to avoid meta-circularity issues

        # gather all the classes to look through
        my @classes = ($self);
        # we need to just dig one level deep here since
        # we know it is a class, that is okay, but
        # still it is a hack. 
        push @classes => @{::opaque_instance_attrs($self)->{'@:superclasses'}}
            # however, we dont actually need to go there
            # if what we are asking for is a private method
            if $method_table_name ne '%:private_methods';

        # now try and find out method ...
        foreach my $class (@classes) {
            my $method_table = ::opaque_instance_attrs($class)->{$method_table_name};
            return $method_table->{$label}->($self, @_) 
                if exists $method_table->{$label};             
        }
        confess "Method ($label) not found for meta-instance ($self)";          
    }

    sub _normal_dispatch {
        my ($self, $label, @args) = @_; 
        warn "... entering _normal_dispatch with label($label)" if $DISPATCH_TRACE;                    
        # NOTE:
        # DESTROYALL is what should really be called
        # so we just deal with it like this :)
        if ($label =~ /DESTROY/) {
            $label = 'DESTROYALL';
        }
        
        my $class = ::opaque_instance_class($self);
        
        my @return_value;
        # check if this is a private method
        if ($label =~ /^_/) {           
            confess "Private Method ($label) not found for instance ($self)"
                unless $class->has_method($label, for => 'private');
            my $method = $class->get_method($label, for => 'private');
            @return_value = $method->($self, @args);  
        }
        else {   
            # get the dispatcher instance ....
            my $dispatcher = _class_dispatch($class, 'dispatcher', (':canonical'));  
            
#            $DISPATCH_TRACE = 1;          
            # walk the methods
            my $method = ::WALKMETH($dispatcher, $label);
            (defined $method)
                || confess "Method ($label) not found for instance ($class)";   
            # store the dispatcher state
            push @DISPATCHER => [ $dispatcher, $label, $self, \@args ];
            # call the method
            @return_value = $method->($self, @args);     
            # we can dispose of the dispatcher state
            # as it  should never be called outside of 
            # a method invocation
            pop @DISPATCHER;
        }
        # return our values
        return wantarray ?
                    @return_value
                    :
                    $return_value[0];          
    }

    ## next METHOD;
    # this mimics the 'next METHOD' 
    # construct to go to the next
    # applicable method 
    sub ::next_METHOD {
        my ($dispatcher, $label, $self, $args) = @{$DISPATCHER[-1]};             
        my $method = ::WALKMETH($dispatcher, $label); 
        return $method->($self, @{$args});            
    }

}

###############################################################################
## Perl 5 magic sugar down here ...

{ ## Perl 5 dispatcher magic
    package Dispatchable;
    
    use strict;
    use warnings;
    
    use Carp 'confess';

    sub isa { our $AUTOLOAD = 'isa'; goto &AUTOLOAD; }
    sub can { our $AUTOLOAD = 'can'; goto &AUTOLOAD; }

    sub AUTOLOAD {
        my @autoload = (split '::', our $AUTOLOAD);
        my $label = $autoload[-1];
        # XXX -
        # this is not okay, we need to pass the DESTORY on
        # but it is not working now, this might be a trivial 
        # issue though ... have to see as development progresses
        return if $label =~ /DESTROY/ && not defined &::dispatch;
        my $self = shift;          
        return ::dispatcher($self, $label, @_);
    }
}

1;
