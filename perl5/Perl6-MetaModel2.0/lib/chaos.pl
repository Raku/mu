#!/usr/bin/perl

use strict;
use warnings;

use Carp 'confess';
use Hash::Util 'lock_keys';
use Scalar::Util 'blessed';
use Data::Dumper ();

our $DISPATCH_TRACE = 0;

{   ## Opaque instances     
    # These functions build opaque instance
    # variables and acces their internal data
    # this is basically the implementation of
    # the p6opaque canidate as described in A12 
    
    # Every instance should have a unique ID
    my $GLOBAL_INSTANCE_COUNT = 0;

    # Input: reference to class and a slurpy attr hash
    sub ::create_opaque_instance ($%) {
        my ($class, %attrs) = @_;
        (defined $class) 
            || confess "Cannot create an opaque instance without a class";
        my $instance = bless {
            'id'    => ++$GLOBAL_INSTANCE_COUNT,
            'class' => $class,
            'attrs' => \%attrs,
        }, 'Dispatchable';
        lock_keys(%{$instance});
        return $instance;
    }

    # Accessors for the inside of the opaque structure
    sub ::opaque_instance_id ($) { 
        my $instance = shift;
        (defined $instance && blessed($instance) eq 'Dispatchable')
            || confess "Bad instance (" . ($instance || 'undef') . ")";
        $instance->{id};
    }
    sub ::opaque_instance_class ($) { 
        my $instance = shift;
        (defined $instance && blessed($instance) eq 'Dispatchable')
            || confess "Bad instance (" . ($instance || 'undef') . ")";        
        ${$instance->{class}};
    }
    sub ::opaque_instance_attrs ($) { 
        my $instance = shift;
        (defined $instance && blessed($instance) eq 'Dispatchable')
            || confess "Bad instance (" . ($instance || 'undef') . ")";
        $instance->{attrs};
    }
}

{   ## Perl 6 Global Functions
    # This is a set of global functions
    # which implement certain global 
    # characteristics of the Perl 6 object 
    # space. 
    
    ## $?SELF, $?CLASS, $?PACKAGE and $?ROLE
    # this mimics the above magical variables    
    $::SELF    = undef;
    $::CLASS   = undef;   
    $::PACKAGE = undef;
    $::ROLE    = undef;
    # this allows us to investigate from where the
    # call came, it is used in private methods 
    $::CALLER::CLASS = undef;
    {
        my (@SELF, @CLASS, @PACKAGE, @ROLE);        
        sub ::bind_SELF_and_CLASS ($$) {
            my ($self, $class) = @_;
            (defined $self && defined $class)
                || confess "Must have defined values to bind $?SELF and $?CLASS"; 
            $::CALLER::CLASS = (@CLASS ? $CLASS[-1] : undef);               
            push @SELF    => ($::SELF    = $self );
            push @CLASS   => ($::CLASS   = $class);        
            # the $?PACKAGE is the same as $?CLASS
            push @PACKAGE => ($::PACKAGE = $class);              
        }
    
        sub ::unbind_SELF_and_CLASS () { 
            pop @SELF;    $::SELF    = (@SELF    ? $SELF[-1]    : undef);
            pop @CLASS;   $::CLASS   = (@CLASS   ? $CLASS[-1]   : undef); 
            pop @PACKAGE; $::PACKAGE = (@PACKAGE ? $PACKAGE[-1] : undef);
            # NOTE:
            # we do not bother to re-bind the $::CALLER::CLASS here
            # because it is only used internally, and only used by
            # the private methods so far, if we expose it to the user
            # we will have to restore it's previous binding here           
        }   
        
        # NOTE:
        # these are used during class creation
        # to bind $?CLASS within the class closure
        sub ::bind_CLASS ($) {
            my ($class) = @_;
            (defined $class)
                || confess "Must have a defined value to bind to $?CLASS";
            push @CLASS => ($::CLASS = $class);   
        } 
        
        sub ::unbind_CLASS () { 
            pop @CLASS; $::CLASS = (@CLASS ? $CLASS[-1] : undef);
        }    
        
        # NOTE:
        # these are used during forcing submethods
        # to bind $?SELF
        sub ::bind_SELF ($) {
            my ($self) = @_;
            (defined $self)
                || confess "Must have a defined value to bind to $?SELF";
            push @SELF => ($::SELF = $self);   
        } 

        sub ::unbind_SELF () { 
            pop @SELF; $::SELF = (@SELF ? $SELF[-1] : undef);
        }     
        
        # NOTE:
        # these are used when STORE-ing subs in a package
        # to make sure they have $?PACKAGE in their scope
        sub ::bind_PACKAGE ($) {
            my ($pkg) = @_;
            (defined $pkg)
                || confess "Must have a defined value to bind to $?PACKAGE";
            push @PACKAGE => ($::PACKAGE = $pkg);   
        } 

        sub ::unbind_PACKAGE () { 
            pop @PACKAGE; $::PACKAGE = (@PACKAGE ? $PACKAGE[-1] : undef);
        }  
        
        # a convience wrapper for binding/unbinding $?PACKAGE
        sub ::make_package_sub {
            my ($sub, $pkg) = @_;
            return sub {
                ::bind_PACKAGE($pkg);
                my @rval = $sub->(@_);
                ::unbind_PACKAGE();
                return wantarray ? @rval : $rval[0];          
            };
        }                
    }

    # these are actually two functiosn
    # which are detailed in A12 and are
    # called 'iterators', they basically
    # are ways to walk a dispatcher() 
    # object. They are very useful :)
    sub ::WALKMETH ($$;%) {
        my ($dispatcher, $label, %opts) = @_;
        (defined $dispatcher && ref($dispatcher) eq 'CODE')
            || confess "Bad dispatcher (" . ($dispatcher || 'undef') . ")";
        (defined $label)
            || confess "You must provide a method label for WALKMETH";
        warn "::WALKMETH called for ($label) ... for " . $opts{for} if $DISPATCH_TRACE;
        while (my $current = $dispatcher->()) {
            warn "\t> Currently looking in id($current) for ($label)" if $DISPATCH_TRACE;
            if ($current->has_method($label, %opts)) {
                return $current->get_method($label, %opts);
            }
        }
        return undef;        
    }
    
    sub ::WALKCLASS ($) {
        my ($dispatcher) = @_;
        (defined $dispatcher && ref($dispatcher) eq 'CODE')
            || confess "Bad dispatcher (" . ($dispatcher || 'undef') . ")";        
        return $dispatcher->();        
    }
 
    ## Attribute Primatives
    # this is kind of a hack currently ..
    
    sub ::make_attribute ($) {
        my ($name) = @_;
        (defined $name)
            || confess "You must provide a name for the attribute";
        return bless \$name => 'Perl6::Attribute';
    }
    
    sub ::make_class_attribute ($) {
        my ($name) = @_;
        (defined $name)
            || confess "You must provide a name for the attribute";        
        return bless \$name => 'Perl6::ClassAttribute';
    }     
    
    sub ::instantiate_attribute_container ($) {
        my ($attr) = @_;
        (blessed($attr) && $attr->isa('Perl6::Attribute'))
            || confess "You must provide an attribute to instantiate";        
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
    
    # This sub basically takes a subroutine
    # and wraps it so that it binds values 
    # to $?SELF and $?CLASS are bound within
    # it.    
    sub ::bind_method_to_class ($$) {
        my ($method, $associated_with) = @_;
        (defined $method && blessed($method) && $method->isa('Perl6::Method'))
            || confess "Bad method (" . ($method || 'undef') . ")";
        (blessed($associated_with) && blessed($associated_with) eq 'Dispatchable')
            || confess "You must associate the method with a class";          
        # now wrap the method once again, 
        # making sure to rebless it
        $_[0] = bless sub {
            my $invocant = shift;
            # NOTE: 
            # we die if we do not have a
            # defined invocant and a class
            # (see bind_SELF_and_CLASS above)
            ::bind_SELF_and_CLASS($invocant, $associated_with);
            my @rval = $method->($invocant, @_);
            ::unbind_SELF_and_CLASS();
            return wantarray ? @rval : $rval[0];            
        } => blessed($method);            
    }

    # make a basic method ...
    sub ::make_method ($) {
        my ($method) = @_;
        (defined $method && ref($method) eq 'CODE')
            || confess "Bad method body (" . ($method || 'undef') . ")";     
        return bless $method => 'Perl6::Method';
    }
    
    # a class method is the same as a regular 
    # method, it just has a class as an invocant
    sub ::make_class_method ($) {  
        my ($method) = @_;
        (defined $method && ref($method) eq 'CODE')
            || confess "Bad method body (" . ($method || 'undef') . ")";          
        return bless $method => 'Perl6::ClassMethod';
    } 
    
    # this is a private method
    sub ::make_private_method ($) {
        my ($method) = @_;
        (defined $method && ref($method) eq 'CODE')
            || confess "Bad method body (" . ($method || 'undef') . ")"; 
        # then the private method wrapper is wrapped
        # around the basic method body. This checks
        # to see the $?CLASS of the previous call
        # and know if we are being called from within
        # our own class or not.
        return bless sub {
            confess "Cannot call private method from different class"
                unless defined $::CALLER::CLASS &&
                       ::opaque_instance_id($::CALLER::CLASS) 
                       eq 
                       ::opaque_instance_id($::CLASS);   
            return $method->(@_);
        } => 'Perl6::PrivateMethod';
    }
    
    # a submethod is a method which has an 
    # implicit:
    #    next METHOD unless $?SELF.class =:= $?CLASS
    # in it. Submethods are also special in that
    # this implcit test can be overridden.
    sub ::make_submethod ($) {
        my ($method) = @_;
        (defined $method && ref($method) eq 'CODE')
            || confess "Bad method body (" . ($method || 'undef') . ")";     
        return bless sub {
            if (!$_[0] || $_[0] ne $Perl6::Submethod::FORCE) {
                return ::next_METHOD()
                    if ::opaque_instance_id(::opaque_instance_class($_[0])) 
                       != 
                       ::opaque_instance_id($::CLASS); 
            }
            elsif ($_[0] eq $Perl6::Submethod::FORCE) {
                shift(@_);
            }   
            # XXX -
            # this is currently a hack to rebind the 
            # $?SELF variable, it is needed until we 
            # can properly handle the forcing of method
            # calls without the use of the FORCE 
            # parameter
            ::bind_SELF($_[0]);
            my @rval = $method->(@_);
            ::unbind_SELF();
            return wantarray ? @rval : $rval[0];            
        } => 'Perl6::Submethod';
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
    sub ::dispatcher ($$$;$) {
        (blessed($_[0]) && blessed($_[0]) eq 'Dispatchable' &&
         defined $_[1]  && 
         defined $_[2]  && ref($_[2]) eq 'ARRAY')
            || confess "dispatch must have an invocant, a label and an arguments array";        
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

    # this will handle all dispatching 
    # for the root $::Class
    sub _class_dispatch ($$$;$) {
        my ($self, $label, $args, $is_class_method) = @_; 
        warn "... entering _class_dispatch with label($label)" if $DISPATCH_TRACE;  
        my $method_table_name;
        # check the private methods
        if ($label =~ /^_/) {
            $method_table_name = '%:private_methods';
        }
        else {
            # NOTE:
            # we never have class methods in $::Class so
            # we can ignore this safely for now
            #if ($is_class_method) {
            #    $method_table_name = '%:class_methods';                
            #}
            #else {
                $method_table_name = '%:methods';                
            #}
        }
        # NOTE:
        # we need to just access stuff directly here
        # so as to avoid the method call, this
        # is needed to avoid meta-circularity issues

        # gather all the classes to look through
        my @classes = ($self);
        # we take the MRO first, however at some early
        # stages of the bootstrap, this is not yet 
        # populated with anything, so ....
        my @supers = @{::opaque_instance_attrs($self)->{'@:MRO'}};
        # if nothing is in MRO, we take the superclasses
        # because we know that is there ...
        @supers = @{::opaque_instance_attrs($self)->{'@:superclasses'}} 
            if scalar @supers == 0;
        # ... carry on,.. nothing to see here ....
        push @classes => @supers
            # however, we dont actually need to go there
            # if what we are asking for is a private method
            if $method_table_name ne '%:private_methods';

        # now try and find out method ...
        foreach my $class (@classes) {
            my $method_table = ::opaque_instance_attrs($class)->{$method_table_name};
            return $method_table->{$label}->($self, @{$args}) 
                if exists $method_table->{$label};             
        }
        confess "Method ($label) not found in \$::Class";          
    }

    sub _normal_dispatch ($$$;$) {
        my ($self, $label, $args, $is_class_method) = @_; 
        warn "... entering _normal_dispatch with label($label)" if $DISPATCH_TRACE;                    
        my $class = ::opaque_instance_class($self);
        
        # XXX - 
        # this seems to prevent some GC issues,.. 
        # so I am leaving it here for now
        #warn "got an undef class here ... ($self)" 
        return unless defined $class;         
        
        my @return_value;
        # check if this is a private method
        if ($label =~ /^_/) {           
            confess "Private Method ($label) not found for current class ($::CLASS)"
                unless $::CLASS->has_method($label, for => 'private');
            my $method = $::CLASS->get_method($label, for => 'private');
            @return_value = $method->($self, @{$args});  
        }
        else {   
            
            my %opts = (for => 'instance');
            if ($is_class_method) {
                %opts = (for => 'class');
                $class = $self;
            }
             
            # get the dispatcher instance ....
            my $dispatcher = $class->dispatcher(':canonical');  
            # walk the methods
            my $method = ::WALKMETH($dispatcher, $label, %opts);
            (defined $method)
                || confess "Method ($label) not found for " . $opts{for} . " ($self)";   
            # store the dispatcher state
            push @DISPATCHER => [ $dispatcher, $label, $self, $args, \%opts ];
            # call the method
            @return_value = $method->($self, @{$args});     
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
    sub ::next_METHOD () {
        (@DISPATCHER)
            || confess "Cannot call next METHOD, we have no current dispatcher";
        warn ">>>> next METHOD called" if $DISPATCH_TRACE;
        my ($dispatcher, $label, $self, $args, $opts) = @{$DISPATCHER[-1]};             
        my $method = ::WALKMETH($dispatcher, $label, %{$opts}); 
        confess "No next-method for '$label' found" unless defined $method;
        return $method->($self, @{$args});            
    }

}

###############################################################################
## Perl 5 magic sugar down here ...

{
    package class;
    
    # this package represents the "magic"
    # of the class layer. Using this 
    # we can get a class invocant which 
    # is how class methods get called 
    
    sub isa {
        $Dispatchable::AUTOLOAD = 'class::isa';
        goto &Dispatchable::AUTOLOAD;        
    }

    sub can {
        $Dispatchable::AUTOLOAD = 'class::can';
        goto &Dispatchable::AUTOLOAD;        
    }
    
    sub AUTOLOAD {
        $Dispatchable::AUTOLOAD = our $AUTOLOAD;
        goto &Dispatchable::AUTOLOAD;
    }
}

{ ## Perl 5 dispatcher magic
    package Dispatchable;
    
    use strict;
    use warnings;
    
    use overload 
        '0+' => sub { $_[0]->{id} },       
        '""' => sub {
            "#<" . (${$_[0]->{class}}->{attrs}->{'$:name'} || 'AnonClass') . "=(" . $_[0]->{id} . ")>"
        },     
        fallback => 1;

    sub isa { our $AUTOLOAD = 'isa'; goto &AUTOLOAD; }
    sub can { our $AUTOLOAD = 'can'; goto &AUTOLOAD; }

    sub AUTOLOAD {
        my @autoload = (split '::', our $AUTOLOAD);
        my $label = $autoload[-1];
        my $self = shift;   
        # NOTE:
        # DESTROYALL is what should really be called
        # so we just deal with it like this, and we deal
        # with it here since this is a p5 issue.
        if ($label =~ /DESTROY/) {
            $label = 'DESTROYALL';
            # XXX -
            # I am not 100% sure why I need the following 
            # line, but I am guessing it has something to
            # do with how perl 5 does not always do ordered
            # destruction of objects. I might be able to 
            # fix this issue with en END block
            return unless defined $self && 
                          defined ::opaque_instance_class($self) && 
                          defined $::Object  &&
                          defined $::Package &&
                          defined $::Module  &&
                          defined $::Class;
        }        
        # go about our dispatching ....      
        return ::dispatcher($self, $label, \@_, ($autoload[0] eq 'class' ? 1 : 0));
    }
}

## add on to Class::Multimethods::Pure 
## to make method composition easier

{
    use Class::Multimethods::Pure ();
    
    sub ::multi_sub {
        my $name = shift or return;

        if (@_) {
            my @params;
            until (!@_ || ref $_[0] eq 'CODE') {
                if ($_[0] =~ /^-/) {
                    my ($k, $v) = splice @_, 0, 2;
                    $k =~ s/^-//;
                    $Class::Multimethods::Pure::MULTIPARAM{$k} = $v;
                }
                else {
                    my $type = shift;
                    unless (ref $type) {
                        if (Class::Multimethods::Pure::Type::Unblessed->is_unblessed($type)) {
                            $type = Class::Multimethods::Pure::Type::Unblessed->new($type);
                        }
                        else {
                            $type = Class::Multimethods::Pure::Type::Package->new($type);
                        }
                    }
                    push @params, $type;
                }
            }

            return () unless @_;

            my $code = shift;

            my $multi = $Class::Multimethods::Pure::MULTI{$name} ||= 
                    Class::Multimethods::Pure::Method->new(
                        Core    => 'Class::Multimethods::Pure::Method::DumbCache',
                        Variant => $Class::Multimethods::Pure::MULTIPARAM{$name}{Variant},
                    );

            $multi->add_variant(\@params, $code);
        }

        return Class::Multimethods::Pure::make_wrapper($name);
    }    
}



1;
