#!/usr/bin/perl

use strict;
use warnings;

use Carp 'confess';
use Hash::Util 'lock_keys', 'unlock_keys';
use Scalar::Util 'blessed';
use Data::Dumper ();

our $DISPATCH_TRACE = 0;

{   ## Opaque instances     
    # These functions build opaque instance
    # variables and access their internal data.
    # This is basically the implementation of
    # the p6opaque candidate as described in A12 
    
    # Every instance should have a unique ID
    my $GLOBAL_INSTANCE_COUNT = 0;

    # Input: reference to class and a slurpy attr hash
    sub ::create_opaque_instance {
        my ($class, %attrs) = @_;
        #(defined $class) 
        #    || confess "Cannot create an opaque instance without a class";
        #my $instance = 
        bless [
            ++$GLOBAL_INSTANCE_COUNT, # 'id'    => 
            $class,                   # 'class' => 
            \%attrs,                  # 'attrs' => 
        ], 'Dispatchable';
        #lock_keys(%{$instance});  
        #lock_keys(%{$instance->[2]});              
        #return $instance;
    }

    # Accessors for the inside of the opaque structure
    sub ::opaque_instance_id { 
        #my $instance = shift;
        #(defined $_[0] && blessed($_[0]) eq 'Dispatchable')
        #    || confess "Bad instance (" . ($_[0] || 'undef') . ")";
        $_[0]->[0];
    }
    sub ::opaque_instance_class { 
        #my $instance = shift;
        #(defined $_[0] && blessed($_[0]) eq 'Dispatchable')
        #    || confess "Bad instance (" . ($_[0] || 'undef') . ")";        
        ${$_[0]->[1]};
    }
    sub ::opaque_instance_attr : lvalue { 
        #my $instance = shift;
        #(defined $instance && blessed($instance) eq 'Dispatchable')
        #    || confess "Bad instance (" . ($instance || 'undef') . ")";
        #my $label = shift;
        #(defined $label) || confess "No label to fetch";
        $_[0]->[2]->{$_[1]};
    }
    
    sub ::opaque_instance_add_new_attribute {
        #my $instance = shift;
        #(defined $instance && blessed($instance) eq 'Dispatchable')
        #    || confess "Bad instance (" . ($instance || 'undef') . ")";       
        #my ($label, $value) = @_;
        #(defined $label) || confess "No label to fetch";
        #unlock_keys(%{$_[0]->[2]}); 
        $_[0]->[2]->{$_[1]} = $_[2];       
        #lock_keys(%{$_[0]->[2]});       
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

    ## WALKMETH and WALKCLASS
    # these are actually two functions
    # which are detailed in A12 and are
    # called 'iterators', they basically
    # are ways to walk a dispatcher() 
    # object. They are very useful :)
    sub ::WALKMETH {
        #my ($dispatcher, $label, %opts) = @_;
        #(defined $dispatcher && ref($dispatcher) eq 'CODE')
        #    || confess "Bad dispatcher (" . ($dispatcher || 'undef') . ")";
        #(defined $label)
        #    || confess "You must provide a method label for WALKMETH";
        #warn "::WALKMETH called for ($label) ... for " . $opts{for} if $DISPATCH_TRACE;
        #while (my $current = $dispatcher->()) {
            #warn "\t> Currently looking in id($current) for ($label)" if $DISPATCH_TRACE;
        #    if ($current->has_method($label, %opts)) {
        #        return $current->get_method($label, %opts);
        #    }
        #}
        my $dispatcher = shift;
        my $method;
        LOOP:
            $method = ($dispatcher->() || return)->get_method(@_) || goto LOOP;
        return $method;
        #return undef;        
    }
    
    sub ::WALKCLASS {
        #my ($dispatcher) = @_;
        #(defined $dispatcher && ref($dispatcher) eq 'CODE')
        #    || confess "Bad dispatcher (" . ($dispatcher || 'undef') . ")";        
        #return $dispatcher->();        
        return $_[0]->();
    }
 
    ## Attribute Primitives
    # this is kind of a hack currently ..
    
    sub ::make_attribute ($) {
        my ($name) = @_;
        (defined $name)
            || confess "You must provide a name for the attribute";
        return bless \$name => 'Perl6::Attribute';
    }
    
    sub ::make_stub_attribute ($) {
        my ($name) = @_;
        (defined $name)
            || confess "You must provide a name for the attribute";
        return bless \$name => 'Perl6::StubAttribute';
    }    
    
    # DEPRECATED
    # Class attribute are just package level variables now
    # sub ::make_class_attribute ($) {
    #     my ($name) = @_;
    #     (defined $name)
    #         || confess "You must provide a name for the attribute";        
    #     return bless \$name => 'Perl6::ClassAttribute';
    # }     
    
    # this just initializes the instance attribute container type, in
    # perl 6 this should be done automatically
    sub ::instantiate_attribute_container ($) {
        #my ($attr) = @_;
        #(blessed($attr) && $attr->isa('Perl6::Attribute'))
        #    || confess "You must provide an attribute to instantiate";       
        return [] if ${$_[0]} =~ /^@/o;
        return {} if ${$_[0]} =~ /^%/o;        
        return undef;
    }
    
    {   ## Attribute "Types"
        # these are just packages
        # so that I can 'tag' the
        # types of the attributes

        package Perl6::Attribute;

        package Perl6::StubAttribute;        
        @Perl6::StubAttribute::ISA = ('Perl6::Attribute'); 

        # DEPRECATED
        # package Perl6::ClassAttribute;        
        # @Perl6::ClassAttribute::ISA = ('Perl6::Attribute');     
    }  
    
    # a convenience wrapper for binding/unbinding $?PACKAGE
    sub ::wrap_package_sub {
        my ($sub, $pkg) = @_;
        return sub {
            local $::PACKAGE = $pkg;
            return $sub->(@_);       
        };
    }         
 
    ## Method primitives
    # since Perl 5 does not have method
    # primitives, we have to make them.
    
    # a convenience wrapper for binding/unbinding $?ROLE
    sub ::wrap_role_method {
        my ($method, $role) = @_;
        (defined $method && blessed($method) && $method->isa('Perl6::Method'))
            || confess "You can only wrap proper methods for Roles";
        return bless sub {
            local $::ROLE = $role;
            return $method->(@_);       
        # NOTE:
        # we bless the new wrapper method 
        # into the same class as before 
        # because this operation should be 
        # fairly transparent to anything 
        # outside of the Role.
        } => blessed($method);
    }      
    
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
            # bind the previous value of $?CLASS
            local $::CALLER::CLASS = $::CLASS;
            # get all the others
            local $::SELF  = $_[0];
            local $::CLASS = local $::PACKAGE = $associated_with;
#            $associated_with;
            # and call the method ...
            $method->(@_);
        } => blessed($method);            
    }

    # make a basic method ...
    sub ::make_method ($) {
        my ($method) = @_;
        # this accounts for a strange optimization in Perl 5
        # it seems that sometimes when a sub being returned
        # from another sub has the same opcodes (a constant
        # subroutine basically), Perl 5 will optimze it and
        # return the exact same sub. This results in this
        # method being passed an already blessed method 
        # instead of a plain CODE ref. This solution is also
        # duplicated in the other ::make_*_method subs below
        return $method if blessed($method) && $method->isa('Perl6::Method');
        (defined $method && ref($method) eq 'CODE')
            || confess "Bad method body (" . ($method || 'undef') . ")";     
        return bless $method => 'Perl6::Method';
    }
    
    # make a method stub
    sub ::make_stub_method () {
        return bless sub { confess "Stub Method!" } => 'Perl6::StubMethod';
    }    
    
    # a class method is the same as a regular 
    # method, it just has a class as an invocant
    sub ::make_class_method ($) {  
        my ($method) = @_;
        return $method if blessed($method) && $method->isa('Perl6::ClassMethod');        
        (defined $method && ref($method) eq 'CODE')
            || confess "Bad method body (" . ($method || 'undef') . ")";          
        return bless $method => 'Perl6::ClassMethod';
    } 
    
    # this is a private method
    sub ::make_private_method ($) {
        my ($method) = @_;
        return $method if blessed($method) && $method->isa('Private::Method');        
        (defined $method && ref($method) eq 'CODE')
            || confess "Bad method body (" . ($method || 'undef') . ")"; 
        # then the private method wrapper is wrapped
        # around the basic method body. This checks
        # to see the $?CLASS of the previous call
        # and know if we are being called from within
        # our own class or not.
        return bless sub {
            (defined $::CALLER::CLASS && $::CALLER::CLASS->[0] == $::CLASS->[0]) 
                || confess "Cannot call private method from different class";   
            return $method->(@_);
        } => 'Perl6::PrivateMethod';
    }
    
    # a submethod is a method which has an 
    # implicit:
    #    next METHOD unless $?SELF.class =:= $?CLASS
    # in it. Submethods are also special in that
    # this implicit test can be overridden.
    sub ::make_submethod ($) {
        my ($method) = @_;
        return $method if blessed($method) && $method->isa('Perl6::Submethod');        
        (defined $method && ref($method) eq 'CODE')
            || confess "Bad method body (" . ($method || 'undef') . ")";     
        return bless sub {
            return ::next_METHOD()
                if ((!$_[0] || $_[0] ne $Perl6::Submethod::FORCE) 
                     &&
                     ${$_[0]->[1]}->[0] 
                     != 
                     $::CLASS->[0]); 
            # if it is a FORCE ....
            shift(@_) if $_[0] eq $Perl6::Submethod::FORCE;
            # XXX -
            # this is currently a hack to rebind the 
            # $?SELF variable, it is needed until we 
            # can properly handle the forcing of method
            # calls without the use of the FORCE 
            # parameter
            local $::SELF = $_[0];
            return $method->(@_);            
        } => 'Perl6::Submethod';
    }   
    
    {   ## Method "Types"
        # these are just packages
        # so that I can 'tag' the
        # types of the methods
        
        package Perl6::Method;
        
        package Perl6::StubMethod;
        @Perl6::StubMethod::ISA = ('Perl6::Method');                

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

    sub ::dispatcher {
        #(blessed($_[0]) && blessed($_[0]) eq 'Dispatchable' &&
        # defined $_[1]  && 
        # defined $_[2]  && ref($_[2]) eq 'ARRAY')
        #    || confess "dispatch must have an invocant, a label and an arguments array";        
        # deal with the $::Class special case ...
        # NOTE: 
        # we refer to $::Class here even though
        # it will not be defined yet, however, 
        # we know this wont be used anyway 
        #if ($_[0] == $::Class) {
             #warn "got the ::Class, going to &_class_dispatch" if $DISPATCH_TRACE;
        #     goto &_class_dispatch;
        #}
        #else {
             #warn "Not a ::Class, going to &_normal_dispatch" if $DISPATCH_TRACE;            
        #     goto &_normal_dispatch;
        #}
        goto &_class_dispatch if $_[0] == $::Class;
        goto &_normal_dispatch;
    }

    # this will handle all dispatching 
    # for the root $::Class
    sub _class_dispatch {
        #my ($self, $label, $args) = @_; 
        #warn "... entering _class_dispatch with label($label)" if $DISPATCH_TRACE;  
        my $method_table_name = '%:methods';
        # check the private methods
        $method_table_name = '%:private_methods' if $_[1] =~ /^_/o;
        
        # NOTE:
        # we need to just access stuff directly here
        # so as to avoid the method call, this
        # is needed to avoid meta-circularity issues

        # gather all the classes to look through
        my @classes = ($_[0]);
        # we take the MRO first, however at some early
        # stages of the bootstrap, this is not yet 
        # populated with anything, so ....
        #my @supers = @{$_[0]->[2]->{'@:MRO'}};
        # if nothing is in MRO, we take the superclasses
        # because we know that is there ...
        #@supers = @{$_[0]->[2]->{'@:superclasses'}} 
        #    if scalar @supers == 0;
        # ... carry on,.. nothing to see here ....
        push @classes => ((scalar @{$_[0]->[2]->{'@:MRO'}}) ? @{$_[0]->[2]->{'@:MRO'}} : @{$_[0]->[2]->{'@:superclasses'}})
            # however, we dont actually need to go there
            # if what we are asking for is a private method
            if $method_table_name ne '%:private_methods';

        # now try and find out method ...
        foreach my $class (@classes) {
            my $method_table = $class->[2]->{$method_table_name};
            return $method_table->{$_[1]}->($_[0], @{$_[2]}) 
                if $method_table->{$_[1]};             
        }
        confess "Method ($_[1]) not found in \$::Class";          
    }

    sub _normal_dispatch {
        #my ($self, $label, $args, $is_class_method) = @_; 
        #warn "... entering _normal_dispatch with label($label)" if $DISPATCH_TRACE;                    
        my $class = ${$_[0]->[1]};
        
        # XXX - 
        # this seems to prevent some GC issues,.. 
        # so I am leaving it here for now
        #warn "got an undef class here ... ($self)" 
        return unless defined $class;         
        
        # check if this is a private method
        if ($_[1] =~ /^_/) {           
            #confess "Private Method ($label) not found for current class ($::CLASS)"
            #    unless $::CLASS->has_method($label, for => 'private');
            my $method = $::CLASS->get_method($_[1], for => 'private') 
                      || confess "Private Method ($_[1]) not found for current class ($::CLASS)";
            return $method->($_[0], @{$_[2]});  
        }
        else {   
            
            my %opts; # = (for => 'instance');
            do { %opts = (for => 'class'); $class = $_[0] } if $_[3];
            #if ($is_class_method) {
            #    %opts = (for => 'class');
            #    $class = $self;
            #}
             
            # get the dispatcher instance ....
            my $dispatcher = $class->dispatcher(':canonical');  
            # walk the methods
            my $method = ::WALKMETH($dispatcher, $_[1], %opts);
                 
            unless (defined $method) {
                # if we find an AUTOLOAD anywhere in the chain, then we can use it ...
                $class->STORE('$AUTOLOAD' => $_[1]) 
                    if $method = ::WALKMETH($class->dispatcher(':canonical'), 'AUTOLOAD', %opts);
            }            
            
            (defined $method)
                || confess "Method ($_[1]) not found for " . ($opts{for} || 'instance') . " ($_[0])";   
            # store the dispatcher state
            #{
                #no warnings 'redefine';   
                local $::DISPATCHER = [ $dispatcher, $_[1], \%opts, $_[0], $_[2] ];     
                #local *::next_METHOD = sub {
                #    my $method = ::WALKMETH($dispatcher, $label, %opts); 
                #    confess "No next-method for '$label' found" unless defined $method;
                #    return $method->($self, @{$args});                   
                #};
                # call the method
                return $method->($_[0], @{$_[2]});     
            #}
        }        
    }

    ## next METHOD;
    # this mimics the 'next METHOD' 
    # construct to go to the next
    # applicable method it is localized
    # in each run of _normal_dispatch
    # so that 
    sub ::next_METHOD () {
        #my ($dispatcher, $label, $opts, $self, $args) = @{$::DISPATCHER};
        my $method = ::WALKMETH($::DISPATCHER->[0], $::DISPATCHER->[1], %{$::DISPATCHER->[2]}); 
        confess "No next-method for found" unless defined $method;
        return $method->($::DISPATCHER->[3], @{$::DISPATCHER->[4]});         
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
        '0+' => sub { $_[0]->[0] },       
        '""' => sub {
            "#<" . (exists ${$_[0]->[1]}->[2]->{'$:name'} ?
                        (${$_[0]->[1]}->[2]->{'$:name'} || 'AnonClass')
                        : 
                        'AnonClass') . "=(" . $_[0]->[0] . ")>"
        }, 
        fallback => 1;

    sub isa { our $AUTOLOAD = 'isa'; goto &AUTOLOAD; }
    sub can { our $AUTOLOAD = 'can'; goto &AUTOLOAD; }

    sub AUTOLOAD {
        my @autoload = (split '::', our $AUTOLOAD);
        my $label = $autoload[-1];
        #my $self = shift;   
        # NOTE:
        # DESTROYALL is what should really be called
        # so we just deal with it like this, and we deal
        # with it here since this is a p5 issue.
        if ($label =~ /DESTROY/) {
            # this is to avoid GC errors during global destruction
            # I am not 100% sure it will work 100% of the time
            return if $::IN_GLOBAL_DESTRUCTION || !defined($::Class);            
            $label = 'DESTROYALL';
        }        
        # go about our dispatching ....      
        return ::dispatcher(shift, $label, \@_, ($autoload[0] eq 'class' ? 1 : 0));
    }
}

## add on to Class::Multimethods::Pure 
## to make method composition easier

{
    use Class::Multimethods::Pure ();
    
    sub ::multi_sub {
        my $wrapper;
        my $registry = {
            # maybe we should start our own multi registry to handle scoping
            multi => \%Class::Multimethods::Pure::MULTI,
            multiparam => \%Class::Multimethods::Pure::MULTIPARAM,
            install_wrapper => sub { 
                my ($name, $registry) = @_;
                $wrapper = $registry->{multi}{$name};
            },
        };

        my $name = shift;
        Class::Multimethods::Pure::process_multi($registry,
            $name, -core => 'DumbCache', @_);

        return $wrapper;
    }    
}

# NOTE:
# this *might* work correctly, it should
# collect all the classes, then call DESTROY
# on all them, then set a flag to tell the
# dispatcher to ignore any futher DESTROY calls
$::IN_GLOBAL_DESTRUCTION = 0;
END {
    $::IN_GLOBAL_DESTRUCTION = 1;
    if (defined $::Class && defined $::Object) {
        my @classes;
        my %seen;
        my $traversal;
        $traversal = sub {
            foreach my $subclass (@{$_[0]->subclasses}) {
                unless (exists $seen{$subclass}) {
                    $seen{$subclass} = undef;
                    push @classes => $subclass;
                    $traversal->($subclass);
                }
            }
        };
        $traversal->($::Object);
        # now destroy them... destroy then ALL .... muhahahahaha
        $_->DESTROYALL() foreach ((reverse @classes), $::Object);
        $::IN_GLOBAL_DESTRUCTION = 1;
    }
}

1;

__END__

START:
Files=38, Tests=743, 74 wallclock secs (56.29 cusr +  2.16 csys = 58.45 CPU)

CURRENT:
Files=38, Tests=743, 28 wallclock secs (20.04 cusr +  1.73 csys = 21.77 CPU)

=pod

=head1 NAME

chaos

=head1 DESCRIPTION

=head1 AUTHORS

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
