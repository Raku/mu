#!/usr/bin/perl

use strict;
use warnings;

use Carp 'confess';
#use Hash::Util 'lock_keys', 'unlock_keys';
use Scalar::Util 'blessed';
#use Data::Dumper ();

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
        bless [
            ++$GLOBAL_INSTANCE_COUNT, # 'id'    => 
            $class,                   # 'class' => 
            \%attrs,                  # 'attrs' => 
        ], 'Dispatchable';
    }

    # Accessors for the inside of the opaque structure
    sub ::opaque_instance_id { 
        $_[0]->[0];
    }
    sub ::opaque_instance_class {  
        ${$_[0]->[1]};
    }
    sub ::opaque_instance_attr : lvalue { 
        $_[0]->[2]->{$_[1]};
    }
    
    sub ::opaque_instance_add_new_attribute {
        $_[0]->[2]->{$_[1]} = $_[2];            
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
        my $dispatcher = shift;
        { ($dispatcher->() || return)->get_method(@_) || redo }       
    }
    
    sub ::WALKCLASS {      
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
    
    # this just initializes the instance attribute container type, in
    # perl 6 this should be done automatically
    sub ::instantiate_attribute_container ($) {       
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
        return _class_dispatch(@_)->($_[0], @{$_[2]}) if $_[0] == $::Class;
        return _normal_dispatch(@_);
    }

    # this will handle all dispatching 
    # for the root $::Class
    my %_class_dispatch_cache;
    sub _class_dispatch { 
        # memoize the method lookup ...
        return $_class_dispatch_cache{$_[1]} 
            if exists $_class_dispatch_cache{$_[1]};
            
        my $method_table_name = '%:methods';
        # check the private methods
        $method_table_name = '%:private_methods' if $_[1] =~ /^_/o;

        # gather all the classes to look through
        my @classes = ($_[0]);
        push @classes => ((scalar @{$_[0]->[2]->{'@:MRO'}}) ? @{$_[0]->[2]->{'@:MRO'}} : @{$_[0]->[2]->{'@:superclasses'}})
            # however, we dont actually need to go there
            # if what we are asking for is a private method
            if $method_table_name ne '%:private_methods';

        # now try and find out method ...
        foreach my $class (@classes) {
            my $method_table = $class->[2]->{$method_table_name};
            return ($_class_dispatch_cache{$_[1]} = $method_table->{$_[1]}) 
                if exists $method_table->{$_[1]};             
        }
        confess "Method ($_[1]) not found in \$::Class";          
    }

    # we use this to store the 
    # memoized method lookup :)
    my %_normal_dispatch_cache;
    sub _normal_dispatch {                    
        my $class = ${$_[0]->[1]};
        # XXX - 
        # this seems to prevent some GC issues,.. 
        # so I am leaving it here for now
        #warn "got an undef class here ... ($self)" 
        return unless defined $class;  
            
        my %opts;
        do { %opts = (for => 'class'); $class = $_[0] } if $_[3];
                
        local $::ARGS = [ $_[1], \%opts, $_[0], $_[2] ];          

        return $_normal_dispatch_cache{$class}->{ $_[1] }->[0]->($_[0], @{$_[2]}) 
            if exists $_normal_dispatch_cache{$class}->{ $_[1] };        
        
        # check if this is a private method
        if ($_[1] =~ /^_/) {           
            my $method = $::CLASS->get_method($_[1], for => 'private') 
                      || confess "Private Method ($_[1]) not found for current class ($::CLASS)";
            $_normal_dispatch_cache{$::CLASS} = { $_[1] => [ $method, undef ] };
            return $method->($_[0], @{$_[2]});  
        }
        else {   
             
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

            $_normal_dispatch_cache{$class} = { $_[1] => [ $method, $dispatcher->(1) ] };
            return $method->($_[0], @{$_[2]});     
        }        
    }

    ## next METHOD;
    # this mimics the 'next METHOD' 
    # construct to go to the next
    # applicable method it is localized
    # in each run of _normal_dispatch
    # so that 
    sub ::next_METHOD () {
        my $dispatcher;
        if ($::DISPATCHER) {
           $dispatcher = $::DISPATCHER; 
        }
        else {
                                                          # class-id               # label         # dispacther-depth
            my $dispatcher_depth = $_normal_dispatch_cache{${$::ARGS->[2]->[1]}}->{$::ARGS->[0]}->[1];
                            # class
            $dispatcher = ${$::ARGS->[2]->[1]}->dispatcher(':canonical');
            $dispatcher->() while $dispatcher_depth--;
        }
        local $::DISPATCHER = $dispatcher;
        my $method = ::WALKMETH($dispatcher, $::ARGS->[0], %{$::ARGS->[1]}); 
        confess "No next-method for $::ARGS->[0] found" unless defined $method;
        return $method->($::ARGS->[2], @{$::ARGS->[3]});         
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
        my $registry; 
        $registry = {
            # maybe we should start our own multi registry to handle scoping
            multi => \%Class::Multimethods::Pure::MULTI,
            multiparam => \%Class::Multimethods::Pure::MULTIPARAM,
            install_wrapper => sub { 
                my (undef, $_name) = @_;
                $wrapper = $registry->{multi}->{$_name};
            },
        };

        my $name = shift;
        Class::Multimethods::Pure::process_multi($registry,
            $name, -core => 'DumbCache', @_);

        return sub {
            my $call = $wrapper->can('call');
            unshift @_, $wrapper;
            goto &$call;
        };
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

=pod

=head1 NAME

chaos

=head1 DESCRIPTION

=head1 AUTHORS

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
