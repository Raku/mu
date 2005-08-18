
package Perl6::MetaModel;

use strict;
use warnings;

use Scalar::Util 'blessed';
use Hash::Util 'lock_keys';
use Carp 'confess';

use Perl6::Role;
use Perl6::Class;

sub import {
    shift;
    return if @_; # return if anything is passed    
    no strict 'refs';
    
    my $caller_pkg = caller();
    
    # meta model helpers
    *{$caller_pkg . '::class'} = \&class;
    *{$caller_pkg . '::role'}  = \&role;
    
    # instance attribute access helpers
    *{$caller_pkg . '::_'} = \&_; 
    # class attribute access helpers
    *{$caller_pkg . '::__'} = \&__; 
}

our @CURRENT_DISPATCHER = ();
our %AUTOLOAD = ();
our $OBJ_ID = 0;

## this just makes sure to clear the invocant when
## something dies, it is not pretty, but it works
$SIG{'__DIE__'} = sub { 
    @Perl6::Method::CURRENT_INVOCANT_STACK = (); 
    @Perl6::Method::CURRENT_CLASS_STACK    = ();     
    @Perl6::MetaModel::CURRENT_DISPATCHER  = ();
    %Perl6::MetaModel::AUTOLOAD            = ();    
    CORE::die @_; 
};

# a little helper function 
sub _class_name {
    my $class = shift;
    blessed($class) ?  
        # accomidate the Perl6::Class instance
        blessed($class) eq 'Perl6::Class' ?
            ::get_P6opaque_instance_data($class)->{name}
            :
            blessed($class)
        :
        $class;    
}

## GLOBAL META FUNCTIONS

sub ::create_P6opaque {
    my ($class, %attrs) = @_;
    my $instance = bless {
        id            => $OBJ_ID++,
        class         => $class,
        instance_data => \%attrs
    }, _class_name($class);  
    # lock the keys for now, this is for p5 debugging
    # to foil the autovivification, it just makes my 
    # life easier by introducing a level of sanity into
    # the opaque structure   
    lock_keys(%{$instance});
    lock_keys(%{$instance->{instance_data}});      
    return $instance;
}

# NOTE:
# in theory the P6_opaque structure is opaque (duh), so
# this means that our code should not try to peak into it
# and so we should then use some kind of outside mechanism 
# to get at the object id and class 
sub ::get_P6opaque_instance_id    { (shift)->{id}            }
sub ::get_P6opaque_instance_class { (shift)->{class}         }
sub ::get_P6opaque_instance_data  { 
    confess "Instance is undefined" if not defined $_[0];
    (shift)->{instance_data} 
}

# XXX
# this is a temporary hack to 
# move us away from the named
# class thing
sub ::find_class {
    my ($class) = @_;
    (defined $class)
        || confess "You did not specify the class you want";
    (exists $Perl6::Class::ALL_CLASSES{$class})
        || confess "You asked for a class which does not exist ($class)";
    return $Perl6::Class::ALL_CLASSES{$class};
}

sub ::meta {
    my ($class) = @_;
    confess "::meta called without a class" unless defined $class;
    
    # if we have a Perl6::Class, we can just extract 
    # the data we need from it.
    return ::get_P6opaque_instance_data($class)->{meta}
        if blessed($class) && blessed($class) eq 'Perl6::Class';
        
    if (blessed($class) && blessed($class) ne 'Perl6::MetaClass') {
        my $class_obj = ::get_P6opaque_instance_class($class);
        if (blessed($class_obj) && blessed($class_obj) eq 'Perl6::Class') {
            #warn "we got the meta from instance -> class -> meta ($class)";
            return ::get_P6opaque_instance_data($class_obj)->{meta};
        }
    }

    # otherwise we need to determine the class
    # name, and croak if we cannot ...
    my $class_name = _class_name($class);
    confess "could not find class name" 
        unless defined $class_name;
        
    ## BOOTSTRAPPING
    # we have to check this here because we 
    # need to create the meta(MetaClass) which
    # itself is not hooked up through the 
    # class yet.
    if ($class_name eq 'Perl6::MetaClass') {
        no strict 'refs';
        return ${$class_name . '::META'};        
    }
       
    # now that we have determined the class name
    # and it was not a Perl6::MetaClass, then we
    # need to find the right Perl6::Class instance 
    # and croak if we cannot
    my $class_obj = ::find_class($class_name);
    confess "could not find ::Class instance for ($class_name)" 
        unless defined $class_obj;     
        
    # now we are assured to have the right 
    # class object, so we can extract the data
    return ::get_P6opaque_instance_data($class_obj)->{meta};
}

sub ::dispatch {
    my ($self) = @_;    
    # deal with the MetaClass special case ...
    if ((blessed($self) && blessed($self) eq 'Perl6::MetaClass')) {
         goto &_metaclass_dispatch;
    }
    else {
        goto &_normal_dispatch;
    }
}

# this dispatch routine avoids
# all method calls since that 
# could cause issues.
sub _metaclass_dispatch {
    my ($self, $label) = (shift, shift);    

    return if ($label =~ /DESTROY/);

    my $method_table_name;
    # check the private methods
    if ($label =~ /^_/) {
        $method_table_name = '%:private';
    }
    else {
        $method_table_name = '%:class_definition';
    }
    # we need to just access stuff directly here
    # so as to avoid the method call ... but this
    # is only needed in this package to avoid the
    # circularity

    my $meta_meta = ::meta($self);

    # gather all the metaclasses to look through
    my @metaclasses = ($meta_meta);
    
    # we need to just dig one level deep here since
    # we know it is a metaclass, that is okay, but
    # still it is a hack. 
    push @metaclasses => @{::get_P6opaque_instance_data($meta_meta)->{'@:superclasses'}}
        # however, we dont actually need to go there
        # if what we are asking for is a private method
        if $method_table_name ne '%:private';
    
    # now try and find out method ...
    foreach my $meta (@metaclasses) {
        my $method_table = ::get_P6opaque_instance_data($meta)->{$method_table_name}->{methods};
        return $method_table->{$label}->do($self, @_) if (exists $method_table->{$label});             
    }
    confess "Method ($label) not found for instance ($self)";  
}

sub _normal_dispatch {
    my ($self, $label) = (shift, shift);     
    # NOTE:
    # DESTROYALL is what should really be called
    # so we just deal with it like this :)
    if ($label =~ /DESTROY/) {
        $label = 'DESTROYALL';
    }

    my @return_value;

    # check if this is a private method
    if ($label =~ /^_/) {
        (::dispatch(::meta($self), 'has_method', ($label, (for => 'private'))))
            || confess "Private Method ($label) not found for instance ($self)";        
        my $method = ::dispatch(::meta($self), 'get_method', ($label, (for => 'private')));
        @return_value = $method->do($self, @_);             
    }
    else {      

        # get the dispatcher instance ....
        my $dispatcher = ::dispatch(::meta($self), 'dispatcher', (':canonical'));
        
        my %opts = (
            blessed($self) ? 
                (blessed($self) eq 'Perl6::Class' ?
                    (for => 'Class')
                    :
                    ()) 
                : 
                (for => 'Class')
        );

        # this needs to be fully qualified for now
        my $method = ::WALKMETH($dispatcher, $label, %opts);
        
        unless (defined $method) {
            # we need to get a new dispatcher, becuase the other
            # one has run out,.. 
            my $autoload_dispatcher = ::dispatch(::meta($self), 'dispatcher', (':canonical'));
            # if we find an AUTOLOAD anywhere in the chain, then
            # we can use it ...
            if ($method = ::WALKMETH($autoload_dispatcher, 'AUTOLOAD', %opts)) {
                # but dont forget to stash the label 
                $AUTOLOAD{::meta($self)} = $label;
            }
        }
        
        (blessed($method) && $method->isa('Perl6::Method'))
            || confess "Method ($label) not found for instance ($self)";        

        push @CURRENT_DISPATCHER => [ $dispatcher, $label, $self, \@_, \%opts ];
        
        @return_value = $method->do($self, @_);     

        # we can dispose of this value, as it 
        # should never be called outside of 
        # a method invocation
        pop @CURRENT_DISPATCHER;
    }
    # return our values
    return wantarray ?
                @return_value
                :
                $return_value[0];     
}

## these get exported to the caller's namespace ...

sub __ {
    my ($label, $value) = @_;
    my $class = ::CLASS();
    my $prop = ::dispatch(::meta($class), 'find_attribute_spec', ($label, for => 'Class'))
        || confess "Cannot locate class property ($label) in class ($class)";    
    $prop->set_value($value) if defined $value;    
    $prop->get_value();    
}

sub _ {
    my ($label, $value) = @_;
    my $self = ::SELF();
    if (defined $value) {
        my $prop = ::dispatch(::meta($self), 'find_attribute_spec', ($label))
            || confess "Perl6::Attribute ($label) not found";
        # do some basic container type checking here
        (ref($value) eq 'ARRAY') 
            || confess "You can only asssign an ARRAY ref to the label ($label)"
                if $prop->is_array();
        (ref($value) eq 'HASH') 
            || confess "You can only asssign a HASH ref to the label ($label)"
                if $prop->is_hash();
        ::get_P6opaque_instance_data($self)->{$label} = $value;         
    }
    # now return it ...
    ::get_P6opaque_instance_data($self)->{$label};
}

sub role {
    my ($name, $role) = @_;
    Perl6::Role->add_role($name, $role);
}

sub class {
    my ($name, $params) = @_;
    my $class = Perl6::Class->new_class($name, $params);
    $class->_apply_class_to_environment();
    return $class;
}

## GLOBAL FUNCTIONS

sub ::next_METHOD {
    my ($dispatcher, $label, $self, $args, $opts) = @{$CURRENT_DISPATCHER[-1]};             
    my $method = ::WALKMETH($dispatcher, $label, %{$opts}); 
    confess "No next-method for '$label' found" unless defined $method;
    return $method->do($self, @{$args});    
}

sub ::AUTOLOAD {
    my $self = shift;
    (exists $AUTOLOAD{::meta($self)})
        || confess "You cannot call \$AUTOLOAD from outside of a MetaModel defined method";
    $AUTOLOAD{::meta($self)};   
}

sub ::WALKMETH {
    my ($dispatcher, $label, %opts) = @_;
    while (my $current = $dispatcher->next()) {
        if (::dispatch($current, 'has_method', ($label, %opts))) {
            return ::dispatch($current, 'get_method', ($label, %opts));
        }
    }
    return undef;
}

sub ::WALKCLASS {
    my ($dispatcher, %opts) = @_;
    return $dispatcher->next();
}

sub ::SELF {
    (@Perl6::Method::CURRENT_INVOCANT_STACK)
        || confess "You cannot call \$?SELF from outside of a MetaModel defined Instance method";
    $Perl6::Method::CURRENT_INVOCANT_STACK[-1];     
}

sub ::CLASS {
    (@Perl6::Method::CURRENT_CLASS_STACK)
        || confess "You cannot call \$?CLASS from outside of a MetaModel defined method";
    $Perl6::Method::CURRENT_CLASS_STACK[-1];     
}

sub ::CALLONE {
    my ($obj, $methname, $maybe, $opt, $args) = @_;
    $opt  ||= {};
    $args ||= [];
    my $startclass = ::dispatch(::meta($obj), 'dispatcher');
    push @CURRENT_DISPATCHER => [ $startclass, $methname, $obj, @{$args} ];
    while (my $method = ::WALKMETH($startclass, $methname, %{$opt})) {
        return $method->do($obj, @{$args});
    }
    confess "Can't locate method '$methname' via class '$startclass'" unless $maybe;
    return undef;
}


sub ::CALLALL {
    my ($obj, $methname, $maybe, $force, $opt, $args) = @_;
    $opt  ||= {};
    $args ||= [];
    my $startclass = ::dispatch(::meta($obj), 'dispatcher');
    push @CURRENT_DISPATCHER => [ $startclass, $methname, $obj, @{$args} ];    
    my @results;
    if ($force) {
        # NOTE:
        # I am not sure the usefulness of :force, unless
        # it is to override any 'last METHOD' or other 
        # calls, because it forces itself into the class
        # dispatch table, which can only lead to bad thing
        # if the method is not there ... 
        while (my $class = ::WALKCLASS($startclass, $methname, %{$opt})) {
            # redispatch (we don't have symbol tables, so we need to do meta-stuff)
            return ::dispatch(::meta($class), 'get_method', ($methname)->do($obj, @{$args}));
        }            
    }
    else {
        while (my $method = ::WALKMETH($startclass, $methname, %{$opt})) {
            push @results => [ $method->do($obj, @{$args}) ];
        }        
    }
    return @results if @results;
    return undef    if $maybe;
    confess "Can't locate method '$methname' via class '$startclass'";
}

## BOOTSTRAPPING
{
    # create the Perl6::Object
    # which in turn will also 
    # create the Perl6::MetaClass 
    # metaobject, thus bootstapping
    # itself.
    use Perl6::Object;
    
    # now we can say that a MetaClass
    # is a Object ...
    ::dispatch(::meta('Perl6::MetaClass'), 'superclasses', [ ::meta('Perl6::Object') ]);
}

1;

__END__

=pod

=head1 NAME

Perl6::MetaModel - Perl 5 Prototype of the Perl 6 Metaclass model

=head1 SYNOPSIS

    use Perl6::MetaModel;

    role MyRole => {
        methods => {
            test => sub { print 'MyRole::test' }
        }
    };

    class 'MyClass-0.0.1-cpan:JRANDOM' => {
        is => [ 'MyBaseClass' ],
        does => [ 'MyRole' ],
        class => {
            methods => {
                a_class_method => sub { ... }
            }
        },
        instance => {
            attrs => [ '$.foo' ],
            BUILD => sub {
                my ($self) = @_;
                $self->set_value('$.foo' => 'Foo::Bar');
            },
            methods => {
                tester => sub {
                    my ($self) = shift;
                    $self->test(); # call the role method
                    print $self->get_value('$.foo');
                }
            }
        }
    };

    my $c = MyClass->new();
    # or 
    my $c = 'MyClass-0.0.1-cpan:JRANDOM'->new();
    
    $c->foo('Testing 1 2 3');

    $c->tester();

=head1 DESCRIPTION

This set of modules is a prototype for the Perl 6 Metaclass model, which is the
model which describes the interactions of classes, objects and roles in the Perl
6 object space. 

I am prototyping this in Perl 5 as part of the Perl 6 -> PIL compiler to run on
a Perl 5 VM.  It is currently in the early stages of a refactoring from the
original which was just a hacked together prototype. 

=head1 EXPORTED FUNCTIONS

These functions are exported and are thin wrappers around the Perl6::Class and Perl6::Role
modules to make class/role construction easier.

=over 4

=item B<class>

=item B<role>

=back

=head1 SEE ALSO

=over 4

=item All the Perl 6 documentation. 

In particular Apocolypse and Synopsis 12 which describe the object system.

=item L<Class::Role>, L<Class::Roles> & L<Class::Trait>

The first two are early attempts to prototype role behavior, and the last is an implementation
of the Trait system based on the paper which originally inspired Roles.

=item Any good Smalltalk book.

I prefer the Brown book by Adele Goldberg and David Robinson, but any one which talks about the
smalltalk metaclasses is a good reference.

=item CLOS

The Common Lisp Object System has a very nice meta-model, and plently of reference on it. In 
particular there is a small implementation of CLOS called TinyCLOS which is very readable (if 
you know enough Scheme that is)

=back

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut


