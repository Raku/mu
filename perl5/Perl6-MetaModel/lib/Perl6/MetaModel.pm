
package Perl6::MetaModel;

use strict;
use warnings;

use Scalar::Util 'blessed';
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

## this just makes sure to clear the invocant when
## something dies, it is not pretty, but it works
$SIG{'__DIE__'} = sub { 
    @Perl6::Method::CURRENT_INVOCANT_STACK = (); 
    @Perl6::Method::CURRENT_CLASS_STACK    = ();     
    @Perl6::MetaModel::CURRENT_DISPATCHER     = ();
    CORE::die @_; 
};

## GLOBAL META FUNCTIONS

sub ::dispatch {
    my ($self) = @_;    
    # deal with the MetaClass special case ...
    if ((blessed($self) && blessed($self) eq 'Perl6::MetaClass')) {
         goto &::metaclass_dispatch;
    }
    else {
        goto &::normal_dispatch;
    }
}

# this dispatch routine avoids
# all method calls since that 
# could cause issues.
sub ::metaclass_dispatch {
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

    my $meta_meta = $self->meta();       
    # XXX - we are checking the non local table for 
    # private methods, which is not okay, that part
    # needs to be fixed. (but at least we are checking
    # everything here ... )
    foreach my $meta ($meta_meta, @{$meta_meta->{instance_data}->{'@:superclasses'}}) {
        my $method_table = $meta->{instance_data}->{$method_table_name}->{methods};
        return $method_table->{$label}->do($self, @_) if (exists $method_table->{$label});             
    }
    confess "Method ($label) not found for instance ($self)";  
}

sub ::normal_dispatch {
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
        (::dispatch($self->meta, 'has_method', ($label, (for => 'private'))))
            || confess "Private Method ($label) not found for instance ($self)";        
        my $method = ::dispatch($self->meta, 'get_method', ($label, (for => 'private')));
        @return_value = $method->do($self, @_);             
    }
    else {      

        # get the dispatcher instance ....
        my $dispatcher = ::dispatch($self->meta, 'dispatcher', (':canonical'));
        

        # this needs to be fully qualified for now
        my $method = ::WALKMETH($dispatcher, $label, (
                blessed($self) ? 
                    (blessed($self) eq 'Perl6::Class' ?
                        (for => 'Class')
                        :
                        ()) 
                    : 
                    (for => 'Class')
            )
        );
        (blessed($method) && $method->isa('Perl6::Method'))
            || confess "Method ($label) not found for instance ($self)";        

        push @CURRENT_DISPATCHER => [ $dispatcher, $label, $self, @_ ];

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
    my $prop = ::dispatch($class->meta, 'find_attribute_spec', ($label, for => 'Class'))
        || confess "Cannot locate class property ($label) in class ($class)";    
    $prop->set_value($value) if defined $value;    
    $prop->get_value();    
}

sub _ {
    my ($label, $value) = @_;
    my $self = ::SELF();
    if (defined $value) {
        my $prop = ::dispatch($self->meta, 'find_attribute_spec', ($label))
            || confess "Perl6::Attribute ($label) not found";
        # since we are not private, then check the type
        # assuming there is one to check ....
        if (my $type = $prop->type()) {
            if ($prop->is_array()) {
                (blessed($_) && ($_->isa($type) || $_->does($type))) 
                    || confess "IncorrectObjectType: expected($type) and got($_)"
                        foreach @$value;                        
            }
            else {
                (blessed($value) && ($value->isa($type) || $value->does($type))) 
                    || confess "IncorrectObjectType: expected($type) and got($value)";            
            }
        }  
        else {
            (ref($value) eq 'ARRAY') 
                || confess "You can only asssign an ARRAY ref to the label ($label)"
                    if $prop->is_array();
            (ref($value) eq 'HASH') 
                || confess "You can only asssign a HASH ref to the label ($label)"
                    if $prop->is_hash();
        }                      
        # We are doing a 'binding' here by linking the $value into the $label
        # instead of storing into the container object available at $label
        # with ->store().  By that time the typechecking above will go away
        $self->{instance_data}->{$label} = $value;         
    }
    # now return it ...
    $self->{instance_data}->{$label};
}

sub role {
    my ($name, $role) = @_;
    Perl6::Role->add_role($name, $role);
}

sub class {
    my ($name, $params) = @_;
    my $class = Perl6::Class::Util::_create_new_class('Perl6::Class', $name, $params);
    Perl6::Class::Util::_apply_class_to_environment($class);
    return $class;
}

## GLOBAL FUNCTIONS

sub ::next_METHOD {
    my ($dispatcher, $label, $self, @args) = @{$CURRENT_DISPATCHER[-1]};             
    my $method = ::WALKMETH($dispatcher, $label); 
    return $method->do($self, @args);    
}

sub ::WALKMETH {
    my ($dispatcher, $label, %opts) = @_;
    while (my $current = $dispatcher->next()) {
        if (::dispatch($current, 'has_method', ($label, %opts))) {
            return ::dispatch($current, 'get_method', ($label, %opts)) 
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
    my $startclass = ::dispatch($obj->meta, 'dispatcher');
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
    my $startclass = ::dispatch($obj->meta, 'dispatcher');
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
            return ::dispatch($class->meta, 'get_method', ($methname)->do($obj, @{$args}));
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
    require Perl6::Object;
    # now we can say that a MetaClass
    # is a Object ...
    ::dispatch(Perl6::MetaClass->meta, 'superclasses', [ Perl6::Object->meta ]);
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


