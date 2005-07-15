
package Perl6::Object;

use strict;
use warnings;

use Perl6::MetaClass;

use Scalar::Util 'blessed';
use Carp 'confess';

## ----------------------------------------------------------------------------
## this is to handle the $?SELF and $?CLASS variables 
## which are current is used in AUTOLOAD, BUILDALL and DESTORYALL 

our @CURRENT_INVOCANT_STACK;
our @CURRENT_CLASS_STACK;

## this just makes sure to clear the invocant when
## something dies, it is not pretty, but it works
$SIG{'__DIE__'} = sub { 
    @CURRENT_INVOCANT_STACK = (); 
    @CURRENT_CLASS_STACK = ();     
    CORE::die @_; 
};

## ----------------------------------------------------------------------------

# the default .new()

sub new {
    my ($class, %params) = @_;
    return $class->bless(undef, %params);
}

# but this is what really constructs the class

# XXX - this might move up the MetaClass at some point - per $Larry
sub bless : method {
    my ($class, $canidate, %params) = @_;
    $canidate ||= 'P6opaque'; # opaque is our default
    my $instance_structure = $class->CREATE(repr => $canidate, %params);
    # XXX - We do this because we are in Perl5, this 
    # should not be how the real metamodel behave 
    # at least I dont think it is how it should :)
    my $self = CORE::bless($instance_structure, $class);
    $self->BUILDALL(%params);
    return $self;
}

## Submethods (hacked here for now)

sub CREATE {
    my ($class, %params) = @_;
    ($params{repr} eq 'P6opaque') 
        || confess "Sorry, No other types other than 'P6opaque' are currently supported";    
    
    # this just gathers all the 
    # attributes that were defined
    # for the instances.
    my %attrs;
    $class->meta->traverse_post_order(sub {
        my $c = shift;
        foreach my $attr ($c->get_attribute_list) {
            my $attr_obj = $c->get_attribute($attr);
            $attrs{$attr} = $attr_obj->instantiate_container;
            
        }
    }); 
    
    # this is our P6opaque data structure
    # it's nothing special, but it works :)
    return {
        class         => $class->meta,
        instance_data => \%attrs,
    };         
}

sub BUILDALL {
    my ($self, %params) = @_;
    # XXX - hack here to call Perl6::Object::BUILD
    $self->Perl6::Object::BUILD(%params);
    # then we post order traverse the rest of the class
    # hierarchy. This will all be fixed when Perl6::Object
    # is properly bootstrapped
    $self->meta->traverse_post_order(sub {
        my $c = shift;
        return unless $c->has_method('BUILD');
        my $method = $c->get_method('BUILD');
        # NOTE: this is to mimic $?SELF and $?CLASS
        push @CURRENT_INVOCANT_STACK => $self; 
        push @CURRENT_CLASS_STACK => $method->associated_with;          
        $method->call($self, %params);   
        pop @CURRENT_INVOCANT_STACK;
        pop @CURRENT_CLASS_STACK;        
    });    
}

sub BUILD {
    my ($self, %params) = @_;
    $self->set_value($_ => $params{$_}) foreach keys %params;
}

sub DESTROYALL {
    my ($self) = @_;
    $self->meta->traverse_pre_order(sub {
        my $c = shift;
        return unless $c->has_method('DESTROY');
        my $method = $c->get_method('DESTROY');
        # NOTE: this is to mimic $?SELF and $?CLASS
        push @CURRENT_INVOCANT_STACK => $self; 
        push @CURRENT_CLASS_STACK => $method->associated_with;          
        $method->call($self);   
        pop @CURRENT_INVOCANT_STACK;
        pop @CURRENT_CLASS_STACK;             
    });      
}

## end Submethods

## XXX - all the methods below are called automagicaly by 
## Perl5, so we need to handle them here in order to control
## the metamodels functionality

sub isa {
    my ($self, $class) = @_;
    return undef unless $class;
    return $self->meta->is_a($class);
}

sub can {
    my ($self, $label) = @_;
    return undef unless $label;
    if (blessed($self)) {
        return $self->meta->responds_to($label);
    }
    else {
        return $self->meta->responds_to($label, for => 'Class');
    }
}

{
    # XXX - this is a hack to make SUPER:: work
    # otherwise the default SUPER:: needs to be 
    # used, and that is not what I want to happen
    package SUPER;
    sub AUTOLOAD {
        $Perl6::Object::AUTOLOAD = our $AUTOLOAD;
        goto &Perl6::Object::AUTOLOAD;
    }
}

sub AUTOLOAD {
    my @AUTOLOAD = split '::', our $AUTOLOAD;
    my $label = $AUTOLOAD[-1];
    # NOTE:
    # DESTROY is never called like this, it always
    # goes through the DESTORYALL submethod (see below)
    return if $label =~ /DESTROY/;
    my $self = shift;
    my @return_value;
    if (blessed($self)) {       
        
        # get the dispatcher instance ....
        my $dispatcher = $self->meta->dispatcher;
        
        # just discard it if we are calling SUPER
        $dispatcher->() if ($AUTOLOAD[0] eq 'SUPER');
        
        my $current;
        while ($current = $dispatcher->()) {
            last if $current->has_method($label);
        }
        (blessed($current) && $current->isa('Perl6::MetaClass'))
            || confess "Method ($label) not found for instance ($self)";        
        
        my $method = $current->get_method($label);

        # NOTE: this is to mimic $?SELF and $?CLASS
        push @CURRENT_INVOCANT_STACK => $self; 
        push @CURRENT_CLASS_STACK => $method->associated_with;            
                             
        @return_value = $method->call($self, @_);        
    }
    else {  
        # get the dispatcher instance ....
        my $dispatcher = $self->meta->dispatcher;

        # just discard it if we are calling SUPER
        $dispatcher->() if ($AUTOLOAD[0] eq 'SUPER');

        my $current;
        while ($current = $dispatcher->()) {
            last if $current->has_method($label, for => 'Class');
        }
        (blessed($current) && $current->isa('Perl6::MetaClass'))
            || confess "Method ($label) not found for instance ($self)";        

        my $method = $current->get_method($label, for => 'Class');            

        # NOTE: this is to mimic $?CLASS            
        push @CURRENT_CLASS_STACK => $method->associated_with;                 
             
        @return_value = $method->call($self, @_);
    }
    # we can dispose of this value, as it 
    # should never be called outside of 
    # a method invocation
    pop @CURRENT_INVOCANT_STACK;
    pop @CURRENT_CLASS_STACK;
    return wantarray ?
                @return_value
                :
                $return_value[0];
}

# this just dispatches to the DESTROYALL
# which deals with things correctly
sub DESTROY {
    my ($self) = @_;
    $self->DESTROYALL();
}

## Perl6 metamodel methods and misc. support 
## methods for our Perl5 version

sub get_class_value {
    my ($self, $label) = @_;
    my $prop = $self->meta->find_attribute_spec($label, for => 'Class')
        || confess "Cannot locate class property ($label) in class ($self)";        
    $prop->get_value();
}

sub set_class_value {
    my ($self, $label, $value) = @_;
    my $prop = $self->meta->find_attribute_spec($label, for => 'Class')
        || confess "Cannot locate class property ($label) in class ($self)";
    $prop->set_value($value);
}

sub get_value {
    my ($self, $label) = @_;
    ${$self->{instance_data}->{$label}};
}

sub set_value {
    my ($self, $label, $value) = @_;
    my $prop = $self->meta->find_attribute_spec($label)
        || confess "Perl6::Attribute ($label) no found";

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
    ${$self->{instance_data}->{$label}} = $value;        
}

# Initialize the Perl6::Object's metaclass here ...
our $META = Perl6::MetaClass->new(name => 'Perl6::Object');

# metaclass access for all our objects ...
sub meta {
    my ($class) = @_;
    $class = blessed($class) if blessed($class);       
    no strict 'refs';
    return ${$class .'::META'};
}

1;

__END__

=pod

=head1 NAME

Perl6::Object

=head1 DESCRIPTION

This is the base 'Object' class. It will eventually be self-hosting, but for now
it contains a number of hacks to support the expected behavior of the Perl 6 object
model.

=head1 AUTHOR

Stevan Little E<lt>stevan@iinteractive.comE<gt>

Autrijus Tang E<lt>autrijus@autrijus.orgE<gt>

=cut
