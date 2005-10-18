#!/usr/bin/perl

use strict;
use warnings;
use Perl6::MetaModel::Metamorph;

# This is pneuma, this creates ::Object, ::Package and ::Module
# (also the ::EigenClass too)

# From Answers.com:
# According to the Gnostics, the Demiurge was able to endow man 
# only with psyche (sensuous soul) — the pneuma (rational soul) 
# having been added by God.

## (see psyche.pl for more information)

$::Object     = undef;
$::Package    = undef;
$::Module     = undef;
$::EigenClass = undef;

## ----------------------------------------------------------------------------
## Eigenclass

$::EigenClass = $::Class->new();
# do this (and some other stuff later), in the bootstrap  
# $::EigenClass->superclasses([ $::Class ]);

## ----------------------------------------------------------------------------
## Object

# The 'Object' class
$::Object = $::Class->new();

## submethods

$::Object->add_method('BUILD' => ::make_submethod(sub { 
    my ($self, %params) = @_;
    foreach my $key (keys %params) {
        # XXX -
        # The default BUILD method should accept
        # params which are not included in the 
        # attributes. It will do nothing with them
        # but it will allow them to exist.
        # - (see t_oo/submethods.t)
        ::opaque_instance_attr($self => $key) = $params{$key}
            # NOTE:
            # this is an ugly way to do this, ideally
            # we would peek into the instance structure
            # itself and see if we had the spot, and
            # otherwise ignore it ... but this will do
            if $self->class->find_attribute_spec($key);
    }
}));

## instance methods

$::Object->add_method('BUILDALL' => ::make_method(sub { 
    my ($self, %params) = @_;
    my $dispatcher = $self->class->dispatcher(':descendant');
    while (my $method = ::WALKMETH($dispatcher, 'BUILD')) {                      
        $method->($Perl6::Submethod::FORCE, $self, %params);                  
    }      
}));

$::Object->add_method('DESTROYALL' => ::make_method(sub { 
    my ($self) = @_;
    my $dispatcher = $self->class->dispatcher(':ascendant');
    while (my $method = ::WALKMETH($dispatcher, 'DESTROY')) {  
        $method->($Perl6::Submethod::FORCE, $self);   
    }  
}));

$::Object->add_method('id'    => ::make_method(sub { ::opaque_instance_id($::SELF)    }));
$::Object->add_method('class' => ::make_method(sub { ::opaque_instance_class($::SELF) }));

$::Object->add_method('isa' => ::make_method(sub { 
    my ($self, $class_name) = @_;
    return undef unless $class_name;
    return $self->class->isa($class_name);    
}));

$::Object->add_method('can' => ::make_method(sub { 
    my ($self, $label) = @_;   
    return undef unless $label;
    return ::WALKMETH($self->class->dispatcher(':canonical'), $label);    
}));

$::Object->add_method('add_singleton_method' => ::make_method(sub { 
    my ($self, $label, $method) = @_;  
    if ($self->class->class != $::EigenClass ){
#        warn "hello from not yet eigened : " . $self;
        my $eigenclass = $::EigenClass->new('$:name' => 'EigenClass[' . 
                            ($self->can('name') ? $self->name : $self->class->name) . 
                        ']');                   
        $eigenclass->superclasses([ $self->class ]);        
        ::opaque_instance_change_class($self, $eigenclass);
        ::bind_method_to_class($method, $self);
        $eigenclass->add_method($label, $method);        
    }
    else {
#        warn "hello from already eigened : $self";
        ::bind_method_to_class($method, $self);        
        $self->class->add_method($label, $method);
    }
}));

$::Object->add_method('dump' => ::make_method(sub { 
    my $self = shift;
    require Data::Dumper;
    $Data::Dumper::Maxdepth = shift if @_;
    Data::Dumper::Dumper $self;
}));

## ----------------------------------------------------------------------------
## Package

## See http://article.gmane.org/gmane.comp.lang.perl.perl6.language/4956 for
## disucssion on how Packages work

$::Package = $::Class->new();

$::Package->superclasses([ $::Object ]);

$::Package->add_attribute('$:name'      => ::make_attribute('$:name'));
$::Package->add_attribute('%:namespace' => ::make_attribute('%:namespace'));

$::Package->add_method('name' => ::make_method(sub {
    my $self = shift;
    ::opaque_instance_attr($self => '$:name') = shift if @_;        
    ::opaque_instance_attr($self => '$:name');
}));

$::Package->add_method('FETCH' => ::make_method(sub {
    my ($self, $label) = @_;
    (defined $label && $label)
        || confess "Cannot FETCH at (" . ($label || 'undef') . ")";
    if ($label eq '{}') {
        return ::opaque_instance_attr($self => '%:namespace');
    }
    ::opaque_instance_attr($self => '%:namespace')->{$label};
}));

$::Package->add_method('STORE' => ::make_method(sub {
    my ($self, $label, $value) = @_;
    (defined $label && $label)
        || confess "Cannot STORE at (" . ($label || 'undef') . ")";   
    # NOTE: special case
    # if we are storing CODE in the package, we 
    # need to wrap it so that $?PACKAGE is bound
    # correctly within it
    if ($label =~ /^\&/ && ref($value) eq 'CODE') {
        $value = ::wrap_package_sub($value, $self);
    } 
    ::opaque_instance_attr($self => '%:namespace')->{$label} = $value;
}));

=pod

Thoughts on ::Package's API:

- LOCATE

FETCH and STORE work on very basic levels, but I think a LOCATE method would 
be helpful as well. It would take a fully qualified name, and attempt to 
locate the item. 

The other option is to ask FETCH and STORE to handle this, however, this would
put a burdon on subclassers possibly.

- CREATE

CREATE would take a fully qualified name, and create any missing packages 
nessecary, possibly storing a value in the last location.

  # create ::Foo and ::Bar as needed from the current level
  $pkg->CREATE('Foo::Bar::&baz' => sub { ... }); 
  
  # create ::Foo and ::Bar as needed from the top level
  $pkg->CREATE('*Foo::Bar::&baz' => sub { ... });   

- parentage

Should a child package know it's parent package? How would this look in Perl 6?
Would it be a package level version of OUTER::?

Either way, I think it might be useful to have this if for nothing other than 
a helper to other functions.  

=cut

## ----------------------------------------------------------------------------
## Module

$::Module = $::Class->new();

$::Module->superclasses([ $::Package ]);

$::Module->add_attribute('$:version'   => ::make_attribute('$:version'));
$::Module->add_attribute('$:authority' => ::make_attribute('$:authority'));

$::Module->add_method('version' => ::make_method(sub {
    my ($self, $version) = @_;
    if (defined $version) {
        ($version =~ /^\d+\.\d+\.\d+$/)
            || confess "The version ($version) is not in the correct format '0.0.0'";
        ::opaque_instance_attr($self => '$:version') = $version;
    }
    ::opaque_instance_attr($self => '$:version');    
}));

$::Module->add_method('authority' => ::make_method(sub {
    my $self = shift;
    ::opaque_instance_attr($self => '$:authority') = shift if @_;        
    ::opaque_instance_attr($self => '$:authority');
}));

$::Module->add_method('identifier' => ::make_method(sub {
    return join '-' => ($::SELF->name, $::SELF->version, ($::SELF->authority || ()));
}));

1;

__END__

=pod

=head1 NAME

pneuma

=head1 DESCRIPTION

=head1 AUTHORS

Stevan Little E<lt>stevan@iinteractive.comE<gt>

=cut
