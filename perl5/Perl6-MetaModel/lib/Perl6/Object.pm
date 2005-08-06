
use strict;
use warnings;

use Scalar::Util 'blessed';
use Carp 'confess';

use Perl6::MetaModel;

my $isa = sub {    
    my ($self, $class) = @_;
    return undef unless $class;
    return ::dispatch(::meta($self), 'is_a', ($class));
};

my $can = sub {
    my ($self, $label) = @_;
    return undef unless $label;
    return ::WALKMETH(::dispatch(::meta($self), 'dispatcher', (':canonical')), $label, (
            blessed($self) ? 
                (blessed($self) eq 'Perl6::Class' ?
                    (for => 'Class')
                    :
                    ()) 
                : 
                (for => 'Class')
        )
    );
};  

class 'Perl6::Object' => {
    'class' => {
        methods => {
            # the default .new()
            'new' => sub {
                my ($class, %params) = @_;
                return ::dispatch($class, 'bless', (undef, %params));
            },
            # but this is what really constructs the class
            # XXX - this might move up the MetaClass at some point - per $Larry 
            'bless' => sub {
                my ($class, $canidate, %params) = @_;
                $canidate ||= 'P6opaque'; # opaque is our default
                my $self = ::dispatch($class, 'CREATE', (repr => $canidate, %params));
                ::dispatch($self, 'BUILDALL', (%params));
                return $self;
            },
            # XXX - According to $Larry, the initial CREATE, 
            # BUILDALL and DESTROYALL are not submethod, but
            # regular methods  
            'CREATE' => sub {
                my ($class, %params) = @_;
                ($params{repr} eq 'P6opaque') 
                    || confess "Sorry, No other types other than 'P6opaque' are currently supported";    

                # this just gathers all the 
                # attributes that were defined
                # for the instances.
                my %attrs;
                my $dispatcher = ::dispatch(::meta($class), 'dispatcher', (':descendant'));
                while (my $c = ::WALKCLASS($dispatcher)) {
                    foreach my $attr (::dispatch($c, 'get_attribute_list')) {
                        my $attr_obj = ::dispatch($c, 'get_attribute', ($attr));
                        $attrs{$attr} = $attr_obj->instantiate_container;
                    }
                }
                # this is our P6opaque data structure
                # it's nothing special, but it works :)
                my $self = ::create_P6opaque($class, %attrs);              
                # and now return it ...
                return $self;
            },
            'isa' => $isa,
            'can' => $can,         
        }
    },
    instance => {
        submethods => {
            'BUILD' => sub {
                my ($self, %params) = @_;
                _($_ => $params{$_}) foreach keys %params;
            }
        },
        methods => {
            'BUILDALL' => sub {
                my ($self, %params) = @_;
                my $dispatcher = ::dispatch(::meta($self), 'dispatcher', (':descendant'));
                while (my $method = ::WALKMETH($dispatcher, 'BUILD')) {                      
                    $method->force_call($self, %params);                  
                }              
            },
            'DESTROYALL' => sub {
                my ($self) = @_;
                my $dispatcher = ::dispatch(::meta($self), 'dispatcher', (':ascendant'));
                while (my $method = ::WALKMETH($dispatcher, 'DESTROY')) {  
                    $method->force_call($self);   
                }               
            },
            'isa' => $isa,
            'can' => $can, 
            'id' => sub { ::get_obj_id(shift) }
        }
    }
};           


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
