#!/usr/bin/perl

# ChangeLog
#
# 2005-08-22
# * Added 'slurpy' parameter
# * Compatible with Perl 5 scalar/list context
# * Removed classes Params, Signature
# * Added 'default' parameter hook
# * Added required/optional parameters
# * Param 'type' is a closure, allowing the use of 'subtype'
# * New methods: .arity, .name
# * Finished migration to Object model
#
# 2005-08-21
# * Refactored Code.pm from MetaModel t/80_Code.t
# * 'Code', 'Sub', 'MultiSub', 'Block' are now Perl6 classes

# Notes:
# Subroutine global names are not created by the Code object.
# - code.t stores names in %Perl6::[Multi]Sub::SUBS

# TODO - caller context
# TODO - test the integration with Value and Container types
# TODO - add hooks for signature checks, autoboxing/un-boxing, return value checking and autoboxing/un-boxing
# TODO - add support for optional parameters ?$x, pairs
# TODO - add *@slurpyarray params, boxed data
# TODO - add default values
# TODO - implement say { @_ } - test it with Int->new(3)->say
# TODO - move subs from code.t into the module
# TODO - Coro
# TODO - implement infinite list parameter - (1..Inf).shift
# TODO - box-up .arity and .name return values

use strict;
use warnings;

use Carp 'confess';

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Value;

my $class_description = '-0.0.1-cpan:FGLOCK';

{
    package Perl6::Param;

    sub new {
        # TODO - autobox/un-box hook
        # NOTE - default and type are closures
        my ($class, %param ) = @_;
        my ( $type, $name, $default, $required, $slurpy ) = 
           ( $param{type}, $param{name}, $param{default}, $param{required}, $param{slurpy} );
        $default = sub {} unless defined $default;

        my $optional = $name =~ s/^.?\?//;
        $required = defined $required ? $required : ! $optional;

        my $slurp = $name =~ s/^.?\*//;
        $slurpy = defined $slurpy ? $slurpy : $slurp;

        $type = 
            defined $type ? $type :
            $name =~ /^\$/ ? sub { 1 
            #    my $r = ref($_[0]);
            #    $r eq 'SCALAR' || 
            #    $r eq 'int'    ||
            #    $r eq 'num'    ||
            #    $r eq 'str'    
            } : 
            $name =~ /^\@/ ? sub { ref($_[0]) eq 'ARRAY' || ref($_[0]) eq 'Array' }  : 
            $name =~ /^\%/ ? sub { ref($_[0]) eq 'HASH'  || ref($_[0]) eq 'Hash' }   : 
            die 'Sigil required in the parameter name ('.$name.')';
        bless {
            name =>     $name,
            type =>     $type,   
            default =>  $default,    
            required => $required,  
            slurpy =>   $slurpy,                    
        } => $class;
    }
    
    sub name     { $_[0]{name} }
    sub default  { $_[0]{default}() }
    sub required { $_[0]{required} }
    sub optional { ! $_[0]{required} }
    sub slurpy   { $_[0]{slurpy} }
    sub match_type { 
        ref($_[1]) ? 
        $_[0]{type}( $_[1] ) : 
        $_[0]{type}( \$_[1] )
    }        
}

class 'Code'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.body', '$.params', '$.name' ],
        DESTROY => sub {},
        methods => {
            # TODO
            #'num' =>  sub { Num->new( '$.unboxed' => _('$.unboxed')->num  ) },
            #'int' =>  sub { Int->new( '$.unboxed' => _('$.unboxed')->int  ) },
            #'str' =>  sub { Str->new( '$.unboxed' => 'sub {...}' ) },
            #'bit' =>  sub { Bit->new( '$.unboxed' => _('$.unboxed')->bit  ) },
            'perl' => sub { Str->new( '$.unboxed' => 'sub {...}' ) },
            'ref' =>  sub { ::CLASS }, 
    
            do => sub {
                my ($self, @arguments) = @_;
                ::SELF->check_params(@arguments)
                    || confess "Signature does not match";
                my %bound_params = ::SELF->bind_params(@arguments);    
                ::SELF->body->();
            },
            arity => sub {
                scalar @{ ::SELF->params }
            }, 
            check_params => sub {
                my ($self, @params) = @_;
                for (my $i = 0; $i < @{ $self->params }; $i++) {
                    my $spec = ${ $self->params }[$i];
                    my $candidate = $params[$i];
                    next if $spec->optional && ! defined $candidate;
                    if ( $spec->slurpy ) {
                        return 0 unless $spec->match_type( [ @params[$i..$#params] ] );
                        @params = ();
                    }
                    else {
                        return 0 unless $spec->match_type($candidate);
                    }
                }
                return 1;
            },
            bind_params => sub {
                my ($self, @params) = @_;
                my %bound_params;
                for (my $i = 0; $i < @{ $self->params }; $i++) {
                    my $spec = ${ $self->params }[$i];
                    my $candidate = $params[$i];
                    $candidate = $spec->default if $spec->optional && ! defined $candidate;
                    if ( $spec->slurpy ) {
                        $bound_params{ ${$self->params}[$i]->name } = [ @params[$i..$#params] ];
                        @params = ();
                    }
                    else {
                        $bound_params{ ${$self->params}[$i]->name } = $candidate;
                    }
                }
                return %bound_params;        
            },
        },
    }
};

class 'Sub'.$class_description => {
    is => [ 'Code', 'Perl6::Object' ],
    class => {},
    instance => {
        attrs => [],  # '$.return_value' ],
        methods => {
            #do => sub {
            #    ::next_METHOD();
            #},
        },
    },
};
    
class 'MultiSub'.$class_description => {
    is => [ 'Code', 'Perl6::Object' ],
    class => {},
    instance => {
        attrs => [ '@.subs' ],     # , '$.return_value' ],
        methods => {
            do => sub {
                my ($self, @args) = @_;
                my $num_params = scalar(@args);
                my $sub;
                foreach my $_sub ( @{ _('@.subs') } ) {
                    if ($_sub->arity == $num_params) {
                        $sub = $_sub;
                        last;
                    }
                }
                $sub->do(@args);
                # _('$.return_value', $sub->return_value );
            },
        },
    },
};

class 'Block'.$class_description => {
    is => [ 'Code', 'Perl6::Object' ],
    class => {},
    instance => {},
};
        
1;

__END__

=pod

=head1 AUTHOR

Original code and tests by Stevan Little E<lt>stevan@iinteractive.comE<gt>

The code was refactored into it's own Class from MetaModel t/80_Code.t
by Flavio S. Glock E<lt>fglock@gmail.comE<gt>

=cut
