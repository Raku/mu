#!/usr/bin/perl

# ChangeLog
#
# 2005-08-22
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

# TODO - test integration with Value and Container types
# TODO - add hooks for signature checks, autoboxing, return value checking
# TODO - *@slurpyarray params, boxed data
# TODO - implement say { @_ }
# TODO - move subs from code.t into the module
# TODO - Coro
# TODO - implement infinite list parameter - (1..Inf).shift

use strict;
use warnings;

use Carp 'confess';

use Perl6::MetaModel;
use Perl6::Object;
use Perl6::Value;

my $class_description = '-0.0.1-cpan:FGLOCK';

{
    package Perl6::Signature;

    sub new {
        my ($class, $params, $returns) = @_;
        bless {
            params  => $params,
            returns => $returns,
        } => $class;
    }
    
    sub params  { (shift)->{params}  }
    sub returns { (shift)->{returns} }
}

{
    package Perl6::Params;

    sub new {
        my ($class, @params) = @_;
        bless {
            params => [ @params ]
        } => $class;
    }
    
    sub num_params { 
        scalar @{(shift)->{params}}
    }
    
    sub check_params {
        my ($self, @params) = @_;
        for (my $i = 0; $i < scalar @params; $i++) {
            my $spec = $self->{params}->[$i];
            my $candidate = $params[$i];
            return 0 unless $spec->match_type($candidate);
        }
        return 1;
    }
    
    sub bind_params {
        my ($self, @params) = @_;
        my %bound_params;
        for (my $i = 0; $i < scalar @params; $i++) {
            $bound_params{$self->{params}->[$i]->name} = $params[$i];
        }
        return %bound_params;        
    }
}

{
    package Perl6::Param;

    sub new {
        # TODO - autobox/auto-unbox hook
        my ($class, %param ) = @_;
        my ( $type, $name ) = ( $param{type}, $param{name} );
        $type = 
            defined $type ? $type :
            $name =~ /^\$/ ? sub {
                my $r = ref($_[0]);
                $r eq 'SCALAR' || 
                $r eq 'int'    ||
                $r eq 'num'    ||
                $r eq 'str'    
            } : 
            $name =~ /^\@/ ? sub { ref($_[0]) eq 'ARRAY' || ref($_[0]) eq 'Array' }  : 
            $name =~ /^\%/ ? sub { ref($_[0]) eq 'HASH'  || ref($_[0]) eq 'Hash' }   : 
            sub { 1 };
        bless {
            name => $name,
            type => $type,                             
        } => $class;
    }
    
    sub name  { $_[0]{name} }
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
        attrs => [ '$.body', '$.signature', '$.name' ],
        DESTROY => sub {},
        methods => {
            # TODO
            #'num' =>  sub { Num->new( '$.unboxed' => _('$.unboxed')->num  ) },
            #'int' =>  sub { Int->new( '$.unboxed' => _('$.unboxed')->int  ) },
            #'str' =>  sub { Str->new( '$.unboxed' => 'sub {...}' ) },
            #'bit' =>  sub { Bit->new( '$.unboxed' => _('$.unboxed')->bit  ) },
            'perl' => sub { Str->new( '$.unboxed' => 'sub {...}' ) },
            #'ref' =>  sub { ::CLASS }, 
    
            do => sub {
                my ($self, @arguments) = @_;
                ::SELF->signature->params->check_params(@arguments)
                    || confess "Signature does not match";
                my %bound_params = ::SELF->signature->params->bind_params(@arguments);    
                ::SELF->body->();
            },
            arity => sub {
                ::SELF->signature->params->num_params
            },    
        },
    }
};

class 'Sub'.$class_description => {
    is => [ 'Code', 'Perl6::Object' ],
    class => {},
    instance => {
        attrs => [ '$.return_value' ],
        methods => {
            do => sub {
                _('$.return_value', ::next_METHOD() );
            },
        },
    },
};
    
class 'MultiSub'.$class_description => {
    is => [ 'Code', 'Perl6::Object' ],
    class => {},
    instance => {
        attrs => [ '@.subs', '$.return_value' ],
        methods => {
            do => sub {
                my ($self, @args) = @_;
                my $num_params = scalar(@args);
                my $sub;
                foreach my $_sub ( @{ _('@.subs') } ) {
                    if ($_sub->signature->params->num_params == $num_params) {
                        $sub = $_sub;
                        last;
                    }
                }
                $sub->do(@args);
                _('$.return_value', $sub->return_value );
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
