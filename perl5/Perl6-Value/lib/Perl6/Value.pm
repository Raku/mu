#
# - Value classes 
# Num, Int, Str, Bit, Pair, Ref, List
#
# - functions for implementation of unboxed Perl6 Values in Perl5
# Perl6::Value::Num - Inf, NaN
# Perl6::Value::Int
# Perl6::Value::Str
# Perl6::Value::Bit - bool::* taint::*
# Perl6::Value::List (separate file)

# ChangeLog
#
# 2005-08-18
# * added unboxed enums: bool::* taint::*
# * 'List' clean up - removed 'multisub' methods
#
# 2005-08-17
# * added boxed type: List (lazy, non-lazy, infinite)
# * More descriptive method names:
#   - .unboxed() - returns the unboxed value
#     Pair->unboxed returns two objects.
#   - Ref->referred - returns the referred object
#   - removed method .value() - except for Pair
#
# 2005-08-16
# * 'Scalar' temporarily implemented here 
# * added class Ref
# * added method .perl()
#
# 2005-08-15
# * added boxed types: Num, Int, Str, Bit, Pair
#
# 2005-08-13
# * refactored from Perl6::Value::List

# Notes:
# - All library functions - add, subtract, sqrt, ...
#   are implemented using multisubs
# - 'Ref' do not auto-deref List or any other Value

# TODO - verify .ref() implementation - AUTOMETH
# TODO - implement tests from t/var/autoderef.t
# TODO - tie
# TODO - constant
# TODO - List methods: .str, .perl
# TODO - move .increment, .decrement to Primitives (this will break some tests)
# TODO - move Num::Inf, Num::NaN to Primitives

use strict;

use Perl6::MetaModel;
use Perl6::Object;

my $class_description = '-0.0.1-cpan:FGLOCK';

class 'Num'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {
            # TODO - pi ?
            'Inf' => sub {
                my ($class) = @_;           
                return $class->new( '$.unboxed' => &Perl6::Value::Num::Inf );
            },
            'NaN' => sub {
                my ($class) = @_;           
                return $class->new( '$.unboxed' => &Perl6::Value::Num::NaN );
            },
        }
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' =>  sub { ::SELF },
            'int' =>  sub { Int->new( '$.unboxed' => Perl6::Value::Num::to_int( _('$.unboxed') ) ) },
            'str' =>  sub { Str->new( '$.unboxed' => Perl6::Value::Num::to_str( _('$.unboxed') ) ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => Perl6::Value::Num::to_bit( _('$.unboxed') ) ) },
            'perl' => sub { ::SELF->str },
            'increment' => sub { 
                my $value = _('$.unboxed');
                ::SELF->ref->new( '$.unboxed' => ++$value ) },
            'decrement' => sub { 
                my $value = _('$.unboxed');
                ::SELF->ref->new( '$.unboxed' => --$value ) },
            'ref' => sub { ::CLASS }, 
        },
    }
};

class 'Int'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' =>  sub { Num->new( '$.unboxed' => Perl6::Value::Int::to_num( _('$.unboxed') ) ) },
            'int' =>  sub { ::SELF },
            'str' =>  sub { Str->new( '$.unboxed' => Perl6::Value::Int::to_str( _('$.unboxed') ) ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => Perl6::Value::Int::to_bit( _('$.unboxed') ) ) },
            'perl' => sub { ::SELF->str },
            'increment' => sub { 
                my $value = _('$.unboxed');
                ::SELF->ref->new( '$.unboxed' => ++$value ) },
            'decrement' => sub { 
                my $value = _('$.unboxed');
                ::SELF->ref->new( '$.unboxed' => --$value ) },
            'ref' => sub { ::CLASS }, 
        },
    }
};

class 'Str'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' =>  sub { Num->new( '$.unboxed' => Perl6::Value::Str::to_num( _('$.unboxed') ) ) },
            'int' =>  sub { Int->new( '$.unboxed' => Perl6::Value::Str::to_int( _('$.unboxed') ) ) },
            'str' =>  sub { ::SELF },
            'bit' =>  sub { Bit->new( '$.unboxed' => Perl6::Value::Str::to_bit( _('$.unboxed') ) ) },
            'perl' => sub { 
                my $tmp = _('$.unboxed');
                $tmp =~ s/(\\|\')/\\$1/g;  # quote ' \
                return Str->new( '$.unboxed' => "'" . $tmp . "'" );
              },
            'increment' => sub { 
                my $value = _('$.unboxed');
                ::SELF->ref->new( '$.unboxed' => ++$value ) },
            'decrement' => sub { 
                my $value = _('$.unboxed');
                ::SELF->ref->new( '$.unboxed' => --$value ) },
            'ref' => sub { ::CLASS }, 
        },
    }
};

class 'Bit'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' => sub { Num->new( '$.unboxed' => Perl6::Value::Bit::to_num( _('$.unboxed') ) ) },
            'int' => sub { Int->new( '$.unboxed' => Perl6::Value::Bit::to_int( _('$.unboxed') ) ) },
            'str' => sub { Str->new( '$.unboxed' => Perl6::Value::Bit::to_str( _('$.unboxed') ) ) },
            'bit' => sub { ::SELF },
            'perl' => sub { ::SELF->str },
            'ref' => sub { ::CLASS }, 
        },
    }
};

class 'Pair'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.key', [ '$.value' => { access => 'rw' } ] ],
        DESTROY => sub {},
        methods => {
            'num' => sub { Num->new( '$.unboxed' => 0 ) },
            'int' => sub { Int->new( '$.unboxed' => 0 ) },
            'str' => sub { Str->new( '$.unboxed' => '' ) },
            'bit' => sub { Bit->new( '$.unboxed' => 0 ) },
            'perl' => sub { 
                my $key = defined _('$.key') ? _('$.key')->perl->unboxed : 'undef';
                my $value = defined _('$.value') ? _('$.value')->perl->unboxed : 'undef';
                Str->new( '$.unboxed' => "($key, $value)" ) 
              },
            'unboxed' => sub { ( _('$.key'), _('$.value') ) },
            'ref' => sub { ::CLASS }, 
        },
    }
};

class 'Ref'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.referred' ],
        DESTROY => sub {
            # XXX - didn't undefine the value 
            # _('$.cell' => undef) },
            my $self = shift;
            $self->{'instance_data'}{'$.referred'} = undef;  # XXX
        },
        methods => { 
             # See perl5/Perl6-MetaModel/t/14_AUTOLOAD.t  
            'isa' => sub { next_METHOD() },
            'AUTOLOAD' => sub {
                my ($self, @param) = @_;
                my $method = AUTOLOAD($self);
                my $tmp = _('$.referred');
                # Containers are auto-dereferenced
                if ( defined $tmp && ( 
                        $tmp->isa( 'Scalar' ) || $tmp->isa( 'Array' ) || $tmp->isa( 'Hash' )
                    ) ) {
                    return $tmp->$method( @param );
                }
                # everything else is not auto-dereferenced
                return Bit->new( '$.unboxed' => 1 ) 
                    if $method eq 'bit';
                return ::CLASS 
                    if $method eq 'ref';
                if ( $method eq 'perl' ) {
                    return Str->new( '$.unboxed' => '\\undef' ) unless defined $tmp;
                    return Str->new( '$.unboxed' => '\\' . $tmp->perl->unboxed ) 
                }
                return Bit->new( '$.unboxed' => defined $tmp ? 1 : 0 ) 
                    if $method eq 'defined';                
                die "unsupported ref method .$method";
            },
        },
    }
};

class 'List'.$class_description => {
    is => [ 'Perl6::Object' ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' =>  sub { Num->new( '$.unboxed' => _('$.unboxed')->num  ) },
            'int' =>  sub { Int->new( '$.unboxed' => _('$.unboxed')->int  ) },
            'str' =>  sub { Str->new( '$.unboxed' => _('$.unboxed')->str  ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => _('$.unboxed')->bit  ) },
            'perl' => sub { Str->new( '$.unboxed' => _('$.unboxed')->perl ) },
            'ref' =>  sub { ::CLASS }, 

            'shift' => sub { _('$.unboxed')->shift },
            'pop' =>   sub { _('$.unboxed')->pop   },

            # These methods are implemented in List.pm but are not exposed here,
            # they are going to be moved to Primitives:
            #     elems, is_infinite, is_lazy, is_contiguous, flatten, grep, ...
        },
    }
};

# ---------- Implementation of unboxed values --------------

#package Perl6::Value;
#
#sub to_ref {
#    my $object = shift;
#    my $class_name = ::dispatch( ::meta( $object ), 'name' );  # ** get the class name, from object
#    my $class = $Perl6::Class::ALL_CLASSES{$class_name};  # ** get the class, from class name
#    return $class;
#}

package Perl6::Value::Num;

use constant Inf => 100**100**100;
use constant NaN => Inf / Inf;

sub to_str        { 
    my $v = 0 + $_[0];
    return 'Inf'  if $v == Inf;
    return '-Inf' if $v == -&Inf;
    return 'NaN'  if $v =~ m/n/i;
    return "" . $v 
}
sub to_bit        { $_[0] == 0 ? '' : 1 }
sub to_num        { 0 + $_[0] }
sub to_int        { int( $_[0] ) }

package Perl6::Value::Int;

sub to_str        { 
    no warnings 'uninitialized';
    my $v = 0 + $_[0];
    return 'Inf'  if $v == Perl6::Value::Num::Inf;
    return '-Inf' if $v == -&Perl6::Value::Num::Inf;
    return 'NaN'  if $v =~ m/n/i;
    return "" . $v 
}
sub to_bit        { $_[0] == 0 ? '' : 1 }
sub to_num        { 0 + $_[0] }
sub to_int        { $_[0] }

package Perl6::Value::Str;

sub to_str        { $_[0] }
sub to_bit        { 
    return '' if $_[0] eq '0' || $_[0] eq '';
    return 1;
}
sub to_num        {
    my $v = $_[0];
    $v =~ s/\s+//g;
    return Perl6::Value::Num::Inf  if $v eq 'Inf';
    return -&Perl6::Value::Num::Inf if $v eq '-Inf';
    return Perl6::Value::Num::NaN  if $v eq 'NaN';
    return 0 + $v;
}
sub to_int        { Perl6::Value::Num::to_int( to_num( $_[0] ) ) }

package Perl6::Value::Bit;

# built-in, unboxed 'bit' enums
sub bool::false      { '' }
sub bool::true       { 1 }
sub taint::untainted { '' }
sub taint::tainted   { 1 }

sub to_str        { $_[0] ? 1 : '' }
sub to_bit        { $_[0] }
sub to_num        { $_[0] == 0 ? 0 : 1 }
sub to_int        { to_num( $_[0] ) }

1;
__END__

=head1 NAME

Perl6::Value - Perl6 boxed and unboxed Values

=head1 SYNOPSIS

  use Perl6::Value;
  
  # unboxed Perl5 value
  my $num = Perl6::Value::Str::to_num( 'NaN' );
 
  # Perl6 "Num" object
  my $num = Num->NaN;

=head1 DESCRIPTION

This module implements the "Value" classes - Num, Int, Str, Bit, Pair.

It also implements platform-specific, low-level functions for "unboxed" Values.

=head1 SEE ALSO

Pugs

=head1 AUTHOR

Flavio S. Glock, E<lt>fglock@Egmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Flavio S. Glock

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut
