#
# Value classes - Num, Int, Str, Bit, Pair, Ref, List
#
# Perl6::Value::Num
# Perl6::Value::Int
# Perl6::Value::Str
# Perl6::Value::Bit
# Perl6::Value::List (separate file)
# - functions for implementation of Perl6 Values in Perl5

# ChangeLog
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

# TODO - library functions - add, subtract, sqrt, ...
# TODO - verify .ref() implementation - AUTOMETH
# TODO - implement tests from t/var/autoderef.t
# TODO - tie
# TODO - constant
# TODO - List methods: .str, .perl
# TODO - test 'Ref' auto-deref with a lazy List

use strict;

use Perl6::MetaModel;
use Perl6::Object;

my $class_description = '-0.0.1-cpan:FGLOCK';

# $Perl6::Value::Num::class =
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
            'num' =>  sub { SELF },
            'int' =>  sub { Int->new( '$.unboxed' => Perl6::Value::Num::to_int( _('$.unboxed') ) ) },
            'str' =>  sub { Str->new( '$.unboxed' => Perl6::Value::Num::to_str( _('$.unboxed') ) ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => Perl6::Value::Num::to_bit( _('$.unboxed') ) ) },
            'perl' => sub { SELF->str },
            'increment' => sub { 
                my $value = _('$.unboxed');
                SELF->ref->new( '$.unboxed' => ++$value ) },
            'decrement' => sub { 
                my $value = _('$.unboxed');
                SELF->ref->new( '$.unboxed' => --$value ) },
            'ref' => sub { CLASS }, 
        },
    }
};

# $Perl6::Value::Int::class =
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
            'int' =>  sub { SELF },
            'str' =>  sub { Str->new( '$.unboxed' => Perl6::Value::Int::to_str( _('$.unboxed') ) ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => Perl6::Value::Int::to_bit( _('$.unboxed') ) ) },
            'perl' => sub { SELF->str },
            'increment' => sub { 
                my $value = _('$.unboxed');
                SELF->ref->new( '$.unboxed' => ++$value ) },
            'decrement' => sub { 
                my $value = _('$.unboxed');
                SELF->ref->new( '$.unboxed' => --$value ) },
            'ref' => sub { CLASS }, 
        },
    }
};

# $Perl6::Value::Str::class =
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
            'str' =>  sub { SELF },
            'bit' =>  sub { Bit->new( '$.unboxed' => Perl6::Value::Str::to_bit( _('$.unboxed') ) ) },
            'perl' => sub { 
                my $tmp = _('$.unboxed');
                $tmp =~ s/(\\|\')/\\$1/g;  # quote ' \
                return Str->new( '$.unboxed' => "'" . $tmp . "'" );
              },
            'increment' => sub { 
                my $value = _('$.unboxed');
                SELF->ref->new( '$.unboxed' => ++$value ) },
            'decrement' => sub { 
                my $value = _('$.unboxed');
                SELF->ref->new( '$.unboxed' => --$value ) },
            'ref' => sub { CLASS }, 
        },
    }
};

# $Perl6::Value::Bit::class =
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
            'bit' => sub { SELF },
            'perl' => sub { SELF->str },
            'ref' => sub { CLASS }, 
        },
    }
};

# $Perl6::Value::Pair::class =
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
            'ref' => sub { CLASS }, 
        },
    }
};

# quick hack until we get AUTOMETH working
# - not proxied methods: .id, .value
my %ref_AUTOMETH = map {
        my $method = $_;
        ( $method => sub { 
            my $tmp = _('$.referred');
            my @param = @_;
            shift @param;
            if ( defined $tmp && ( 
                    $tmp->isa( 'Scalar' ) || $tmp->isa( 'Array' ) || $tmp->isa( 'Hash' )
                ) ) {
                return $tmp->$method( @param );
            }
            else {
                # "real ref"
                return Bit->new( '$.unboxed' => 1 ) if $method eq 'bit';
                return CLASS if $method eq 'ref';
                if ( $method eq 'perl' ) {
                    return Str->new( '$.unboxed' => '\\undef' ) unless defined $tmp;
                    return Str->new( '$.unboxed' => '\\' . $tmp->perl->unboxed ) 
                }
                return Bit->new( '$.unboxed' => defined $tmp ? 1 : 0 ) if $method eq 'defined';                
                die "unsupported ref method .$method";
            }
        } )
    } 
    qw( num int str bit perl ref 
        increment decrement 
        fetch store
        map grep shift pop unshift push reverse
        defined undefine );

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
            %ref_AUTOMETH,
        },
    }
};

# quick hack until we get AUTOMETH working
# - not proxied methods: .id .ref
my %list_AUTOMETH = map {
        my $method = $_;
        ( $method => sub { 
            my $tmp = _('$.unboxed');
            my @param = @_;
            shift @param;
            die "The list object is not defined" unless defined $tmp;
            $tmp = $tmp->$method( @param );
            # "box" the result
            return $tmp
                if $method eq 'shift' ||
                   $method eq 'pop';
            return Str->new( '$.unboxed' => $tmp ) 
                if $method eq 'perl' ||
                   $method eq 'str';
            return Int->new( '$.unboxed' => $tmp ) 
                if $method eq 'elems' ||
                   $method eq 'int';
            return Num->new( '$.unboxed' => $tmp ) 
                if $method eq 'num';
            return Bit->new( '$.unboxed' => $tmp )  
                if $method =~ m/^is_/ ||
                   $method eq 'bit';
            return List->new( '$.unboxed' => $tmp )
                unless UNIVERSAL::isa( $tmp, 'List' );
            return $tmp;
        } )
    } 
    qw( num int str bit perl ref 
        map grep shift pop reverse uniq zip
        kv keys values pairs
        elems is_infinite is_contiguous is_lazy flatten 
       );

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
            %list_AUTOMETH,
            'ref' => sub { CLASS }, 
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
sub to_bit        { $_[0] == 0 ? 0 : 1 }
sub to_num        { 0 + $_[0] }
sub to_int        { int( $_[0] ) }

package Perl6::Value::Int;

sub to_str        { 
    my $v = 0 + $_[0];
    return 'Inf'  if $v == Perl6::Value::Num::Inf;
    return '-Inf' if $v == -&Perl6::Value::Num::Inf;
    return 'NaN'  if $v =~ m/n/i;
    return "" . $v 
}
sub to_bit        { $_[0] != 0 }
sub to_num        { 0 + $_[0] }
sub to_int        { $_[0] }

package Perl6::Value::Str;

sub to_str        { $_[0] }
sub to_bit        { 
    return 0 if $_[0] eq '0' || $_[0] eq '';
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

sub to_str        { $_[0] == 0 ? 'bool::false' : 'bool::true' }
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
