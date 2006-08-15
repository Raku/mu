#
# - Value classes 
# Num, Int, Str, Bit, Pair, Ref, List
#
# - functions for implementation of unboxed Perl6 Values in Perl5
# Pugs::Runtime::Value::Num - Inf, NaN
# Pugs::Runtime::Value::Int
# Pugs::Runtime::Value::Str
# Pugs::Runtime::Value::Bit - bool::* taint::*
# Pugs::Runtime::Value::List (separate file)

# ChangeLog
#
# 2005-10-03
# * Fixed Pair stringification ("$key\t$value") and .perlification
#   ("($key.perl() => $value.perl())")
#
# 2005-09-27
# * Str.decrement() works
# * Migrated to MetaModel-2 (putter)
#
# 2005-09-03
# * Fixed Pair stringification
#
# 2005-09-01
# * Added support for unboxed Pair.key, Pair.value
#
# 2005-08-18
# * clean-up Ref implementation - uses AUTOLOAD to auto-deref
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
# * refactored from Pugs::Runtime::Value::List

# Notes:
# - All library functions - add, subtract, sqrt, ...
#   are implemented using multisubs
# - 'Ref' do not auto-deref List or any other Value - only Containers

# TODO - implement tests from t/var/autoderef.t
# TODO - constant
# TODO - move .increment, .decrement to Primitives (this will break some tests)
# TODO - move Num::Inf, Num::NaN to Primitives

use v6-alpha;

use v5;

sub Pugs::Runtime::Value::p6v_isa {
    my($o,$cls)=@_;
    my $ref = ref($o);
    return 1 if $ref eq 'Dispatchable' && $o->isa($cls);
    return 0;
}

use v6;

# TODO
# my $class_description = '-0.0.1-cpan:FGLOCK';

class Num {

    has $.unboxed;

    # TODO - pi ?
    sub Inf {
        my ($class) = @_;           
        return $class.new( '$.unboxed' => Pugs::Runtime::Value::Num::Inf() );
    }
    sub NaN {
        my ($class) = @_;           
        return $class->new( '$.unboxed' => Pugs::Runtime::Value::Num::NaN() );
    }

    method num { self }
    method int { 
        Int.new( '$.unboxed' => Pugs::Runtime::Value::Num::to_int( self.unboxed ) ) 
    }
    method str  sub { 
        Str.new( '$.unboxed' => Pugs::Runtime::Value::Num::to_str( self.unboxed ) )
    }
    method bit { 
        Bit.new( '$.unboxed' => Pugs::Runtime::Value::Num::to_bit( self.unboxed ) )
    }
    method perl { self.str }
    # method increment - don't belong here
    # method decrement - don't belong here
    method ref { self }

};

use v5;

class1 'Int'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' =>  sub { Num->new( '$.unboxed' => Pugs::Runtime::Value::Int::to_num( _('$.unboxed') ) ) },
            'int' =>  sub { $::SELF },
            'str' =>  sub { Str->new( '$.unboxed' => Pugs::Runtime::Value::Int::to_str( _('$.unboxed') ) ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => Pugs::Runtime::Value::Int::to_bit( _('$.unboxed') ) ) },
            'perl' => sub { $::SELF->str },
            'increment' => sub { 
                my $value = _('$.unboxed');
                $::SELF->ref->new( '$.unboxed' => ++$value ) },
            'decrement' => sub { 
                my $value = _('$.unboxed');
                $::SELF->ref->new( '$.unboxed' => --$value ) },
            'ref' => sub { $::CLASS }, 
        },
    }
};

class1 'Str'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' =>  sub { Num->new( '$.unboxed' => Pugs::Runtime::Value::Str::to_num( _('$.unboxed') ) ) },
            'int' =>  sub { Int->new( '$.unboxed' => Pugs::Runtime::Value::Str::to_int( _('$.unboxed') ) ) },
            'str' =>  sub { $::SELF },
            'bit' =>  sub { Bit->new( '$.unboxed' => Pugs::Runtime::Value::Str::to_bit( _('$.unboxed') ) ) },
            'perl' => sub { 
                my $tmp = _('$.unboxed');
                $tmp =~ s/(\\|\')/\\$1/g;  # quote ' \
                return Str->new( '$.unboxed' => "'" . $tmp . "'" );
              },
            'increment' => sub { 
                my $value = _('$.unboxed');
                $::SELF->ref->new( '$.unboxed' => ++$value ) },
            'decrement' => sub { 
                my $value = _('$.unboxed');
                # perl5 doesn't support string decrement
                $::SELF->ref->new( '$.unboxed' => Pugs::Runtime::Value::Str::decrement( $value ) ) },
            'ref' => sub { $::CLASS }, 
        },
    }
};

class1 'Bit'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.unboxed' ],
        DESTROY => sub {},
        methods => {
            'num' => sub { Num->new( '$.unboxed' => Pugs::Runtime::Value::Bit::to_num( _('$.unboxed') ) ) },
            'int' => sub { Int->new( '$.unboxed' => Pugs::Runtime::Value::Bit::to_int( _('$.unboxed') ) ) },
            'str' => sub { Str->new( '$.unboxed' => Pugs::Runtime::Value::Bit::to_str( _('$.unboxed') ) ) },
            'bit' => sub { $::SELF },
            'perl' => sub { $::SELF->str },
            'ref' => sub { $::CLASS }, 
        },
    }
};

class1 'Rat'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {},
    },
    instance => {
        attrs => [ '$.a', '$.b' ],
        DESTROY => sub {},
        methods => {
            'num' =>  sub { Num->new( '$.unboxed' => _('$.a')/_('$.b') ) },
            'int' =>  sub { Int->new( '$.unboxed' => Pugs::Runtime::Value::Num::to_int( _('$.a')/_('$.b') ) ) },
            'str' =>  sub { Str->new( '$.unboxed' => Pugs::Runtime::Value::Num::to_str( _('$.a')/_('$.b') ) ) },
            'bit' =>  sub { Bit->new( '$.unboxed' => Pugs::Runtime::Value::Num::to_bit( _('$.a')/_('$.b') ) ) },
            'perl' => sub { $::SELF->str },
            'ref' =>  sub { $::CLASS }, 
        },
    }
};

class1 'Pair'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.key', '$.value' ],  # [ '$.value' => { access => 'rw' } ] ],
        DESTROY => sub {},
        methods => {
            'num' => sub { Num->new( '$.unboxed' => 0 ) },
            'int' => sub { Int->new( '$.unboxed' => 0 ) },
            'str' => sub {
                my $key =   Pugs::Runtime::Value::stringify( _('$.key') );
                my $value = Pugs::Runtime::Value::stringify( _('$.value') );
                Str->new( '$.unboxed' => "$key\t$value" ) 
              },
            'bit' => sub { Bit->new( '$.unboxed' => 0 ) },
            'perl' => sub { 
                my $self = shift;
                my $key =   Pugs::Runtime::Value::stringify( $self->key->perl );
                my $value = Pugs::Runtime::Value::stringify( $self->value->perl );
                Str->new( '$.unboxed' => "($key => $value)" ) 
              },
            'unboxed' => sub { ( _('$.key'), _('$.value') ) },
            'ref' => sub { $::CLASS }, 
            'isa' => sub { ::next_METHOD() },
            'does' => sub { ::next_METHOD() },
            'AUTOLOAD' => sub {
                my ($self, @param) = @_;
                my $method = __('$AUTOLOAD');
                die "unsupported pair method .$method";
            },

        },
    }
};

class1 'Ref'.$class_description => {
    is => [ $::Object ],
    class => {
        attrs => [],
        methods => {}
    },
    instance => {
        attrs => [ '$.referred' ],
        DESTROY => sub {
            # _('$.referred' => undef); # XXX - MM2.0 gc workaround
        },
        methods => { 
             # See perl5/Perl6-MetaModel/t/14_AUTOLOAD.t  
            'isa' => sub { 
                return 1 if $_[1] eq 'Array' && Pugs::Runtime::Value::p6v_isa( _('$.referred'), 'Array' );
                return 1 if $_[1] eq 'Hash' && Pugs::Runtime::Value::p6v_isa( _('$.referred'), 'Hash' );
                ::next_METHOD();
            },
            'does' => sub { ::next_METHOD() },
            'unboxed' => sub { \(_('$.referred')) },
            'AUTOLOAD' => sub {
                my ($self, @param) = @_;
                my $method = __('$AUTOLOAD');
                my $tmp = _('$.referred');
                # Array and Hash are auto-dereferenced
                if ( ref $tmp && ( 
                        $tmp->isa( 'Array' ) || $tmp->isa( 'Hash' )
                    ) ) {
                    return $tmp->$method( @param );
                }
                # everything else is not auto-dereferenced
                return Bit->new( '$.unboxed' => 1 ) 
                    if $method eq 'bit';
                return $::CLASS 
                    if $method eq 'ref';
                if ( $method eq 'perl' || $method eq 'str' ) {
                    return Str->new( '$.unboxed' => '\\' . Pugs::Runtime::Value::stringify($tmp) ) 
                }
                return Bit->new( '$.unboxed' => defined $tmp ? 1 : 0 ) 
                    if $method eq 'defined';                
                die "unsupported ref method .$method";
            },
        },
    }
};

role Lazy => {
    methods => {}
};

role Eager => {
    methods => {}
};

class1 'List'.$class_description => {
    is => [ $::Object ],
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
            'ref' =>  sub { $::CLASS }, 

            'shift' => sub { _('$.unboxed')->shift },
            'pop' =>   sub { _('$.unboxed')->pop   },

            # These methods are implemented in List.pm but are not exposed here,
            # they are going to be moved to Primitives:
            #     elems, is_infinite, is_lazy, is_contiguous, flatten, grep, ...
        },
    }
};

# ---------- Implementation of unboxed values --------------

#package Pugs::Runtime::Value;
#
#sub to_ref {
#    my $object = shift;
#    my $class_name = ::dispatch( ::meta( $object ), 'name' );  # ** get the class name, from object
#    my $class = $Pugs::Runtime::Class::ALL_CLASSES{$class_name};  # ** get the class, from class name
#    return $class;
#}

$Pugs::Runtime::Value::obj_id = '**ObJecT**' . rand;
sub Pugs::Runtime::Value::identify {
    my $key = shift;
    return $Pugs::Runtime::Value::obj_id unless defined $key;
    my $s = $key;
    if ( ref($key) && (
        $key->isa( 'Int' ) ||
        $key->isa( 'Num' ) ||
        $key->isa( 'Str' ) ||
        $key->isa( 'Bit' ) ||
        $key->isa( 'Rat' ) )
    ) {
        $s = $key->str->unboxed
    }
    elsif ( UNIVERSAL::can( $key, 'id' ) ) {
        $s = $Pugs::Runtime::Value::obj_id . $key->id
    }
    return $s;
}

sub Pugs::Runtime::Value::stringify {
    my $s = shift;
    $s = $s->fetch if Pugs::Runtime::Value::p6v_isa( $s, 'Scalar');
    my $tmp;
    # warn "stringify - $s\n";
    eval { $tmp = $s->str(max=>3) };
    $s = $tmp unless $@;
    # warn "   str - $s - $@\n";
    eval { $tmp = $s->unboxed };
    $s = $tmp unless $@;
    # warn "   unboxed - $s - $@\n";
    return 'undef' unless defined $s;
    no warnings 'numeric';
    $s = Pugs::Runtime::Value::Num::to_str( $s ) if $s+0 eq $s;
    return $s;
}

sub Pugs::Runtime::Value::numify {
    my $s = shift;
    $s = $s->fetch if Pugs::Runtime::Value::p6v_isa( $s, 'Scalar');
    my $tmp;
    eval { $tmp = $s->num };
    $s = $tmp unless $@;
    eval { $tmp = $s->unboxed };
    $s = $tmp unless $@;
    unless ( defined $s ) {
        warn "attempting to use 'undef' as a number";
        return 0;
    }
    no warnings 'numeric';
    $s = Pugs::Runtime::Value::Str::to_num( $s ) if $s+0 ne $s;
    return $s;
}

package Pugs::Runtime::Value::Num;

use constant Inf => 100**100**100;
use constant NaN => Inf / Inf;

sub to_str        { 
    my $v = shift;
    # native
    return 'Inf'  if $v eq "".&Pugs::Runtime::Value::Num::Inf;
    return '-Inf' if $v eq "".(-&Pugs::Runtime::Value::Num::Inf);
    return 'NaN'  if $v eq "".&Pugs::Runtime::Value::Num::NaN;
    $v = 0 + $v;
    return 'Inf'  if $v == Inf;
    return '-Inf' if $v == -&Inf;
    return 'NaN'  if $v =~ m/n/i;
    return "" . $v 
}
sub to_bit        { $_[0] == 0 ? '' : 1 }
sub to_num        { 0 + $_[0] }
sub to_int        { int( $_[0] ) }

package Pugs::Runtime::Value::Int;

sub to_str        { 
    no warnings 'uninitialized';
    my $v = 0 + $_[0];
    return 'Inf'  if $v == Pugs::Runtime::Value::Num::Inf;
    return '-Inf' if $v == -&Pugs::Runtime::Value::Num::Inf;
    return 'NaN'  if $v =~ m/n/i;
    return "" . $v 
}
sub to_bit        { $_[0] == 0 ? '' : 1 }
sub to_num        { 0 + $_[0] }
sub to_int        { $_[0] }

package Pugs::Runtime::Value::Str;

sub to_str        { $_[0] }
sub to_bit        { 
    return '' if $_[0] eq '0' || $_[0] eq '';
    return 1;
}
sub to_num        {
    my $v = $_[0];
    $v =~ s/\s+//g;
    # Perl 6
    return Pugs::Runtime::Value::Num::Inf   if $v eq 'Inf';
    return -&Pugs::Runtime::Value::Num::Inf if $v eq '-Inf';
    return Pugs::Runtime::Value::Num::NaN   if $v eq 'NaN';
    # native
    return Pugs::Runtime::Value::Num::Inf   if $v eq "".&Pugs::Runtime::Value::Num::Inf;
    return -&Pugs::Runtime::Value::Num::Inf if $v eq "".(-&Pugs::Runtime::Value::Num::Inf);
    return Pugs::Runtime::Value::Num::NaN   if $v eq "".&Pugs::Runtime::Value::Num::NaN;

    no warnings 'numeric';
    return 0 + $v;
}
sub to_int        { Pugs::Runtime::Value::Num::to_int( to_num( $_[0] ) ) }

sub decrement {
    return '' if $_[0] eq '' || $_[0] eq 'a' || $_[0] eq '0';
    my ($s,$c) = $_[0] =~ /(.*)(.)/;
    return str_decr($s) . 'z' if $c eq 'a';
    return str_decr($s) . '9' if $c eq '0';
    return $s . chr( ord($c) - 1 );
}

package Pugs::Runtime::Value::Bit;

# built-in, unboxed 'bit' enums
sub bool::false      { '' }
sub bool::true       { 1 }
sub taint::untainted { '' }
sub taint::tainted   { 1 }

sub to_str        { $_[0] ? 1 : '' }
sub to_bit        { $_[0] }
sub to_num        { $_[0] == 0 ? 0 : 1 }
sub to_int        { to_num( $_[0] ) }

use v6;

=head1 NAME

Pugs::Runtime::Value - Perl6 boxed and unboxed Values

=head1 SYNOPSIS

  use Pugs::Runtime::Value;
  
  # unboxed Perl5 value
  my $num = Pugs::Runtime::Value::Str::to_num( 'NaN' );
 
  # Perl6 "Num" object
  my $num = Num->NaN;

=head1 DESCRIPTION

This module implements the "Value" classes - Num, Int, Str, Bit, Pair.

It also implements platform-specific, low-level functions for "unboxed" Values.

=head1 SEE ALSO

Pugs

=head1 AUTHOR

Flavio S. Glock, E<lt>fglock@gmail.com<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Flavio S. Glock

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut
