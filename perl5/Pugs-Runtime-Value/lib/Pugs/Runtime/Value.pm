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
# 2006-08-15
# * forked from Perl6::Value
# * changed 'use MetaModel' to 'use v6-alpha' (p6)
# * unboxed subs moved to Pugs::Runtime::Unboxed
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
use Pugs::Runtime::Unboxed;

# TODO
# my $class_description = '-0.0.1-cpan:FGLOCK';

class Object {
}

class Num is Object {

    has $.unboxed;

    # TODO - pi ?
    sub Inf {
        my ($class) = @_;           
        return $class.new( unboxed => Pugs::Runtime::Value::Num::Inf() );
    }
    sub NaN {
        my ($class) = @_;           
        return $class.new( unboxed => Pugs::Runtime::Value::Num::NaN() );
    }

    method num { 
        self 
    }
    method int { 
        Int.new( unboxed => Pugs::Runtime::Value::Num::to_int( self.unboxed ) ) 
    }
    method str { 
        Str.new( unboxed => Pugs::Runtime::Value::Num::to_str( self.unboxed ) )
    }
    method bit { 
        Bit.new( unboxed => Pugs::Runtime::Value::Num::to_bit( self.unboxed ) )
    }
    method perl { self.str }
    method WHAT  { self.class }

}

class Int is Object {

    has $.unboxed;

    method num { 
        Num.new( unboxed => Pugs::Runtime::Value::Int::to_num( self.unboxed ) ) 
    }
    method int { 
        self 
    }
    method str { 
        Str.new( unboxed => Pugs::Runtime::Value::Int::to_str( self.unboxed ) )
    }
    method bit { 
        Bit.new( unboxed => Pugs::Runtime::Value::Int::to_bit( self.unboxed ) )
    }
    method perl { self.str }
    method WHAT  { self.class }

}

class Str is Object {

    has $.unboxed;

    method num { 
        Num.new( unboxed => Pugs::Runtime::Value::Str::to_num( self.unboxed ) ) 
    }
    method int { 
        Int.new( unboxed => Pugs::Runtime::Value::Str::to_int( self.unboxed ) ) 
    }
    method str { 
        self
    }
    method bit { 
        Bit.new( unboxed => Pugs::Runtime::Value::Str::to_bit( self.unboxed ) )
    }
    method perl { 
        # TODO: escape ' and \
        "'" ~ self ~ "'" 
    }
    method WHAT  { self.class }

}

class Bit is Object {

    has $.unboxed;

    method num { 
        Num.new( unboxed => Pugs::Runtime::Value::Bit::to_num( self.unboxed ) ) 
    }
    method int { 
        Int.new( unboxed => Pugs::Runtime::Value::Bit::to_int( self.unboxed ) )
    }
    method str { 
        Str.new( unboxed => Pugs::Runtime::Value::Bit::to_str( self.unboxed ) )
    }
    method bit { 
        self
    }
    method perl { self.str }
    method WHAT  { self.class }

}

class Rat is Object {

    has $.a;
    has $.b;

    method num { 
        Num.new( unboxed => self.a / self.b ) 
    }
    method int { 
        Int.new( unboxed => self.a / self.b )
    }
    method str { 
        Str.new( unboxed => self.a ~ '/' ~ self.b )
    }
    method bit { 
        Bit.new( unboxed => self.a / self.b )
    }
    method perl { self.str }
    method WHAT  { self.class }

}

class Pair is Object {

    has $.key;
    has $.value;

    method num { 
        Num.new( unboxed => 0 ) 
    }
    method int { 
        Int.new( unboxed => 0 )
    }
    method str { 
        Str.new( unboxed => self.key ~ "\t" ~ self.value )
    }
    method bit { 
        Bit.new( unboxed => 0 )
    }
    method perl { "(" ~ self.key ~ " => " ~ self.value ~ ")" }
    method WHAT  { self.class }

}

=begin v5

# TODO: move Capture and List to p6-land

class1 'Ref'.$class_description => {
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


=end 

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

Copyright (C) 2005, 2006 by Flavio S. Glock

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.4 or,
at your option, any later version of Perl 5 you may have available.

=cut
