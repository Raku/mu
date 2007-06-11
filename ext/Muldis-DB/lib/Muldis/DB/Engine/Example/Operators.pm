use v6-alpha;

###########################################################################
###########################################################################

module Muldis::DB::Engine::Example::Operators-0.0.0 {

    use Muldis::DB::Engine::Example::PhysType <ptBool ptText ptBlob ptInt
        ptTuple ptQuasiTuple ptRelation ptQuasiRelation ptTypeInvoNQ
        ptTypeInvoAQ ptTypeDictNQ ptTypeDictAQ ptValueDictNQ ptTypeDictAQ>;

    my Hash $OPS = {

###########################################################################

## sys.type.Bool ##


## sys.type.Text ##


## sys.type.Blob ##


## sys.type.Int ##

'sys.rtn.Int.equal' => sub (:$v1!, :$v2!) {
    return ptBool( :v($v1.equal( $v2 )) );
},

'sys.rtn.Int.not_equal' => sub (:$v1!, :$v2!) {
    return ptBool( :v(!$v1.equal( $v2 )) );
},

'sys.rtn.Int.assign' => sub (:$target!, :$v!) {
    $target.set( $v );
    return;
},

'sys.rtn.Int.sum' => sub (:$addends!) {
    my Int $sum = 0;
    for $addends.array_from_value_attr() -> $addend {
        $sum += $addend.v();
    }
    return ptInt( :v($sum) );
},

'sys.rtn.Int.difference' => sub (:$minuend!, :$subtrahend!) {
    return ptInt( :v($minuend.v() - $subtrahend.v()) );
},

'sys.rtn.Int.product' => sub (:$factors!) {
    my Int $product = 1;
    for $factors.array_from_value_attr() -> $factor {
        $product *= $factor.v();
    }
    return ptInt( :v($product) );
},

'sys.rtn.Int.quotient' => sub (:$dividend!, :$divisor!) {
    my Int $divisor_v = $divisor.v();
    die q{sys.rtn.Int.quotient(): Arg :$divisor is zero.}
        if $divisor_v === 0;
#    return ptInt( :v(floor ($dividend.v() div $divisor_v)) );
    return ptInt( :v(floor ($dividend.v() / $divisor_v)) );
},

'sys.rtn.Int.remainder' => sub (:$dividend!, :$divisor!) {
    my Int $divisor_v = $divisor.v();
    die q{sys.rtn.Int.remainder(): Arg :$divisor is zero.}
        if $divisor_v === 0;
#    return ptInt( :v($dividend.v() mod $divisor_v) );
    return ptInt( :v($dividend.v() % $divisor_v) );
},

'sys.rtn.Int.abs' => sub (:$v!) {
    return ptInt( :v(abs $v.v()) );
},

'sys.rtn.Int.power' => sub (:$radix!, :$exponent!) {
    return ptInt( :v($radix.v() ** $exponent.v()) );
},

## sys.type.Tuple ##


## sys.type.Relation ##


###########################################################################

    }; # my Hash $OPS

    sub get_ops of Hash () {
        return $OPS;
    }

} # Muldis::DB::Engine::Example::Operators

###########################################################################
###########################################################################

=pod

=encoding utf8

=head1 NAME

Muldis::DB::Engine::Example::Operators -
Implementations of all core Muldis D system-defined operators

=head1 VERSION

This document describes Muldis::DB::Engine::Example::Operators version
0.0.0 for Perl 6.

=head1 DESCRIPTION

This file is used internally by L<Muldis::DB::Engine::Example>; it is not
intended to be used directly in user code.

It provides implementations of all core Muldis D system-defined operators,
and their API is designed to exactly match the operator definitions in
L<Muldis::DB::Language>.

Specifically, this file implements the core system-defined operators that
all Muldis D implementations must have, which is the selectors for and
general purpose functions and update operators for these data types: Bool,
Text, Blob, Int, Tuple, Relation, and the Cat.* types.

By contrast, the operators specific to the optional data types are
implemented by other files: L<Muldis::DB::Engine::Example::Operators::Num>,
L<Muldis::DB::Engine::Example::Operators::Temporal>,
L<Muldis::DB::Engine::Example::Operators::Spatial>.

=head1 BUGS AND LIMITATIONS

The operators declared in this file assume that any user-defined Muldis D
code which could be invoking them has already been validated by the Muldis
D compiler, in so far as compile time validation can be done, and so the
operators in this file only test for invalid input such that couldn't be
expected to be caught at compile time.  For example, it is usually expected
that the compiler will catch attempts to invoke these operators with the
wrong number of arguments, or arguments with the wrong names or data types.
So if the compiler missed something which the runtime doesn't expect to
have to validate, then the Example Engine could have inelegant failures.

=head1 AUTHOR

Darren Duncan (C<perl@DarrenDuncan.net>)

=head1 LICENSE AND COPYRIGHT

This file is part of the Muldis::DB framework.

Muldis::DB is Copyright © 2002-2007, Darren Duncan.

See the LICENSE AND COPYRIGHT of L<Muldis::DB> for details.

=cut
