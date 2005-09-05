# $Header: /home/fergal/my/cvs/Code-Perl/lib/Code/Perl/Expr.pm,v 1.10 2003/06/17 21:51:25 fergal Exp $

use strict;

package Code::Perl::Expr;

require Exporter;
use base 'Exporter';

use Code::Perl::Expr::Scalar;
use Code::Perl::Expr::Number;
use Code::Perl::Expr::String;
use Code::Perl::Expr::List;
use Code::Perl::Expr::DerefHash;
use Code::Perl::Expr::DerefArray;
use Code::Perl::Expr::CallSub;
use Code::Perl::Expr::CallMethod;
use Code::Perl::Expr::Not;
use Code::Perl::Expr::Perl;
use Code::Perl::Expr::Append;
use Code::Perl::Expr::Holder;
use Code::Perl::Expr::SubName;

our %EXPORT_TAGS = (
    easy => [qw( scal number string list derefh derefa calls callm boolnot perl
        append holder subname )],
);

our @EXPORT_OK = @{$EXPORT_TAGS{"easy"}};

=head1 NAME

Code::Perl::Expr - Produce Perl expression from a tree

=head1 SYNOPSIS

  use Code::Perl::Expr qw( :easy );

  my $c = derefh(scal('hash'), calls('getkey'));

  print $c->perl; # ($hash)->{getkey()}

=head1 DESCRIPTION

Code::Perl::Expr handles the expressions part of L<Code::Perl>. It can
export convenience functions for constructing new Expr objects of all
available types.

Rather than documenting each available Expression class in it's own file,
they are documented here, along with easy constructor exported by this
module.

=head1 EXPRESSION CLASSES

C<Code::Perl::Expr> has been removed from the front of the class names, so
for example C<SubName> is really C<Code::Perl::Expr::SubName>.

=cut

=head2 Number

  $class->new(Value => $number);

  number($number);

$number - a number

Number is for handling numbers.

  number(5)->perl # 5

=cut

sub number
{
    return Code::Perl::Expr::Number->new(
        Value => shift
    );
}

=head2 String

  $class->new(Value => $string);

  string($string);

$string - a string

String is for handling strings. It produces a double quoted string, escaped
so that it will stay on one line

  string('hello\nworld$')->perl # "hello\nworld\$"

=cut

sub string
{
    return Code::Perl::Expr::String->new(
        Value => shift
    );
}

=head2 Scalar

  $class->new(Name => $name);

  scal($name);

$name - a string

This handles a scalar.

  scal("var")->perl # $var

=cut

sub scal
{
    return Code::Perl::Expr::Scalar->new(
        Name => shift
    );
}

=head2 List

  $class->new(Value => \@exprs);

  list(@exprs);

@exprs - an array of Expression objects

List is for producing a comma separated list of expressions.

  list(scal("v"), string("a"), number(5))->perl # $v, "a", 5

=cut

sub list
{
    return Code::Perl::Expr::List->new(
        Value => [@_]
    );
}

=head2 Not

  $class->new(Expr => $expr);

  boolnot($expr);

$expr - an Expression object.

This applies a logical not to $expr

  boolnot(scal("is"))->perl # ! $is

=cut

sub boolnot
{
    return Code::Perl::Expr::Not->new(
        Expr => shift,
    );
}

=head2 Append

  $class->new(Exprs => \@exprs);

  append(@exprs);

@exprs - a list of Expression objects.

Append produces code that appends the
expressions together using Perl's C<.> string append operator.

  append(string("hello"), string(" world"))->perl # "hello"." world";

=cut

sub append
{
    return Code::Perl::Expr::Append->new(
        Exprs => [@_],
    );
}

=cut

=head2 DerefHash

  $class->new(Ref => $hash, Key => $key);

  derefh($hash, $key);

$hash - Expression objects
$key - Expression objects or string

C<derefh()> will turn $key into a String object if it is a string.

DerefHash is for dereferencing a hash ref.

  derefh(scal("hash"), "key")->perl # $hash->{"key"}
  derefh(scal("hash"), calls("getkey"))->perl # $hash->{getkey()}

=cut

sub derefh
{
    my $hash = shift;
    my $index = shift;
    if (! ref($index))
    {
        $index = string($index);
    }

    return Code::Perl::Expr::DerefHash->new(
        Ref => $hash,
        Key => $index
    );
}

=head2 DerefArray

  $class->new(Ref => $hash, Index => $index);

  derefa($hash, $index);

$hash - an Expression objects
$index - an Expression object or a number

C<derefa()> will turn $index into a Number object if it is a number.

DerefArray is for dereferencing an array ref.

  derefa(scal("array"), 5)->perl # $array->[5]
  derefa(scal("array"), calls("getindex"))->perl # $array->[getindex()]

=cut

sub derefa
{
    my $hash = shift;
    my $index = shift;
    if (! ref($index))
    {
        $index = number($index);
    }

    return Code::Perl::Expr::DerefArray->new(
        Ref => $hash,
        Index => $index
    );
}

=head2 SubName

  $class->new(Value => $name);

  subname($name);

$name should be a string, naming a subroutine or a method. Used when
creating a CallSub or CallMethod object.

=cut

sub subname
{
    return Code::Perl::Expr::SubName->new(
        Value => shift,
    );
}

=head2 CallSub

  $class->new(SubName => $sub, Args => $args);

  calls($subname, @args);

$sub, $args - Expression objects.
$subname - an Expression object or a string

C<calls> will turn $subname into a SubName object if it is a string. It
will wrap @args in a List object.

CallSub will produce code to call the subroutine in $sub with passing the
list in $args.

  calls("wangle", string("John"))->perl # wangle("John")

  callm(scal("sub_ref"), string("John"))->perl # &{$sub_ref}("John")

=cut

sub calls
{
    my $subname = shift;
    if (! ref($subname))
    {
        $subname = subname($subname);
    }

    return Code::Perl::Expr::CallSub->new(
        SubName => $subname,
        Args => list(@_)
    );
}

=head2 CallMethod

  $class->new(Object => $object, MethodName => $method, Args => $args);

  callm($object, $methodname, @args);

$object, $method, $args - Expression object.
$methodname - an Expression object or a string

C<callm> will turn $methodname into a SubName object if it is a string. It
will wrap @args in a List object.

CallMethod will produce code to call the method in $method on the object in
$object, passing the list in $args.

  callm(scal("obj"), "setName", string("John"))->perl # $obj->setName("John")

  callm(scal("obj"), scal("meth"), string("John"))->perl # $obj->$meth("John")

It is possible to pass in any Expression object in $method, however the only
ones that make sense are SubName and Scalar objects.

=cut

sub callm
{
    my $object = shift;
    my $method = shift;
    if (! ref($method))
    {
        $method = subname($method);
    }

    return Code::Perl::Expr::CallMethod->new(
        Object => $object,
        MethodName => $method,
        Args => list(@_)
    );
}

=head2 Perl

  $class->new(Perl => $perl);

  perl($perl);

$perl - a string of Perl code.

Perl allows you to insert a piece of Perl code
as is, without have to parse it and turn it into a Code::Perl structure.

  perl("2 + 2")->perl # 2 + 2

=cut

sub perl
{
    return Code::Perl::Expr::Perl->new(
        Perl => shift,
    );
}

=head2 Holder

  $class->new(Expr => $expr);

  holder($expr);

$expr - an Expression object.

A Holder simply holds another
expression inside. A Holder is useful when you want "parameterise" an
expression. For example,

  $holder = holder();
  $exp = derefh(scal("hash"), derefa(scal("array", $holder));

  $holder->setExpr(number(10));
  print $exp->perl # $hash->{$array->[10]);

  $holder->setExpr(calls("getnum"));
  print $exp->perl # $hash->{$array->[getnum()]);

A single Holder can be placed in more than one position in your tree,
allowing you make the same expression appear in multiple places in your code
of you code and allowing you to change all of the occurrences at the same
time.

=cut

sub holder
{
    return Code::Perl::Expr::Holder->new(
        Expr => shift,
    );
}

1;

__END__

=head1 AUTHOR

Written by Fergal Daly <fergal@esatclear.ie>.

=head1 COPYRIGHT

Copyright 2003 by Fergal Daly E<lt>fergal@esatclear.ieE<gt>.

This program is free software and comes with no warranty. It is distributed
under the LGPL license

See the file F<LGPL> included in this distribution or
F<http://www.fsf.org/licenses/licenses.html>.

=cut

