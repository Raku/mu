module fp-0.0.1;

# Function composition
sub infix:<o> (Code &f, Code &g) { sub($x) { f g $x } }
sub infix:<∘> (Code &f, Code &g) { sub($x) { f g $x } }

# Haskell `...` metaoperator
sub infix:<`map`>  (Code &f, *@y) { map &f, @y }
sub infix:<`grep`> (Code &f, *@y) { grep &f, @y }

# Pair constructor
sub infix:<⇒> { $^x => $^y }

# Comparision ops
sub infix:<≥> { $^a >= $^b }
sub infix:<≤> { $^a <= $^b }
sub infix:<≠> { $^a != $^b }
sub infix:<≣> { $^a =:= $^b }
sub infix:<≡> { $^a =:= $^b }

# Misc. mathematical chars
sub prefix:<∑>  { [+] *@^nums }
sub prefix:<∏>  { [*] *@^nums }
sub postfix:<!> { [*] 1..$^x }
# sub ∞()       { Inf } -- doesn't work
sub infix:<÷>   { $^a / $^b }

# Standard functions of fp
sub id($x)        { $x }
sub const($x)     { return -> $y { $x } }
sub tail(@array)  { @array[1...] }
sub init(@array)  { @array[0..@array.end-1] }

sub replicate(Int $n, Code &f) { (1..$n).map:{ f() } }

=head1 NAME

fp - Functional programming for Perl 6

=head1 SYNOPSIS

  use fp;

  (&say o &int)(10/3);               # 3
  (&say ∘ &int)(10/3);               # 3
  { $_ % 2 == 0 } `grep` [1,2,3,4];  # [2,4]
  { $_ * 2 } `map` [1,2,3];          # [2,4,6]
  my $pair = key ⇒ "value";

=head1 DESCRIPTION

This is an experimental module which eases the use of functional programming
techniques in Perl 6.

=head1 OPERATORS

=head2 C<< infix:<∘> (Code &f, Code &g) >>

Function composition, think of Haskell's C<.>. There's also the ASCII
equivalent C<o>.

=head2 C<< infix:<`map`> (Code &f, *@y) >>

=head2 C<< infix:<`grep`> (Code &f, *@y) >>

Infix versions of C<map> and C<grep>.

These will go when we can define own metaoperators (like C<[...]> or C<»...«>).
Then, all functions can be "infixized".

=head2 C<< infix:<⇒> ($key, $value) >>

Pair constructor (equivalent to C<< => >>).

=head2 C<≥>, C<≤>, C<≠>

Standard comparision operators.

=head2 C<≣>, C<≡>

Equivalent to C<=:=>.

=head2 C<< prefix:<∑>(@nums) >>, C<< prefix:<∏>(@nums) >>

Sum and product.

=head2 C<< postfix:<!>(Int $x) >>

Factorial.

=head1 FUNCTIONS

=head2 C<< id($x) >>

The identity function.

=head2 C<< const($x) >>

Returns a new function which always returns C<$x>.

=head2 C<< tail(@array) >>

Returns all except the first element of C<@array>.

=head2 C<< init(@array) >>

Returns all except the last element of C<@array>.

=head2 C<< replicate(Int $n, Code &f) >>

Runs C<&f> C<$n> times and returns the results.

=head1 BUGS

This module is currently somewhat short, additions welcome! :)

=head1 AUTHOR

Ingo Blechschmidt E<lt>iblech@web.deE<gt>

=cut
