#!/usr/bin/pugs
use v6;
use Test;

=pod

=head1 List parameter test

These tests are the testing for "List paameters" section of Synopsis 06

L<<S06/"List parameters" /Slurpy parameters follow any required or optional parameters. They are marked by a * before the parameter:/>>

You might also be interested in the thread L<"Calling positionals by name in
presence of a slurpy hash" on p6l started by Ingo
Blechschmidt|http://www.nntp.perl.org/group/perl.perl6.language/22883>.

=cut

plan 27;

# Positional with slurpy *%hash and slurpy *@array
sub position_with_slurpy_hash1($n, *%hash, *@data) {
        return $n;
}
sub position_with_slurpy_hash2($n, *%hash, *@data) {
        %hash<test> + %hash<n> - %hash<a>;
}
sub position_with_slurpy_hash3($n, *%hash, *@data) {
        @data.sum;
}

## Synopsis says, all *remaining* pairs will be slurped into hash,
## So, For positional parameter, There might be no 'name' conflict'
ok( 'Testing with positional arguments' );
lives_ok { position_with_slurpy_hash1 100, a => 30, test => 40, 1, 3, 5, 7 },
  'Passing positional arguments without "name" conflict';
is ( position_with_slurpy_hash1 100, a => 30, test => 40, 1, 3, 5, 7 ), 100,
  'Testing the positional value';

lives_ok { position_with_slurpy_hash1 100, n => 30, test => 40, 1, 3, 5, 7 },
  'Passing positional arguments with "name" conflict';
is ( position_with_slurpy_hash1 100, n => 30, test => 40, 1, 3, 5, 7 ), 30,
  'Testing the value for positional';
is ( position_with_slurpy_hash2 100, n => 30, test => 40, 1, 3, 5, 7 ), 40,
  'Testing the value for slurpy *%hash';

lives_ok { position_with_slurpy_hash1 100, a => 30, test => 40, 1, 3, 5, 7 },
  'Passing positional arguments without "name" conflict';
is ( position_with_slurpy_hash2 100, a => 30, test => 40, 1, 3, 5, 7 ), 10,
  'Testing the value for slurpy *%hash';
is ( position_with_slurpy_hash3 100, a => 30, test => 40, 1, 3, 5, 7 ), 16,
  'Testing the value for slurpy *@array';

## We *can* pass positional arguments as a 'named' pair with slurpy *%hash.
## Only *remaining* pairs are slurped into the slurpy *%hash
diag( 'Testing without positional arguments' );
lives_ok { position_with_slurpy_hash2 n => 100, test => 40, 1, 3, 5, 7 },
  'Passing positional arguments using named pair form with slurpy *%hash.';
is ( position_with_slurpy_hash2 n => 100, test => 40, 1, 3, 5, 7 ), 40,
  'Testing the value for slurpy *%hash';


# named with slurpy *%hash and slurpy *@array
## Named arguments aren't required in tests below
sub named_with_slurpy_hash1( +$n, *%hash, *@data ) {
        return $n;
}
sub named_with_slurpy_hash2( +$n, *%hash, *@data ) {
        %hash<test> + %hash<n> - %hash<a>;
}
sub named_with_slurpy_hash3( +$n, *%hash, *@data ) {
        return @data.sum;
}

diag( 'Testing with named arguments (not required named param)' );
lives_ok { named_with_slurpy_hash1 100, a => 30, test => 40, 1, 3, 5, 7 },
  'Passing named arguments ( Not required ) without "name" conflict';
### named_with_slurpy_hash1 will always get undef ( maybe an ERROR? ) in this example,
lives_ok { named_with_slurpy_hash1 100, n => 30, test => 40, 1, 3, 5, 7 },
  'Passing named arguments ( Not required ) with "name" conflict';
# Or error?
#dies_ok { named_with_slurpy_hash1 100, n => 30, test => 40, 1, 3, 5, 7 },
#  'Passing named arguments ( Not required ) with "name" conflict';
is ( named_with_slurpy_hash1 100, n => 30, test => 40, 1, 3, 5, 7 ), 30,
  'Testing the named argument';

lives_ok { named_with_slurpy_hash1 100, n => 30, test => 40, 1, 3, 5, 7 },
  'Passing named arguments ( Not required ) with "name" conflict';
is ( named_with_slurpy_hash2 100, n => 30, test => 40, a => 10, 1, 3, 5, 7 ), 30,
  'Testing value for slurpy *%hash';
is ( named_with_slurpy_hash3 100, n => 30, test => 40, a => 10, 1, 3, 5, 7 ), 116,
  'Testing the value for slurpy *@array';

lives_ok { named_with_slurpy_hash1 100, a => 30, test => 40, 1, 3, 5, 7 },
  'Passing named arguments ( Not required ) without "name" conflict';
is ( named_with_slurpy_hash2 100, a => 30, test => 40, 1, 3, 5, 7 ), 10,
  'Testing value for slurpy *%hash';
is ( named_with_slurpy_hash3 100, a => 30, test => 40, 1, 3, 5, 7 ), 116,
  'Testing the value for slurpy *@array';

# named with slurpy *%hash and slurpy *@array
## Named arguments **ARE** required in tests below

#### ++ version
sub named_required_with_slurpy_hash_plus( ++$n, *%hash, *@data ) {
        return $n;
}

diag( 'Testing with named arguments (required named param)' );
dies_ok { named_required_with_slurpy_hash_plus 100, a => 30, test => 40, 1, 3, 5, 7 },
  'Try named arguments ( required ) without "name" conflict (++ version)';
lives_ok { named_required_with_slurpy_hash_plus 100, n => 30, test => 40, 1, 3, 5, 7 },
  'Try named arguments ( required ) with "name" conflict (++ version)';

#### "trait" version
sub named_required_with_slurpy_hash_trait( +$n is required, *%hash, *@data ) {
        return $n;
}
diag( 'Testing with named arguments (required named param)' );
dies_ok { named_required_with_slurpy_hash_trait 100, a => 30, test => 40, 1, 3, 5, 7 },
  'Try named arguments ( required ) without "name" conflict (trait version)';
dies_ok { named_required_with_slurpy_hash_trait 100, n => 30, test => 40, 1, 3, 5, 7 },
  'Try named arguments ( required ) with "name" conflict (trait version)';


##### Now slurpy scalar tests here.
=kwid

=head1 List parameter test

These tests are the testing for "List paameters" section of Synopsis 06

L<<S06/"List parameters" /Slurpy scalar parameters capture what would otherwise be the first elements of the variadic array:/>>

=cut

sub first(*$f, *$s, *@r) { return $f };
sub second(*$f, *$s, *@r) { return $s };
sub rest(*$f, *$s, *@r) { return @r.sum };
diag 'Testing with slurpy scalar';
is first(1, 2, 3, 4, 5), 1,
  'Testing the first slurpy scalar...';
is second(1, 2, 3, 4, 5), 2,
  'Testing the second slurpy scalar...';
is rest(1, 2, 3, 4, 5), 12,
  'Testing the rest slurpy *@ary';

