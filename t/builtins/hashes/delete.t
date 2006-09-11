use v6-alpha;
use Test;
plan 4;

# L<S29/"Hash"/"=item delete">

=pod

Test delete method of Spec Functions.

  our List  multi method Hash::delete ( *@keys )
  our Scalar multi method Hash::delete ( $key ) is default

  Deletes the elements specified by C<$key> or C<$keys> from the invocant.
  returns the value(s) that were associated to those keys.

=cut


sub gen_hash {
	my %h{'a'..'z'} = (1..26);
	return %h;
}

{
	my %h1 = gen_hash;
	my %h2 = gen_hash;

	my $b = %h1<b>;
	is delete(%h1, <b>), $b, "Test for delete singe key. (Indirect notation)";
	is %h2.delete(<b>), $b, "Test for delete singe key. (Method call)";

	my @cde = %h1<c d e>;
	is delete(%h1, <c d e>), @cde, "test for delete multiple keys. (Indirect notation)";
	is %h2.delete(<c d e>), @cde, "test for delete multiple keys. (method call)";
}
