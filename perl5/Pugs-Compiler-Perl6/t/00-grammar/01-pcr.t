
use Test::More tests => 1;

use_ok( 'Pugs::Grammar::Rule' );
use_ok( 'Pugs::Grammar::BaseCategory' );

{
  my $match = Pugs::Grammar::BaseCategory->ws( ' ' );
  ok( $match ? 1 : 0, "<ws>" );
}

use_ok( 'Pugs::Grammar::Term' );

{
  my $match = Pugs::Grammar::Term->ident( 'abc' );
  ok( $match ? 1 : 0, "/abc/" );
}

{
  my $match = Pugs::Grammar::Term->cpan_bareword( 'abc-1.0' );
  is( "$match", "", "cpan_bareword" );
}
