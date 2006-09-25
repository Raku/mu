
use Test::More tests => 6;
use Data::Dumper;
    local $Data::Dumper::Terse    = 1;
    local $Data::Dumper::Sortkeys = 1;

use_ok( 'Pugs::Grammar::Rule' );
use_ok( 'Pugs::Grammar::Python' );

{
  my $match = Pugs::Grammar::Python->statement( 'test ' );
  ok( $match ? 1 : 0, "statement" );
  is( $$match, 'test ', "match" );
}

{
  my $match = Pugs::Grammar::Python->line( '  test ' );
  ok( $match ? 1 : 0, "line" );
  # is( $$match, '  test ', "match" );
  print 'Line: ', Dumper( $$match );
}

{
  my $match = Pugs::Grammar::Python->parse( <<'___' );
    test1a
        test2a
            test3a1
            test3a2
        test2a2
    test1b
        test2b
            test3b
    test1c
___
  ok( $match ? 1 : 0, "parse" );
  # is( $$match, '  test ', "match" );
  print 'Parse: ', Dumper( $match->() );
}

