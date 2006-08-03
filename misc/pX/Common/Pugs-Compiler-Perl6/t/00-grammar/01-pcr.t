
use Test::More tests => 19;
use Data::Dumper;

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
  is( "$match", "abc-1.0", "cpan_bareword" );
}

{
  my $match = Pugs::Grammar::Term->single_quoted( "'abc-1.0'", { p => 1 } );
  is( "" . $match->[0] , "abc-1.0", "single_quoted" );
}
{
  my $match = Pugs::Grammar::Term->double_quoted( '"abc-1.0"', { p => 1 } );
  is( "" . $match->[0] , "abc-1.0", "double_quoted" );
}
{
  my $match = Pugs::Grammar::Term->angle_quoted( "<abc-1.0>", { p => 1 } );
  is( "" . $match->[0] , "abc-1.0", "angle_quoted" );
}
{
  my $match = Pugs::Grammar::Term->rx_body( 
        "/abc-1.0/", 
        { p => 1, args => { open => '/' } },
  );
  # print Dumper $match->data;
  is( "" . $match->[0] , "abc-1.0", "rx_body" );
}
{
  my $match = Pugs::Grammar::Term->substitution( 
        "s/abc-1.0/abc-2.0/", 
        { p => 1, args => { open => '/' } },
  );
  # print Dumper $match->data;
  is( "" . $match->[0] , "abc-1.0", "substitution 0" );
  is( "" . $match->[1] , "abc-2.0", "substitution 1" );
}

use_ok( 'Pugs::Grammar::Perl6' );

{
  my $match = Pugs::Grammar::Perl6->perl6_expression( 
        '* 123 ;#', 
        { p => 1 }, );
  #print Dumper $match->data;
  ok( $match ? 1 : 0, "perl6_expression 123" );
}
{
  my $match = Pugs::Grammar::Perl6->perl6_expression( 
        '* 123 + 456 ;#', 
        { p => 1 }, );
  #print Dumper $match->data;
  ok( $match ? 1 : 0, "perl6_expression 123 + 456" );
  is( "" . $match, ' 123 + 456', "perl6_expression as string 123 + 456" );
  is( ref( $match ), 'Pugs::Runtime::Match', "is a Match object" );
  is( ref( $match->() ), 'HASH', "perl6_expression as capture object" );
  is( ref( $$match ), 'HASH', "perl6_expression as capture object" );
}
