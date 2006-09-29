
use Test::More tests => 27;
use Data::Dumper;

use_ok( 'Pugs::Grammar::Rule' );
use_ok( 'Pugs::Grammar::BaseCategory' );

{
  my $match = Pugs::Grammar::BaseCategory->ws( ' ' );
  ok( $match ? 1 : 0, "<ws>" );
  is( $match->to, 1, ".to" );
}

use_ok( 'Pugs::Grammar::Term' );

{
  my $match = Pugs::Grammar::Term->ident( 'abc' );
  ok( $match ? 1 : 0, "/abc/" );
  is( $match->to, 3, ".to" );
}

{
  my $match = Pugs::Grammar::Term->cpan_bareword( 'abc-1.0' );
  is( "$match", "abc-1.0", "cpan_bareword" );
  is( $match->to, 7, ".to" );
}

use_ok( 'Pugs::Grammar::Quote' );

#{
#  my $match = Pugs::Grammar::Quote->single_quoted( "'abc-1.0'", { p => 1 } );
#  #print "match: ", Dumper( $match->data );
#  is( "" . $$match , "abc-1.0", "single_quoted" );
#  is( $match->to, 9, ".to" );
#}
{
  my $match = Pugs::Grammar::Quote->double_quoted( '"abc-1.0"', { p => 1 } );
  #print Dumper( $$match );
  is( "" . $$match->{double_quoted} , "abc-1.0", "double_quoted" );

  # test removed - behaviour changed
  # is( $match->to, 9, ".to" );
}
{
  my $match = Pugs::Grammar::Quote->angle_quoted( "<abc-1.0>", { p => 1 } );
  is( "" . $$match , "abc-1.0", "angle_quoted" );
  is( $match->to, 9, ".to" );
}
{
  my $match = Pugs::Grammar::Term->rx_body( 
        "/abc-1.0/", 
        { p => 1, args => { open => '/' } },
  );
  #print Dumper $match->data;
  is_deeply( $$match , { 'rx' => 'abc-1.0' }, "rx_body" );
  is( $match->to, 9, ".to" );
}
{
  my $match = Pugs::Grammar::Term->substitution( 
        "s/abc-1.0/abc-2.0/", 
        { p => 1, args => { open => '/' } },
  );
  #print Dumper $match->data;
  is_deeply( $$match , 
    {
      'options' => undef,
      'substitution' => [
        'abc-1.0',
        'abc-2.0'
      ],
    }, "substitution" );
  is( $match->to, 18, ".to" );
}

use_ok( 'Pugs::Grammar::Perl6' );

{
  no warnings 'once';
  my $match = $Pugs::Grammar::Term::hash{''}->match( 
        "abc", 
        { p => 0, },
  );
  #print Dumper $match->data;
  is_deeply( $$match , { 'bareword' => 'abc' }, "bareword" );
  is( $match->to, 3, ".to" );
}

{
  my $match = Pugs::Grammar::Expression->parse( 
        '* 123 ;#', 
        { p => 1 }, );
  #print Dumper $match->data;
  ok( $match ? 1 : 0, "perl6_expression 123" );
  is( $match->to, 5, ".to" );
}
{
  my $match = Pugs::Grammar::Expression->parse( 
        '* 123 + 456 ;#', 
        { p => 1 }, );
  #print Dumper $match->data;
  ok( $match ? 1 : 0, "perl6_expression 123 + 456" );
  # is( "" . $match, ' 123 + 456', "perl6_expression as string 123 + 456" );
  is( ref( $match ), 'Pugs::Runtime::Match', "is a Match object" );
  is( ref( $match->() ), 'HASH', "perl6_expression as capture object" );
  is( ref( $$match ), 'HASH', "perl6_expression as capture object" );
  is( $match->to, 11, ".to" );
}
