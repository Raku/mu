use v6-alpha;

use Test;

plan( 6 );

use Set::Relation;

my Set::Relation::H $person_h1
    .= new( attrs => { 'name' => 'Str', 'age' => 'Int', } );

my $person_h2 = heading( attrs => { 'name' => 'Str', 'age' => 'Int', } );

my $person_h3 = heading( attrs => { 'name' => 'Str', 'age' => 'Num', } );

is( $person_h1.is_equal( $person_h1 ), Bool::True,
    q{$person_h1.is_equal( $person_h1 returns Bool::True} );
is( $person_h1 === $person_h1, Bool::True,
    q{$person_h1 === $person_h1 returns Bool::True} );

is( $person_h2.is_equal( $person_h1 ), Bool::True,
    q{$person_h2.is_equal( $person_h1 returns Bool::True} );
is( $person_h2 === $person_h1, Bool::True,
    q{$person_h2 === $person_h1 returns Bool::True} );

is( $person_h3.is_equal( $person_h1 ), Bool::False,
    q{$person_h3.is_equal( $person_h1 returns Bool::False} );
is( $person_h3 === $person_h1, Bool::False,
    q{$person_h3 === $person_h1 returns Bool::False} );
