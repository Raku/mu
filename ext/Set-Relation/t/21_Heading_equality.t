use v6-alpha;

use Test;

plan( 12 );

use Set::Relation;

my Set::Relation::H $person_h1
    .= new( attrs => { 'name' => 'Str', 'age' => 'Int', } );
isa_ok( $person_h1, 'Set::Relation::H',
    q{new $person_h1 is a Set::Relation::H object} );
is_deeply( $person_h1.export_as_hash(),
    {
        'attrs' => {
            'name' => ("Scalar" => ::Str),
            'age'  => ("Scalar" => ::Int),
        },
    },
    q{$person_h1.export_as_hash() returns correct output},
);

my $person_h2 = heading( attrs => { 'name' => 'Str', 'age' => 'Int', } );
isa_ok( $person_h2, 'Set::Relation::H',
    q{new $person_h2 is a Set::Relation::H object} );
is_deeply( $person_h2.export_as_hash(),
    {
        'attrs' => {
            'name' => ("Scalar" => ::Str),
            'age'  => ("Scalar" => ::Int),
        },
    },
    q{$person_h2.export_as_hash() returns correct output},
);

my $person_h3 = heading( attrs => { 'name' => 'Str', 'age' => 'Num', } );
isa_ok( $person_h3, 'Set::Relation::H',
    q{new $person_h3 is a Set::Relation::H object} );
is_deeply( $person_h3.export_as_hash(),
    {
        'attrs' => {
            'name' => ("Scalar" => ::Str),
            'age'  => ("Scalar" => ::Num),
        },
    },
    q{$person_h3.export_as_hash() returns correct output},
);

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
