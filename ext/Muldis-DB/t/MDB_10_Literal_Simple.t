use v6-alpha;

use Test;

use Muldis::DB::Literal;

main();

###########################################################################

sub main {

    plan( 45 ); # 17 less than Perl 5 version, which has 62

    say "#### Starting test of Muldis::DB::Literal Simple ####";

    test_Literal_Bool();
    test_Literal_Order();
    test_Literal_Int();
    test_Literal_Blob();
    test_Literal_Text();

    say "#### Finished test of Muldis::DB::Literal Simple ####";

    return;
}

###########################################################################

sub test_Literal_Bool {

    my ($in, $node, $out);

    $in = undef;
    try {
        $node = ::Muldis::DB::Literal::Bool.new( :v($in) );
    };
    ok( $!, q{Literal::Bool rejects invalid payload undef} );

    $in = Bool::False;
    $node = ::Muldis::DB::Literal::Bool.new( :v($in) );
    pass( q{Literal::Bool accepts valid payload Bool:False} );
    isa_ok( $node, 'Muldis::DB::Literal::Bool' );
    $out = $node.v();
    is( $out, $in, q{Literal::Bool preserves valid payload} );

    $in = Bool::True;
    $node = ::Muldis::DB::Literal::Bool.new( :v($in) );
    pass( q{Literal::Bool accepts valid payload Bool:True} );
    isa_ok( $node, 'Muldis::DB::Literal::Bool' );
    $out = $node.v();
    is( $out, $in, q{Literal::Bool preserves valid payload} );

    $in = 'foo';
    try {
        $node = ::Muldis::DB::Literal::Bool.new( :v($in) );
    };
    ok( $!, q{Literal::Bool rejects invalid payload 'foo'} );

    $in = 42;
    try {
        $node = ::Muldis::DB::Literal::Bool.new( :v($in) );
    };
    ok( $!, q{Literal::Bool rejects invalid payload 42} );

    return;
}

###########################################################################

sub test_Literal_Order {
    return;
}

###########################################################################

sub test_Literal_Int {

    my ($in, $node, $out);

    $in = undef;
    try {
        $node = ::Muldis::DB::Literal::Int.new( :v($in) );
    };
    ok( $!, q{Literal::Int rejects invalid payload undef} );

    $in = '';
    try {
        $node = ::Muldis::DB::Literal::Int.new( :v($in) );
    };
    ok( $!, q{Literal::Int rejects invalid payload ''} );

    $in = 0;
    $node = ::Muldis::DB::Literal::Int.new( :v($in) );
    pass( q{Literal::Int accepts valid payload 0} );
    isa_ok( $node, 'Muldis::DB::Literal::Int' );
    $out = $node.v();
    is( $out, $in, q{Literal::Int preserves valid payload} );

    $in = '0';
    try {
        $node = ::Muldis::DB::Literal::Int.new( :v($in) );
    };
    ok( $!, q{Literal::Int rejects invalid payload '0'} );

    $in = 42;
    $node = ::Muldis::DB::Literal::Int.new( :v($in) );
    pass( q{Literal::Int accepts valid payload 42} );
    isa_ok( $node, 'Muldis::DB::Literal::Int' );
    $out = $node.v();
    is( $out, $in, q{Literal::Int preserves valid payload} );

    $in = '42';
    try {
        $node = ::Muldis::DB::Literal::Int.new( :v($in) );
    };
    ok( $!, q{Literal::Int rejects invalid payload '42'} );

    $in = -42;
    $node = ::Muldis::DB::Literal::Int.new( :v($in) );
    pass( q{Literal::Int accepts valid payload -42} );
    isa_ok( $node, 'Muldis::DB::Literal::Int' );
    $out = $node.v();
    is( $out, $in, q{Literal::Int preserves valid payload} );

    $in = 4.5;
    try {
        $node = ::Muldis::DB::Literal::Int.new( :v($in) );
    };
    ok( $!, q{Literal::Int rejects invalid payload 4.5} );

    return;
}

###########################################################################

sub test_Literal_Blob {

    my ($in, $node, $out);

    skip( 11, q{all newBlobLit tests; Perl's Blob not implemented yet} );
    if 0 {

    $in = undef;
    try {
        $node = ::Muldis::DB::Literal::Blob.new( :v($in) );
    };
    ok( $!, q{Literal::Blob rejects invalid payload undef} );

    $in = '';
    $node = ::Muldis::DB::Literal::Blob.new( :v($in) );
    pass( q{Literal::Blob accepts valid payload ''} );
    isa_ok( $node, 'Muldis::DB::Literal::Blob' );
    $out = $node.v();
    is( $out, $in, q{Literal::Blob preserves valid payload} );

    $in = 'Ceres';
    $node = ::Muldis::DB::Literal::Blob.new( :v($in) );
    pass( q{Literal::Blob accepts valid payload ASCII 'Ceres'} );
    isa_ok( $node, 'Muldis::DB::Literal::Blob' );
    $out = $node.v();
    is( $out, $in, q{Literal::Blob preserves valid payload} );

    $in = 'サンプル';
    try {
        $node = ::Muldis::DB::Literal::Blob.new( :v($in) );
    };
    ok( $!, q{Literal::Blob rejects invalid payload Unicode 'サンプル'} );

    $in = pack 'H2', '\xCC';
    $node = ::Muldis::DB::Literal::Blob.new( :v($in) );
    pass( q{Literal::Blob accepts valid payload pack 'H2', '\xCC'} );
    isa_ok( $node, 'Muldis::DB::Literal::Blob' );
    $out = $node.v();
    is( $out, $in, q{Literal::Blob preserves valid payload} );

    }

    return;
}

###########################################################################

sub test_Literal_Text {

    my ($in, $node, $out);

    $in = undef;
    try {
        $node = ::Muldis::DB::Literal::Text.new( :v($in) );
    };
    ok( $!, q{Literal::Text rejects invalid payload undef} );

    $in = '';
    $node = ::Muldis::DB::Literal::Text.new( :v($in) );
    pass( q{Literal::Text accepts valid payload ''} );
    isa_ok( $node, 'Muldis::DB::Literal::Text' );
    $out = $node.v();
    is( $out, $in, q{Literal::Text preserves valid payload} );

    $in = 'Ceres';
    $node = ::Muldis::DB::Literal::Text.new( :v($in) );
    pass( q{Literal::Text accepts valid payload ASCII 'Ceres'} );
    isa_ok( $node, 'Muldis::DB::Literal::Text' );
    $out = $node.v();
    is( $out, $in, q{Literal::Text preserves valid payload} );

    $in = 'サンプル';
    $node = ::Muldis::DB::Literal::Text.new( :v($in) );
    pass( q{Literal::Text accepts valid payload Unicode 'サンプル'} );
    isa_ok( $node, 'Muldis::DB::Literal::Text' );
    $out = $node.v();
    is( $out, $in, q{Literal::Text preserves valid payload} );

    skip( 1, q{a newTextLit test; Perl's Blob not implemented yet} );
    if 0 {

    $in = pack 'H2', '\xCC';
    try {
        $node = ::Muldis::DB::Literal::Text.new( :v($in) );
    };
    ok( $!, q{Literal::Text rejects invalid payload pack 'H2', '\xCC'} );

    }

    return;
}

###########################################################################
