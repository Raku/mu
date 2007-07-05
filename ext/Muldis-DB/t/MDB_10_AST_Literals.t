use v6-alpha;

use Test;

use Muldis::DB::AST
    <newBoolLit newOrderLit newIntLit newBlobLit newTextLit>;

main();

######################################################################

sub main {

    plan( 45 ); # 17 less than Perl 5 version, which has 62

    say "#### Starting test of Muldis::DB::AST Literals ####";

    test_BoolLit();
    test_OrderLit();
    test_IntLit();
    test_BlobLit();
    test_TextLit();

    say "#### Finished test of Muldis::DB::AST Literals ####";

    return;
}

######################################################################

sub test_BoolLit {

    my ($in, $node, $out);

    $in = undef;
    try {
        $node = newBoolLit( :v($in) );
    };
    ok( $!, q{BoolLit rejects invalid payload undef} );

    $in = Bool::False;
    $node = newBoolLit( :v($in) );
    pass( q{BoolLit accepts valid payload Bool:False} );
    isa_ok( $node, 'Muldis::DB::AST::BoolLit' );
    $out = $node.v();
    is( $out, $in, q{BoolLit preserves valid payload} );

    $in = Bool::True;
    $node = newBoolLit( :v($in) );
    pass( q{BoolLit accepts valid payload Bool:True} );
    isa_ok( $node, 'Muldis::DB::AST::BoolLit' );
    $out = $node.v();
    is( $out, $in, q{BoolLit preserves valid payload} );

    $in = 'foo';
    try {
        $node = newBoolLit( :v($in) );
    };
    ok( $!, q{BoolLit rejects invalid payload 'foo'} );

    $in = 42;
    try {
        $node = newBoolLit( :v($in) );
    };
    ok( $!, q{BoolLit rejects invalid payload 42} );

    return;
}

######################################################################

sub test_OrderLit {
    return;
}

######################################################################

sub test_IntLit {

    my ($in, $node, $out);

    $in = undef;
    try {
        $node = newIntLit( :v($in) );
    };
    ok( $!, q{IntLit rejects invalid payload undef} );

    $in = '';
    try {
        $node = newIntLit( :v($in) );
    };
    ok( $!, q{IntLit rejects invalid payload ''} );

    $in = 0;
    $node = newIntLit( :v($in) );
    pass( q{IntLit accepts valid payload 0} );
    isa_ok( $node, 'Muldis::DB::AST::IntLit' );
    $out = $node.v();
    is( $out, $in, q{IntLit preserves valid payload} );

    $in = '0';
    try {
        $node = newIntLit( :v($in) );
    };
    ok( $!, q{IntLit rejects invalid payload '0'} );

    $in = 42;
    $node = newIntLit( :v($in) );
    pass( q{IntLit accepts valid payload 42} );
    isa_ok( $node, 'Muldis::DB::AST::IntLit' );
    $out = $node.v();
    is( $out, $in, q{IntLit preserves valid payload} );

    $in = '42';
    try {
        $node = newIntLit( :v($in) );
    };
    ok( $!, q{IntLit rejects invalid payload '42'} );

    $in = -42;
    $node = newIntLit( :v($in) );
    pass( q{IntLit accepts valid payload -42} );
    isa_ok( $node, 'Muldis::DB::AST::IntLit' );
    $out = $node.v();
    is( $out, $in, q{IntLit preserves valid payload} );

    $in = 4.5;
    try {
        $node = newIntLit( :v($in) );
    };
    ok( $!, q{IntLit rejects invalid payload 4.5} );

    return;
}

######################################################################

sub test_BlobLit {

    my ($in, $node, $out);

    skip( 11, q{all newBlobLit tests; Perl's Blob not implemented yet} );
    if 0 {

    $in = undef;
    try {
        $node = newBlobLit( :v($in) );
    };
    ok( $!, q{BlobLit rejects invalid payload undef} );

    $in = '';
    $node = newBlobLit( :v($in) );
    pass( q{BlobLit accepts valid payload ''} );
    isa_ok( $node, 'Muldis::DB::AST::BlobLit' );
    $out = $node.v();
    is( $out, $in, q{BlobLit preserves valid payload} );

    $in = 'Ceres';
    $node = newBlobLit( :v($in) );
    pass( q{BlobLit accepts valid payload ASCII 'Ceres'} );
    isa_ok( $node, 'Muldis::DB::AST::BlobLit' );
    $out = $node.v();
    is( $out, $in, q{BlobLit preserves valid payload} );

    $in = 'サンプル';
    try {
        $node = newBlobLit( :v($in) );
    };
    ok( $!, q{BlobLit rejects invalid payload Unicode 'サンプル'} );

    $in = pack 'H2', '\xCC';
    $node = newBlobLit( :v($in) );
    pass( q{BlobLit accepts valid payload pack 'H2', '\xCC'} );
    isa_ok( $node, 'Muldis::DB::AST::BlobLit' );
    $out = $node.v();
    is( $out, $in, q{BlobLit preserves valid payload} );

    }

    return;
}

######################################################################

sub test_TextLit {

    my ($in, $node, $out);

    $in = undef;
    try {
        $node = newTextLit( :v($in) );
    };
    ok( $!, q{TextLit rejects invalid payload undef} );

    $in = '';
    $node = newTextLit( :v($in) );
    pass( q{TextLit accepts valid payload ''} );
    isa_ok( $node, 'Muldis::DB::AST::TextLit' );
    $out = $node.v();
    is( $out, $in, q{TextLit preserves valid payload} );

    $in = 'Ceres';
    $node = newTextLit( :v($in) );
    pass( q{TextLit accepts valid payload ASCII 'Ceres'} );
    isa_ok( $node, 'Muldis::DB::AST::TextLit' );
    $out = $node.v();
    is( $out, $in, q{TextLit preserves valid payload} );

    $in = 'サンプル';
    $node = newTextLit( :v($in) );
    pass( q{TextLit accepts valid payload Unicode 'サンプル'} );
    isa_ok( $node, 'Muldis::DB::AST::TextLit' );
    $out = $node.v();
    is( $out, $in, q{TextLit preserves valid payload} );

    skip( 1, q{a newTextLit test; Perl's Blob not implemented yet} );
    if 0 {

    $in = pack 'H2', '\xCC';
    try {
        $node = newTextLit( :v($in) );
    };
    ok( $!, q{TextLit rejects invalid payload pack 'H2', '\xCC'} );

    }

    return;
}

######################################################################
