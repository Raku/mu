use v6-alpha;

use Test;

use QDRDBMS::AST <newLitBool newLitText newLitBlob newLitInt
    newSetSel newSeqSel newBagSel newQuasiSetSel newQuasiSeqSel
    newQuasiBagSel newEntityName newExprDict newTypeDict newVarInvo
    newFuncInvo newProcInvo newFuncReturn newProcReturn newFuncDecl
    newProcDecl newHostGateRtn>;

main();

######################################################################

sub main {

    plan( 45 ); # 17 less than Perl 5 version, which has 62

    say "#### Starting test of QDRDBMS::AST ####";

    simple_literals();

    say "#### Finished test of QDRDBMS::AST ####";
}

######################################################################

sub simple_literals {

    my ($in, $node, $out);

    # LitBool

    $in = undef;
    try {
        $node = newLitBool( :v($in) );
    };
    ok( $!, q{LitBool rejects invalid payload undef} );

    $in = Bool::False;
    $node = newLitBool( :v($in) );
    pass( q{LitBool accepts valid payload Bool:False} );
    isa_ok( $node, 'QDRDBMS::AST::LitBool' );
    $out = $node.v();
    is( $out, $in, q{LitBool preserves valid payload} );

    $in = Bool::True;
    $node = newLitBool( :v($in) );
    pass( q{LitBool accepts valid payload Bool:True} );
    isa_ok( $node, 'QDRDBMS::AST::LitBool' );
    $out = $node.v();
    is( $out, $in, q{LitBool preserves valid payload} );

    $in = 'foo';
    try {
        $node = newLitBool( :v($in) );
    };
    ok( $!, q{LitBool rejects invalid payload 'foo'} );

    $in = 42;
    try {
        $node = newLitBool( :v($in) );
    };
    ok( $!, q{LitBool rejects invalid payload 42} );

    # LitText

    $in = undef;
    try {
        $node = newLitText( :v($in) );
    };
    ok( $!, q{LitText rejects invalid payload undef} );

    $in = '';
    $node = newLitText( :v($in) );
    pass( q{LitText accepts valid payload ''} );
    isa_ok( $node, 'QDRDBMS::AST::LitText' );
    $out = $node.v();
    is( $out, $in, q{LitText preserves valid payload} );

    $in = 'Ceres';
    $node = newLitText( :v($in) );
    pass( q{LitText accepts valid payload ASCII 'Ceres'} );
    isa_ok( $node, 'QDRDBMS::AST::LitText' );
    $out = $node.v();
    is( $out, $in, q{LitText preserves valid payload} );

    $in = 'サンプル';
    $node = newLitText( :v($in) );
    pass( q{LitText accepts valid payload Unicode 'サンプル'} );
    isa_ok( $node, 'QDRDBMS::AST::LitText' );
    $out = $node.v();
    is( $out, $in, q{LitText preserves valid payload} );

    skip( 1, q{a newLitText test; Perl's Blob not implemented yet} );
    if 0 {

    $in = pack 'H2', '\xCC';
    try {
        $node = newLitText( :v($in) );
    };
    ok( $!, q{LitText rejects invalid payload pack 'H2', '\xCC'} );

    }

    # LitBlob

    skip( 11, q{all newLitBlob tests; Perl's Blob not implemented yet} );
    if 0 {

    $in = undef;
    try {
        $node = newLitBlob( :v($in) );
    };
    ok( $!, q{LitBlob rejects invalid payload undef} );

    $in = '';
    $node = newLitBlob( :v($in) );
    pass( q{LitBlob accepts valid payload ''} );
    isa_ok( $node, 'QDRDBMS::AST::LitBlob' );
    $out = $node.v();
    is( $out, $in, q{LitBlob preserves valid payload} );

    $in = 'Ceres';
    $node = newLitBlob( :v($in) );
    pass( q{LitBlob accepts valid payload ASCII 'Ceres'} );
    isa_ok( $node, 'QDRDBMS::AST::LitBlob' );
    $out = $node.v();
    is( $out, $in, q{LitBlob preserves valid payload} );

    $in = 'サンプル';
    try {
        $node = newLitBlob( :v($in) );
    };
    ok( $!, q{LitBlob rejects invalid payload Unicode 'サンプル'} );

    $in = pack 'H2', '\xCC';
    $node = newLitBlob( :v($in) );
    pass( q{LitBlob accepts valid payload pack 'H2', '\xCC'} );
    isa_ok( $node, 'QDRDBMS::AST::LitBlob' );
    $out = $node.v();
    is( $out, $in, q{LitBlob preserves valid payload} );

    }

    # LitInt

    $in = undef;
    try {
        $node = newLitInt( :v($in) );
    };
    ok( $!, q{LitInt rejects invalid payload undef} );

    $in = '';
    try {
        $node = newLitInt( :v($in) );
    };
    ok( $!, q{LitInt rejects invalid payload ''} );

    $in = 0;
    $node = newLitInt( :v($in) );
    pass( q{LitInt accepts valid payload 0} );
    isa_ok( $node, 'QDRDBMS::AST::LitInt' );
    $out = $node.v();
    is( $out, $in, q{LitInt preserves valid payload} );

    $in = '0';
    try {
        $node = newLitInt( :v($in) );
    };
    ok( $!, q{LitInt rejects invalid payload '0'} );

    $in = 42;
    $node = newLitInt( :v($in) );
    pass( q{LitInt accepts valid payload 42} );
    isa_ok( $node, 'QDRDBMS::AST::LitInt' );
    $out = $node.v();
    is( $out, $in, q{LitInt preserves valid payload} );

    $in = '42';
    try {
        $node = newLitInt( :v($in) );
    };
    ok( $!, q{LitInt rejects invalid payload '42'} );

    $in = -42;
    $node = newLitInt( :v($in) );
    pass( q{LitInt accepts valid payload -42} );
    isa_ok( $node, 'QDRDBMS::AST::LitInt' );
    $out = $node.v();
    is( $out, $in, q{LitInt preserves valid payload} );

    $in = 4.5;
    try {
        $node = newLitInt( :v($in) );
    };
    ok( $!, q{LitInt rejects invalid payload 4.5} );

    return;
}

######################################################################

1; # Magic true value required at end of a reuseable file's code.
