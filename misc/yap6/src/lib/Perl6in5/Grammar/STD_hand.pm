package Perl6in5::Grammar;
use Perl6in5::Grammar;

my @compUnits    = qw{ eval PRE POST ENTER LEAVE KEEP UNDO FIRST 
                     LAST BEGIN END INIT CHECK UNITCHECK do };
my @blkTypes     = qw{ method submethod sub regex token rule
                     macro module class package grammar};
my @bareFuncs    = qw{ use no say };
my @blkDecls     = qw{ module class grammar role };
my @blkNumbers   = qw{ multi proto only };

pattern program {
        opt( usev6 - (stmtTrm | eoi) )
        - opt( pkgDecl )
        . opt( stmtList ) . eoi
};

pattern pkgDecl {
        keyword('package') + identifier - stmtTrm;
};

pattern usev6 {
        keyword('use') + lits(qw{ v6 Perl-6 })
        | keyword('module') + opt(keyword('Main'))
        | lits(qw{ class v6.0.0 v6 6 })
};

pattern identifier {
        match( qr|^([A-Za-z_]\w*)| )
};

pattern bareInt {
        match( qr|^(\d+)| )
};

#rule rx {
#        rx_prmbl . first( map( rx_delims_same( $_ ), qw{ `~!@#$%^&*() }
#};
#
#lrule rx_delims_same {
#        $_[1] . rx_subj . $_[1] . opt( rx_target . $_[1] )
#};

pattern sVari {
        lits( qw{ $^ $? $ @@ @ % & } ) . identifier
};

pattern commalist {
        plus( - ',' - nothing) . opt( $_[1] . opt( $_[0] ) )
};

pattern opt_chain_right_3 {
        $_[1] . opt( $_[2] . opt( $_[3] ) )
};

pattern stmtList {
        star( stmtTrm ) - ( opt_chain_right_3( block, blkTrm, stmtList ) ^ opt_chain_right_3( nbexpr, stmtTrm, stmtList ) ) - nothing
};

pattern stmtTrm {
        plus( - ';' )
};

pattern blkTrm {
        lit( "\n" ) . opt( stmtTrm ) ^ stmtTrm
};

pattern bareString {
        #w('""',star(unmore('"'))) | w("''",star(unmore("'") . identifier))
};

pattern nbexpr {
        one(
            panic( pkgDecl, "Can't declare a non-block package")
            , func_say
            , op_numaddt
            , declareAssign
            , impor
            , blkTrait )
};

pattern func_say {
        keyword('say') . opt( p6ws . expr )
};

pattern impor {
        keywords( qw{ no use require module class } ) + identifier . opt( p6ws . expr )
};

pattern block {
        iff( match( qr/^([^;}]+(?:\(.*\))?\s+{)/sm ) )
        . ( control_protasis_apodosis( keywords( qw{ if unless } ) )
            . star( - control_protasis_apodosis( lit('elsif') ) )
            . opt( - lit('else') - blkBare )
        ^ control_protasis_apodosis( keywords( qw{ while until } ) )
        ^ opt( blkPrmbl - nothing ) . blkBare
        ^ lit('try') + blkBare . opt( + lit('finally') + blkBare )
        ^ control_protasis_apodosis( lit('repeat') + keywords( qw{ while until } )
            . opt( p6ws . arrowCondResult ) )
        ^ control_apodosis( lit('repeat') . opt( p6ws . arrowCondResult ) )
            + keywords( qw{ while until } ) + ow( '()', expr )
        ^ control_apodosis( lit('loop') . opt( p6ws . w( '()', stmtList ) ) )
        ^ forBuilder( opt( '<' ) . lit('->') + blkPrms )
        ^ forBuilder( lit('->') + blkPrms( isRwAppendix ) ) )
};

pattern isRwAppendix {
        opt( p6ws . lit('is') + lit('rw') )
};

pattern forBuilder {
        control_apodosis( lit('for') + expr . opt( p6ws . $_[1] ) )
};

pattern control_protasis_apodosis {
        control_apodosis( $_[1] + ow( '()', expr ) )
};

pattern control_apodosis {
        $_[1] + blkBare
};

pattern blkBare {
        w( "{}", -( opt( stmtList ) ) )
};

pattern blkPrmbl {
        first(
            blkNumber
            . blkDeclarator
            . blkIdentifier
            . blkVisibility
            . blkPrmsGroup
            . blkTraits
            , compUnit
            , blkLabel
            , arrowDecl )
};

pattern blkNumber {
        opt( keywords( @blkNumbers ) . p6ws )
};

pattern blkDeclarator {
        # the fact that blkType must be out in front demonstrates that sometimes
        # rules that have potentially overlapping alternatives (clype) may both hit on the
        # longest token matcher, so we have to put the one(s) that should win, earlier.
        ( blkType ^ opt( opt( scpDecl . p6ws ) . clype . p6ws ) . blkType ) . p6ws
};

pattern scpDecl {
        keywords( qw{ my our } )
};

pattern blkType {
        keywords( @blkTypes )
};

pattern blkIdentifier {
        opt( opt( '^' ) . identifier . p6ws ) 
};

pattern blkVisibility {
        opt( vsblty . p6ws )
};

pattern blkPrmsGroup {
        opt( w( '()', - opt( invcDecl ) - nothing ) )
};

pattern invcDecl {
        prmDecl . ':' - opt( blkPrms ) ^ blkPrms
};

pattern prmDecl {
        opt( clype . p6ws ) . sVari
};

pattern blkPrms {
        my $appendix = $_[1] || nothing;
        prmDecl . $appendix . blkPrmsList( $appendix )
};

pattern blkPrmsList {
        my $appendix = $_[1] || nothing;
        opt( commalist( prmDecl . $appendix ) )
};

pattern blkTraits {
        star( - blkTrait . p6ws )
};

pattern blkTrait {
        keywords( qw{ is does has } ) + clype + sVari
};

pattern arrowDecl {
        arrowInv . blkPrmsList
};

pattern arrowInv {
        lit( '<-' ) - prmDecl
};

pattern arrowCondResult {
        lit( '->' ) - prmDecl
};

pattern blkRetT {
        clype
};

pattern clype {
        identifier  # just take any class/type name for now :)
        #  $Clype    # Class/Type
};

pattern blkLabel {
        identifier . ':';
};

pattern flowCtrl {
        keywords( qw{ do } )
};

pattern compUnit {
        keywords( @compUnits ) . worry( ':' - '{', "colons are for block labels, not special compilation units" )
};

pattern vsblty {
        keywords( qw{ public private } )
};

pattern expr {
        block
        ^ nbexpr
};

pattern declareAssign {
        opt( scpDecl . p6ws ) . prmDecl . opt( - '=' - expr )
        ^ keywords( @blkDecls ) + clype
};

pattern op_numaddt {
        term . star( -( '+' | '-' ) - term )
};

pattern term {
        factor . star( -( '*' | '/' ) - factor )
};

pattern factor {
        base . star( - lit('**') - base )
};

pattern base {
        bareInt ^ sVari ^ w( '()', stmtList )
};

    # N  Terms             42 3.14 'eek' "$eek" q/qq["foo"] $x heredoc :!verbose @$array circumfix(anything) self undef rand $magicals [semilist] ((lazy) array composer) { empty/pair/hash } \(capture items) &div:(Int, Int --> Int) $() @() %() &() @@() /abc/ rx:i[abc] s/foo/bar/ typenames func(*) :by(2) :!verbose :(Dog $self:) .meth/.=meth 4,3, sort 2,1 
    # L  Method postfix    .meth .+ .? .* .() .[] .{} .<> .«» .:: .= .^ .: !***
    # L  Autoincrement     ++ --
    # R  Exponentiation    **
    # L  Symbolic unary    ! + - ~ ? | +^ ~^ ?^ \ ^ =
    # L  Multiplicative    * / % +& +< +> ~& ~< ~> ?& div mod
    # L  Additive          + - +| +^ ~| ~^ ?| ?^
    # L  Replication       x xx
    # X  Concatenation     ~
    # X  Junctive and      &
    # X  Junctive or       | ^
    # L  Named unary       sleep abs sin
    # N  Nonchaining infix but does <=> leg cmp .. ..^ ^.. ^..^
    # C  Chaining infix    != == < <= > >= eq ne lt le gt ge ~~ === eqv !eqv
    # X  Tight and         &&
    # X  Tight or          || ^^ // min max
    # L  Conditional       ?? !! ff fff
    # R  Item assignment   = := ::= => += -= **= xx= .=
    # L  Loose unary       true not
    # X  Comma operator    , p5=>
    # X  List infix        Z minmax X X~X X*X XeqvX
    # R  List prefix       : print push say die map substr ... [+] [*] any $ @
    # X  Loose and         and andthen
    # X  Loose or          or xor orelse
    # N  Terminator        ; <==, ==>, <<==, ==>>, {...}, unless, extra ), ], }
