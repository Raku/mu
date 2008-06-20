package Perl6in5::Grammar;
use Perl6in5::Grammar;

my @compUnits    = qw{ eval PRE POST ENTER LEAVE KEEP UNDO FIRST 
                     LAST BEGIN END INIT CHECK UNITCHECK };
my @blkTypes     = qw{ sub method submethod regex token rule
                     macro module class package grammar};
my @bareFuncs    = qw{ use no say };
my @blkDecls     = qw{ module class grammar };
my @blkNumbers   = qw{ multi proto only };

rule program {
        opt( usev6 - (stmtTrm | eoi) )
        - opt( pkgDecl )
        - opt( stmtList ) - eoi
};

rule pkgDecl {
        keyword('package') + identifier - stmtTrm;
};

rule usev6 {
        keyword('use') + keywords(qw{ v6 Perl-6 })
        | keyword('module') + opt(keyword('Main'))
        | keywords(qw{ class v6.0.0 v6 6 })
};

rule identifier {
        match( qr|^([A-Za-z_]\w*)| )
};

rule bareInt {
        match( qr|^(\d+)| )
};

rule sVari {
        '$' . identifier
};

lrule commalist {
        -( plus( ',' ) ) - opt( $_[1], $_[0] )
};

rule stmtTrm {
        plus( - ';' )
};

rule stmtList {
        star( stmtTrm ) - ( block . opt( blkTrm . opt( stmtList ) )
            | nbexpr . opt( stmtTrm - opt( stmtList ) ) ) - star( stmtTrm )
};

rule blkTrm {
        lit( "\n" ) ^ stmtTrm
};

rule bareString {
        #w('""',star(unmore('"'))) | w("''",star(unmore("'") . identifier))
};

rule nbexpr {
        one(
            panic( pkgDecl, "Can't declare a non-block package")
            , func_say
            , op_numaddt
            , declareAssign
            , impor
            , blkTrait )
};

rule func_say {
        keyword('say') . opt( +( expr ) )
};

rule block {
        opt( blkPrmbl ) . blkBare
};

rule blkBare {
        w( "{}", -( opt( stmtList ) ) )
};

rule blkPrmbl {
        one(
            blkNumber
            . blkDeclarator
            . blkIdentifier
            . blkVisibility
            . blkPrms
            . blkTraits
            , compUnit
            , flowCtrl
            , blkLabel
            , arrowDecl ) - nothing
};

rule blkNumber {
        opt( keywords( @blkNumbers ) . p6ws )
};

rule blkDeclarator {
        # the fact that blkType must be out in front demonstrates that sometimes
        # rules that have potentially overlapping alternatives (clype) may both hit on the
        # longest token matcher, so we have to put the one(s) that should win, earlier.
        ( blkType | opt( opt( scpDecl . p6ws ) . clype . p6ws ) . blkType ) . p6ws
};

rule scpDecl {
        keywords( qw{ my our } )
};

rule blkType {
        keywords( @blkTypes )
};

rule blkIdentifier {
        opt( opt( '^' ) . identifier . p6ws ) 
};

rule blkVisibility {
        opt( vsblty . p6ws )
};

rule blkPrms {
        opt( w( '()', opt( invcDecl ) . blkPrmsList ) )
};

rule blkPrmsList {
        opt( commalist( prmDecl ) )
};

rule blkTraits {
        star( - blkTrait . p6ws )
};

rule blkTrait {
        keywords( qw{ is does has } ) + clype + sVari
};

rule arrowDecl {
        arrowInv . blkPrmsList . opt( p6ws )
};

rule arrowInv {
        lit( '<-' ) - prmDecl
};

rule blkRetT {
        clype
};

rule clype {
        identifier  # just take any class/type name for now :)
        #  $Clype    # Class/Type
};

rule blkLabel {
        panic(compUnit . ':' - blkBare, "colons are for block labels, not special compilation units")
        | identifier . ':';
};

# invocant declaration
rule invcDecl {
        prmDecl . ':'
};

rule prmDecl {
        opt( clype . p6ws ) . sVari
};

rule impor {
        keywords( qw{ no use require module class } ) + identifier . opt( +nbexpr )
};

rule flowCtrl {
        keywords( qw{ do } )
};

rule condBlk { # Until I die, I would cry unless unless/until were included.
        # if unless elsif else 
};

rule compUnit {
        keywords( @compUnits )
};

rule vsblty {
        keywords( qw{ public private } )
};

rule expr {
        nbexpr
        | block
};

rule declareAssign {
        opt( scpDecl . p6ws ) . prmDecl . opt( - '=' - expr )
        ^ keywords( @blkDecls ) + clype
};

rule op_numaddt {
        term . star( -( '+' | '-' ) - term )
};

rule term {
        factor . star( -( '*' | '/' ) - factor )
};

rule factor {
        base . star( - lit('**') - base )
};

rule base {
        bareInt ^ sVari ^ w( '()', expr )
};

