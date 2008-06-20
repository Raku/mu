package Perl6in5::Grammar;
use Perl6in5::Grammar;

my @compUnits    = qw{ eval PRE POST ENTER LEAVE KEEP UNDO FIRST 
                     LAST BEGIN END INIT CHECK UNITCHECK do };
my @blkTypes     = qw{ method submethod sub regex token rule
                     macro module class package grammar};
my @bareFuncs    = qw{ use no say };
my @blkDecls     = qw{ module class grammar };
my @blkNumbers   = qw{ multi proto only };

rule program {
        opt( usev6 - (stmtTrm | eoi) )
        - opt( pkgDecl )
        . opt( stmtList ) . eoi
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
        plus( - ',' - nothing) . opt( $_[1] . opt( $_[0] ) )
};

rule stmtList {
        star( stmtTrm ) - ( block . opt( blkTrm . opt( stmtList ) )
            ^ nbexpr . opt( stmtTrm - opt( stmtList ) ) ) - nothing
};

rule stmtTrm {
        plus( - ';' )
};

rule blkTrm {
        lit( "\n" ) . opt( stmtTrm ) ^ stmtTrm
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
        keyword('say') . opt( p6ws . expr )
};

rule impor {
        keywords( qw{ no use require module class } ) + identifier . opt( p6ws . expr )
};

rule block {
        keywords( qw{ if unless } ) + ow( '()', - expr - nothing ) - blkBare . star( - lit('elsif') + ow( '()', - expr - nothing ) - blkBare ) . opt( - lit('else') - blkBare )
        ^ keywords( qw{ while until } ) + ow( '()', - expr - nothing ) - blkBare
        ^ opt( blkPrmbl - nothing ) . blkBare
        ^ lit('try') - blkBare . opt( - lit('finally') - blkBare )
};

rule blkBare {
        w( "{}", -( opt( stmtList ) ) )
};

rule blkPrmbl {
        first(
            blkNumber
            . blkDeclarator
            . blkIdentifier
            . blkVisibility
            . blkPrms
            . blkTraits
            , compUnit
            , blkLabel
            , arrowDecl )
};

rule blkNumber {
        opt( keywords( @blkNumbers ) . p6ws )
};

rule blkDeclarator {
        # the fact that blkType must be out in front demonstrates that sometimes
        # rules that have potentially overlapping alternatives (clype) may both hit on the
        # longest token matcher, so we have to put the one(s) that should win, earlier.
        ( blkType ^ opt( opt( scpDecl . p6ws ) . clype . p6ws ) . blkType ) . p6ws
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
        opt( w( '()', - opt( invcDecl ) - nothing ) )
};

rule invcDecl {
        prmDecl . ':' - opt( prmDecl . blkPrmsList ) ^ prmDecl . blkPrmsList
};

rule prmDecl {
        opt( clype . p6ws ) . sVari
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
        arrowInv . blkPrmsList
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
        identifier . ':';
};

rule flowCtrl {
        keywords( qw{ do } )
};

rule compUnit {
        keywords( @compUnits ) . worry( ':' - '{', "colons are for block labels, not special compilation units" )
};

rule vsblty {
        keywords( qw{ public private } )
};

rule expr {
        block
        ^ nbexpr
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

