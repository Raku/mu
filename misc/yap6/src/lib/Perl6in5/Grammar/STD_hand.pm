package Perl6in5::Grammar;
use Perl6in5::Grammar;

my @compUnits    = qw{ eval PRE POST ENTER LEAVE KEEP UNDO FIRST 
                     LAST BEGIN END INIT CHECK UNITCHECK };
my @blkTypes     = qw{ sub method submethod regex token rule
                     macro module class package grammar};
my @bareFuncs    = qw{ use no say };

my @blkDecls  = qw{ module class grammar };

#   Rule Writing

# The identifier of each of your rules must begin with a lowercase letter,
# so that the source filter can transform/generate the grammar properly.

rule program {
        # everything must start with a use v6; statement until
        # the perl5zone rule is operational.
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

# this rule is directly right-recursive
rule stmtList {
        opt( stmtTrm ) - ( block . opt( blkTrm . opt( stmtList ) )
            | nbexpr . opt( stmtTrm - opt( stmtList ) ) )
};

rule blkTrm {
        hit( "\n" ) | stmtTrm
};

rule bareString {
        #w('""',star(unmore('"'))) | w("''",star(unmore("'") . identifier))
};

rule nbexpr {
        one(
            panic( pkgDecl, "Can't declare a non-block package")
            , func_say
            , op_numaddt
            , assign
            , declare
            , impor
            , blkTrait )
};

rule func_say {
        keyword('say') + expr
};

rule blkBare {
        w("{}",opt(stmtList))
};

rule block {
        opt(blkPrmbl) - blkBare
};

rule blkPrmbl {
        one(
            opt( blkModf++ )
            . ( opt(scpDecl++) . ( clype + blkType | blkType ) | blkType )
            - opt( opt( '^' ) . identifier )
            - opt( vsblty )
            - opt( w( '()', -( star( blkPrms ) ) ) )
            - star( blkTrait )
            , compUnit
            , flowCtrl
            , blkLabl
            , arrowInv . opt( -( ',' ) - star( blkPrms ) ) )
};

rule arrowInv {
        hit( '<-' ) - prmDecl
};

rule blkTrait {
        keywords( qw{ is does has } ) + clype
};

rule impor {
        keywords( qw{ no use require module class } ) + identifier . opt( +nbexpr )
};

rule flowCtrl {
        keywords( qw{ loop do while until } )
};

rule condBlk { # Until I die, I would cry unless unless/until were included.
        # if unless elsif else 
};

rule compUnit {
        keywords( @compUnits )
};

rule blkType {
        keywords( @blkTypes )
};

rule blkRetT {
        clype
};

rule clype {
        identifier  # just take any class/type name for now :)
        #  $Clype    # Class/Type
};

rule blkLabl {
        identifier . ':';
};

rule blkModf {
        keywords(qw{ multi proto only })
};

rule arg {
        'h'  #obviously this is just a stub.
};

# block parameter declaration
rule blkPrms {
        opt( invcDecl ) - plus( -( ',' ) - prmDecl )
};

# invocant declaration
rule invcDecl {
        prmDecl . ':'
};

rule prmDecl {
        sVari
        | clype + sVari
};

rule vsblty {
        keywords( qw{ public private } )
};

rule stmtTrm {
        plus( -( ';' ) )
};

rule scpDecl {
        keywords( qw{ my our } )
};

rule expr {
        block
        | nbexpr
};

rule declare {
        scpDecl + prmDecl
        | keywords( @blkDecls ) + clype
};

rule assign {
    #    hit('my $blah =') - block |
    #    (opt( keywords( qw{ my our } )++ ) . prmDecl - '=' - expr) |
        ( scpDecl | nothing ) - prmDecl - '=' - expr
};

# this will become some op prec lev (postfix)
rule op_numaddt {
        term - star( ( '+' | '-' ) - term )
};

# this will become some op prec lev (postfix)
rule term {
        factor - star( ( '*' | '/' ) - factor )
};

rule factor {
        base - opt( hit('**') - factor )
};

rule base {
        bareInt | sVari | w( '()', expr )
};

