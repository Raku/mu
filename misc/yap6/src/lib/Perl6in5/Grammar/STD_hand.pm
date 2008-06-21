package Perl6in5::Grammar;
use Perl6in5::Grammar;

my @compUnits    = qw{ eval PRE POST ENTER LEAVE KEEP UNDO FIRST 
                     LAST BEGIN END INIT CHECK UNITCHECK do };
my @blkTypes     = qw{ method submethod sub regex token rule
                     macro module class package grammar};
my @bareFuncs    = qw{ use no say };
my @blkDecls     = qw{ module class grammar role };
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
        keyword('use') + lits(qw{ v6 Perl-6 })
        | keyword('module') + opt(keyword('Main'))
        | lits(qw{ class v6.0.0 v6 6 })
};

rule identifier {
        match( qr|^([A-Za-z_]\w*)| )
};

rule bareInt {
        match( qr|^(\d+)| )
};

#rule rx {
#        rx_prmbl . first( map( rx_delims_same( $_ ), qw{ `~!@#$%^&*() }
#};
#
#lrule rx_delims_same {
#        $_[1] . rx_subj . $_[1] . opt( rx_target . $_[1] )
#};

rule sVari {
        lits( qw{ $^ $? $ @@ @ % & } ) . identifier
};

rule commalist {
        plus( - ',' - nothing) . opt( $_[1] . opt( $_[0] ) )
};

rule opt_chain_right_3 {
        $_[1] . opt( $_[2] . opt( $_[3] ) )
};

rule stmtList {
        star( stmtTrm ) - ( opt_chain_right_3( block, blkTrm, $_[0] ) ^ opt_chain_right_3( nbexpr, stmtTrm, $_[0] ) ) - nothing
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
        keywords( qw{ if unless } ) + ow( '()', - expr - nothing ) - blkBare
        . star( - lit('elsif') + ow( '()', - expr - nothing ) - blkBare )
        . opt( - lit('else') - blkBare )
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
