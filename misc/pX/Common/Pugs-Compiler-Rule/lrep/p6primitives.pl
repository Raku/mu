# This is the Perl 6 Grammar used to Parse and generate the
# Abstract Syntax Tree (AST) for Perl 6 primitives - fglock
#
# This code is compiled and executed by using the previously
# compiled version, which is stored in "$filename-cached.pl"
#
# Things that go in this file are:
# - anything that contains a statement like:
#       eval( '...', :lang<perl5>);
# - anything that is too low-level to go into a real Module
# - anything that does not directly alters the Perl 6 Grammar
#   (these go into the p6prelude.pl file)

grammar grammar1;
        
macro statement_control:<if> () is parsed ( /
    <?ws>? \( 
        $expr := (.*?) 
    \) <?ws>? 
    $block := (<code>)
/ )
{
    return '
        sub prefix:<_if_expr>  { return $expr ; }
        sub prefix:<_if_block> { $block }
        eval( \' 
                if ( &{\\\'prefix:<_if_expr>\\\'}() ) { 
                    &{\\\'prefix:<_if_block>\\\'}() 
                } 
            \', 
            :lang<perl5> );
    ';
}

sub infix:<*> { eval(' $_[0] * $_[1] ', :lang<perl5>); }
sub infix:<+> { eval(' $_[0] + $_[1] ', :lang<perl5>); }
sub infix:<~> { eval(' $_[0] . $_[1] ', :lang<perl5>); }
