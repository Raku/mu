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
