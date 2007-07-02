for (qw(
    
    KindaPerl6/Grammar.pm
    KindaPerl6/Traverse.pm
    
    KindaPerl6/Visitor/Hyper.pm
    KindaPerl6/Visitor/EmitPerl5.pm 
    KindaPerl6/Visitor/EmitPerl5Regex.pm 
    KindaPerl6/Visitor/Perl.pm
    KindaPerl6/Visitor/LexicalSub.pm
    KindaPerl6/Visitor/MetaClass.pm
    
    )) 
{
    print "Compiling $_\n";
    print `perl mp6.pl < lib/$_ > lib5/$_`;
}
