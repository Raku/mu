{
use Language::Toto::Grammar;

print "ok\n";

my $toto = <<END;
Procedure   Acolhimento

    Name    
    
        Procedimento de Acolhimento
        
    Description
    
        Parte do Processo de Contratação

    New         from    Process Contratação
     
    Pending     on      Process CompraPortatil
    Pending     on      Procedure CompraTelemovel
    Pending     on      Procedure ConfiguraRede

END


my $m = Language::Toto::Grammar->block( $toto );
print $m->yaml;
print "Matched: [ ",$m," ]\n";
}

__END__

{
use Language::Python::Grammar;
use Data::Dumper;

print "ok\n";

my $toto = <<END;
Procedure   Acolhimento

    Name    
    
        Procedimento de Acolhimento
        
    Description
    
        Parte do Processo de Contratação

    New         from    Process Contratação
     
    Pending     on      Process CompraPortatil
    Pending     on      Procedure CompraTelemovel
    Pending     on      Procedure ConfiguraRede

END

my $m = Language::Python::Grammar->parse( $toto );
print Dumper $$m;
}