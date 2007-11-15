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
    
    Resolved    to      [Plug]

Activity   Acolhimento2

    Name    
    
        Procedimento de Acolhimento2
        
    Description
    
        Parte do Processo de Contratação2

    New         from    Process Contratação
     
    Pending     on      Process CompraPortatil

END


my $m = Language::Toto::Grammar->TOP( $toto );
print $m->yaml;
print "Matched: [ ",$m," ]\n";

my @blocks = @{ $m->{block} };
print scalar(@blocks), " blocks \n";
extract_block($_) for @blocks;

}

sub extract_block {
    my $m = shift;
    my $type = ${ $m->{block_header}{block_type} };
    print "BLOCK type = $type \n";
    my $ident = ${ $m->{block_header}{ident} };
    print " ident = $ident \n";
    my $name = join( "\n", @{ $m->{block_name}{text} } );
    print " name = $name \n";
    my $description = join( "\n", @{ $m->{block_description}{text} } );
    print " description = $description \n";
    my @transition = @{ $m->{block_transition} };
    print " ",scalar( @transition ), " transitions \n";
    extract_transition($_) for @transition;
}

sub extract_transition {
    my $m = shift;
    my $type = ${ $m->{type} };
    print " TRANSITION type = $type \n";
    if ( exists $m->{plugin} ) {
        print "  PLUGIN\n";
        my $plugin = ${ $m->{plugin} };
        print "  plugin = $plugin \n";
    }
    elsif ( exists $m->{action} ) {
        my $action = ${ $m->{action} };
        print "  action = $action \n";
        my $ident = ${ $m->{ident} };
        print "  ident = $ident \n";
    }
    print "\n";
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