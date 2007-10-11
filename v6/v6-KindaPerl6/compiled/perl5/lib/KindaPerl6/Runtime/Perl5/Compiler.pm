package COMPILER;
use Data::Dumper;

# @COMPILER::CHECK  - CHECK blocks
# @COMPILER::PAD    - Pad structures

sub emit_perl6 {
    # param = AST
    my $perl6 = $_[0]->emit( );# $visitor_emit_perl6  );
    return $perl6;
}

sub env_init {
    @COMPILER::PAD = (Pad->new( 
        outer     => undef, 
        lexicals  => [ ],  
        namespace => 'Main',
    ));
    $List_COMPILER::PAD = \@COMPILER::PAD;   # for mp6 compatibility
}

sub add_pad {
    #print "add_pad\n";
    unshift @COMPILER::PAD, Pad->new( 
        outer     => $COMPILER::PAD[0], 
        lexicals  => [ ], 
        namespace => $_[0],  # optional
    ); 
}

sub drop_pad {
    #print "drop_pad\n";
    shift @COMPILER::PAD;
}
 
sub put_pad {
    #print "put_pad\n";
    unshift @COMPILER::PAD, $_[0];
}
 
sub current_pad {
    $COMPILER::PAD[0];
}
 
#    $PAD[0]->add_lexicals( [ $decl ] );
#    $PAD[0]->eval( $p5_source );

sub begin_block {
    Pad::begin_block( @_ );
}

sub check_block {
    # this routine saves check-blocks, in order to execute the code at the end of compilation
    
    my $ast = $_[0];
    my $pad = $COMPILER::PAD[0];
    #print "CHECK saved\n";
    push @COMPILER::CHECK, [ $ast, $pad ];
    return Val::Undef->new();
}

sub get_var {
    # this routine is called each time a variable is parsed.
    # it checks for proper pre-declaration
    my $var = shift;
    my $decl = $COMPILER::PAD[0]->declaration( $var );
    #print "COMPILER::get_var: @_ --> $decl\n";
    # TODO - annotate the variable with: Type, declarator
    return $var;
}

1;

