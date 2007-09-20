use v6-alpha;

module COMPILER {

    our @PAD;
    our @CHECK;

    sub emit_perl6($node) {
        my $perl6 = $node.emit( $COMPILER::visitor_emit_perl6  );
        return $perl6;
    }

    sub env_init {
        my $pad = Pad.new();
        $pad.outer = undef;
        $pad.lexicals = [ ];
        $pad.namespace = 'Main';
        @COMPILER::PAD.unshift($pad);
        $List_COMPILER::PAD = @COMPILER::PAD;
    }

    sub add_pad($namespace) {
        my $pad = Pad.new();
        $pad.outer = @COMPILER::PAD[0];
        $pad.lexicals = [ ];
        $pad.namespace = $namespace;
        @COMPILER::PAD.unshift($pad);
    }

    sub drop_pad {
        @COMPILER::PAD.shift();
    }

    sub put_pad($pad) {
        @COMPILER::PAD.unshift($pad);
    }

    sub current_pad {
        return @COMPILER::PAD[0];
    }

    # this should vanish in the future
    sub begin_block($ast) {
        Pad::begin_block($ast);
    }

    sub check_block($ast) {
        # this routine saves check-blocks, in order to execute the code at the end of compilation
        my $pad = $COMPILER::PAD[0];
        #print "CHECK saved\n";
        push @COMPILER::CHECK, [ $ast, $pad ];
        return Val::Undef.new();
    }

    sub get_var($sigil, $twigil, $name) {
        # this routine is called each time a variable is parsed.
        # it checks for proper pre-declaration
        my $var = Var.new();
        $var.sigil = $sigil;
        $var.twigil = $twigil;
        $var.name = $name;
        my $pad = @COMPILER::PAD[0];
        #my $decl = $pad.declaration( $var );
        #print "COMPILER::get_var: @_ --> $decl\n";
        # TODO - annotate the variable with: Type, declarator
        return $var;
    }
}
