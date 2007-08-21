package COMPILER;
use Data::Dumper;

use KindaPerl6::Runtime::Perl6::Hash;   # Hash is 'required' too late in Runtime.pm ???

use KindaPerl6::Visitor::Perl;
use KindaPerl6::Visitor::EmitPerl5;
use KindaPerl6::Visitor::EmitPerl6;
#use KindaPerl6::Visitor::Subset;
use KindaPerl6::Visitor::MetaClass;
use KindaPerl6::Visitor::Token;
use KindaPerl6::Visitor::Global;

my $visitor_dump_ast    = KindaPerl6::Visitor::Perl->new();
my $visitor_emit_perl5  = KindaPerl6::Visitor::EmitPerl5->new();
my $visitor_emit_perl6  = KindaPerl6::Visitor::EmitPerl6->new();
#my $visitor_subset      = KindaPerl6::Visitor::Subset->new();
my $visitor_metamodel   = KindaPerl6::Visitor::MetaClass->new();
my $visitor_token       = KindaPerl6::Visitor::Token->new();
my $visitor_global      = KindaPerl6::Visitor::Global->new();

# @COMPILER::CHECK  - CHECK blocks
# @COMPILER::PAD    - Pad structures

sub emit_perl6 {
    # param = AST
    my $perl6 = $_[0]->emit( $visitor_emit_perl6  );
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
 
#    $PAD[0]->add_lexicals( [ $decl ] );
#    $PAD[0]->eval( $p5_source );

sub begin_block {
    # this routine is called by begin-blocks at compile time, in order to execute the code
    # Input: '::Lit::Code' AST node
    
    #print "begin_block\n";
    #print "PARAM: ",Dumper(\@_);
    #my $env = shift;
    my $ast = shift;

    #print Dump( $ast );
    $ast = $ast->emit( $visitor_token );
    #$ast = $ast->emit( $visitor_lexical_sub );
    #$ast = $ast->emit( $visitor_subset );
    $ast = $ast->emit( $visitor_metamodel );
    #$ast = $ast->emit( $visitor_create_env );
    
    # XXX - initialization belongs to $visitor_global
    $visitor_global->pad( [ $COMPILER::PAD[0] ] );
    $ast = $ast->emit( $visitor_global );
    shift @{ $visitor_global->pad };
    
    #print Dump( $ast );
    #Main::say( $ast->emit( $visitor_dump_ast    ));
    my $native = $ast->emit( $visitor_emit_perl5  );
    #print "Native: $native\n";

    # execute the native code inside the current pad
    add_pad;
    my $data = $COMPILER::PAD[0]->eval( $native );  # XXX - want() context
    drop_pad;
    die "At BEGIN: " . $@ . "\n  Native code: $native"if $@;
    #print "RETURN DATA: ", Dumper($data);

    # check for side-effects
    my @begin_stmts;

    #print "=pod\n";
    #print "# BEGIN ENV: ", Dumper( $COMPILER::PAD[0]->lexicals ), "\n";
    #print "BEGIN AST: ", Dumper( \$ast );
    #print "BEGIN: Native code: $native\n\n";

    for my $pad ( @COMPILER::PAD ) {
        #print "# Lexicals here: ", Dumper( $pad->lexicals ), "\n";
        my $side_effects = $pad->eval( '$_MODIFIED' ); 
        #print "MODIFIED: ", Dumper( $side_effects );
        # TODO - emit side-effects...
        my @names = keys %$side_effects;
        for my $name ( @names ) {
            my $value = $COMPILER::PAD[0]->eval( "$name" );
            #print "# modified: $name = ",Dumper( $value );
            #print "# modified: $name = ",$value->{_value}{name},"\n";

            my $src = '';
            if ( $name ne $value->{_value}{name} ) {
                # it seems to be a bound variable
                if ( $value->{_value}{name} ) {
                    # the binded thing has a name
                    $src = $src . "$name := " . $value->{_value}{name} . '; ';
                    # optimize repeated assignments
                    $src = $src . "$name = " . $value->{_dispatch}( $value, 'perl' )->{_value};
                }
                else {
                    # no name; bind to the value
                    $src = $src . "$name := " . $value->{_dispatch}( $value, 'perl' )->{_value} . '; ';
                }
            }
            else {
                # plain assignment
                $src = $src . "$name = " . $value->{_dispatch}( $value, 'perl' )->{_value};
            }

            # TODO - convert directly DATA->AST, instead of DATA->PERL->AST
            #print "# BEGIN SIDE-EFFECT: $src \n\n";
            my $p = KindaPerl6::Grammar->exp_stmts( $src, 0);
            my $pos = $p->to;
            #print "# parsed to $pos - length = ",length($src)," [$src]\n";
            if ( $pos != length( $src ) ) {
                die "Syntax error serializing BEGIN block, after position $pos in: $src\n";
            }
            #print "AST: ", Dumper($$p);
            # TODO - check for shared data (BIND)
            push @begin_stmts, @$$p;
        }
    }
    add_pad;
    my $begin_ast = BEGIN->new(
        block => Lit::Code->new(
            sig   => Sig->new(
                                 'named' => {},
                                 'invocant' => undef,
                                 'positional' => []
                             ),
            body  => \@begin_stmts,
            pad   => $COMPILER::PAD[0], 
            state => {},
        ),
    );
    drop_pad;
    #print "BEGIN AST: ",Dumper($begin_ast);
    #print "BEGIN native: ", $begin_ast->emit( $visitor_emit_perl5  ) );
    #print "data: ", Dumper( $data );
    
    # - convert the 'result' data to ast
    my $source = $data->{_dispatch}( $data, 'perl' )->{_value};
    #print "# begin - result data: $source\n";
    my $p = KindaPerl6::Grammar->exp($source, 0);
    #say( Main::perl( $$p ) );
    add_pad;
    my $final_ast = Do->new(
            'block' => Lit::Code->new(
                pad   => $COMPILER::PAD[0],
                state => { },
                sig   => Sig->new( 'invocant' => undef, 'positional' => [ ], 'named' => { } ),
                body  => [ $begin_ast, $$p ],
            ),
    );
    drop_pad;
    #print "FINAL AST: ",Dumper($final_ast); 
    #print "FINAL native: ", $final_ast->emit( $visitor_emit_perl5  );

    # create the runtime initializer
    # @COMPILER::BEGIN_RUNTIME
    ## push @COMPILER::BEGIN_RUNTIME, $initializer_name;
    #print "/begin_block\n";
    #print "\n=cut\n";

    return $final_ast;
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
    my $var = Var->new( 'sigil' => $_[0], 'twigil' => $_[1], 'name' => $_[2] );
    my $decl = $COMPILER::PAD[0]->declaration( $var );
    #print "COMPILER::get_var: @_ --> $decl\n";
    # TODO - annotate the variable with: Type, declarator
    return $var;
}

