package COMPILER;
use Data::Dumper;

our $Code_begin_block =
  ::DISPATCH
  ( $::Routine, 'new',
    { modified => $_MODIFIED, name => '$Code_begin_block' } );
$Code_begin_block = ::DISPATCH
  (
   $::Code, 'new',
   {
    code => sub {

        # my $ast = shift;
        my $List__ =
          ::DISPATCH( $::Array, 'new',
                      { modified => $_MODIFIED, name => '$List__' } );
        my $ast;
        $ast =
          ::DISPATCH( $::Scalar, 'new',
                      { modified => $_MODIFIED, name => '$ast' } )
            unless defined $ast;

        BEGIN {
            $ast =
              ::DISPATCH( $::Scalar, 'new',
                          { modified => $_MODIFIED, name => '$ast' } );
        }
        my $CAPTURE;
        $CAPTURE =
          ::DISPATCH( $::Scalar, 'new',
                      { modified => $_MODIFIED, name => '$CAPTURE' } )
            unless defined $CAPTURE;
        BEGIN {
            $CAPTURE =
              ::DISPATCH( $::Scalar, 'new',
                          { modified => $_MODIFIED, name => '$CAPTURE' } );
        }
        ::DISPATCH_VAR( $CAPTURE, "STORE", ::CAPTURIZE( \@_ ) );
        do {
            ::MODIFIED($List__);
            $List__ = ::DISPATCH( $CAPTURE, 'array', );
        };
        do {
            ::MODIFIED($ast);
            $ast =
              ::DISPATCH( $List__, 'INDEX',
                          ::DISPATCH( $::Int, 'new', 0 ) );
        };

        # this routine is called by begin-blocks at compile time, in order to execute the code
        # Input: '::Lit::Code' AST node

        #print "begin_block\n";
        #print "PARAM: ",Dumper(\@_);
        #my $env = shift;
        my $ast = shift;

        #print Dump( $ast );
        $ast = $ast->emit( $COMPILER::visitor_token );
        #$ast = $ast->emit( $COMPILER::visitor_lexical_sub );
        #$ast = $ast->emit( $COMPILER::visitor_subset );
        $ast = $ast->emit( $COMPILER::visitor_metamodel );
        #$ast = $ast->emit( $COMPILER::visitor_create_env );
    
    my $ast = shift;

    # execute the code inside the current pad
    add_pad;
    my $data = $COMPILER::PAD[0]->eval_ast( $ast, [
            $visitor_token, $visitor_metamodel, $visitor_global, $visitor_emit_perl5,
        ] );  # XXX - want() context
    drop_pad;
    die "At BEGIN: " . $@ if $@;
    #print "RETURN DATA: ", Dumper($data);

        # check for side-effects
        my @begin_stmts;

        #print "=pod\n";
        #print "# BEGIN ENV: ", Dumper( $COMPILER::PAD[0]->lexicals ), "\n";
        #print "BEGIN AST: ", Dumper( \$ast );
        #print "BEGIN: Native code: $native\n\n";

    for my $pad ( @COMPILER::PAD ) {
        #print "# Lexicals here: ", Dumper( $pad->lexicals ), "\n";
        my $side_effects = $pad->side_effects; 
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
                    } else {
                        # no name; bind to the value
                        $src = $src . "$name := " . $value->{_dispatch}( $value, 'perl' )->{_value} . '; ';
                    }
                } else {
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
        #print "BEGIN native: ", $begin_ast->emit( $COMPILER::visitor_emit_perl5  ) );
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
        #print "FINAL native: ", $final_ast->emit( $COMPILER::visitor_emit_perl5  );

        # create the runtime initializer
        # @COMPILER::BEGIN_RUNTIME
        ## push @COMPILER::BEGIN_RUNTIME, $initializer_name;
        #print "/begin_block\n";
        #print "\n=cut\n";

        return $final_ast;
    }
   }
  );

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
