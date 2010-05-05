use v5.10;
use MooseX::Declare;
role Mildew::Backend::C {
    use AST;
    use File::Temp qw(tempfile tmpnam);

    method c_source($ast) {
        my $body = "SMOP__Object* mold = " . $self->m0ld_to_c($self->ast_to_m0ld($ast)) . ";\n" . 
        "SMOP__Object* frame = SMOP__Mold__Frame_create(interpreter,mold);";
        my $boilerplate = $self->get_boilerplate;
        $boilerplate =~ s/%%BODY%%/$body/;
        $boilerplate =~ s/%%FUNCS%%//;
        $boilerplate;
    }

    requires 'c_source';

    method compile($ast,$output) {
        die "-o is required when compiling to an executable\n" unless $output;
        my ($c_fh,$c_file) = tempfile();
        binmode($c_fh,":utf8");
        print $c_fh $self->c_source($self->add_prelude_load($ast));


        my @SMOP_INCLUDE = map {"-I".$_} glob("../smop/*/include");
        my @MILDEW_LDOPTS = ( '-L../smop/build/lib',
                          map { s/^.+?\/lib\/lib|.so$//g; "-l".$_ } glob("../smop/build/lib/*.so") );

        # compile the c source to the executable
        system("gcc","-g","-xc","-L../smop/build/lib",@SMOP_INCLUDE,@MILDEW_LDOPTS,$c_file,"-o",$output);
    }
    method get_boilerplate {
        open(my $boilerplate,"../smop/m0ld/m0ld_boilerplate") || die "can't open internal file\n";
        local $/;
        return scalar <$boilerplate>;
    }

    method run($ast) {
        my $tmp_executable = tmpnam;
        $self->compile($ast,$tmp_executable);

        local $ENV{LD_LIBRARY_PATH} = '../mildew-old/CORE:../smop/build/lib';
        local $ENV{PERL5LIB} = "../smop/SMOP/blib/lib/:../smop/SMOP/blib/arch:" . ($ENV{PERL5LIB} || '');

        # TODO valgrind and gdb options
        exec($tmp_executable);
    }
    method add_prelude_load($ast) {
        # load the setting
        my $load_CORE = call(load => call(new => FETCH(lookup 'MildewSOLoader')),
        [string 'CORE.mildew.so',FETCH(lookup('$LexicalPrelude'))]);
        unshift @{$ast->stmts},$load_CORE;
        $ast;
    }
}
