use v5.10;
use MooseX::Declare;
use SMOP;
role Mildew::Backend::C {
    use AST;
    use AST::Helpers;
    use File::Temp qw(tempfile tmpnam);

    has cflags=>(lazy_build=>1,is=>'rw');
    has ld_library_path=>(lazy_build=>1,is=>'rw');

    method _build_cflags {
        [SMOP::lib_flags(),SMOP::include_flags()];
    }
    method _build_ld_library_path {
        ['../mildew-old/CORE',SMOP::ld_library_path()];
    }

    requires 'c_source';

    method compile($ast,$output) {
        die "-o is required when compiling to an executable\n" unless $output;
        my ($c_fh,$c_file) = tempfile();
        binmode($c_fh,":utf8");
        print $c_fh $self->c_source($self->add_prelude_load($ast));


        # compile the c source to the executable
        system("gcc","-g","-xc",@{$self->cflags},$c_file,"-o",$output);
    }


    method get_boilerplate {
        require SMOP::Boilerplate;
        return $SMOP::Boilerplate::BOILERPLATE;
    }

    method run($ast) {
        my $tmp_executable = tmpnam;
        $self->compile($ast,$tmp_executable);

        local $ENV{LD_LIBRARY_PATH} = join(':',@{$self->ld_library_path});

        #'../mildew-old/CORE:../smop/build/lib';
        # local $ENV{PERL5LIB} = "../smop/SMOP/blib/lib/:../smop/SMOP/blib/arch:" . ($ENV{PERL5LIB} || '');

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
