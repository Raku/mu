use v5.10;
use MooseX::Declare;
role Mildew::Backend::C {
    use AST;
    use AST::Helpers;
    use File::Temp qw(tempfile tmpnam);

    has cflags=>(lazy_build=>1,is=>'rw');
    has ld_library_path=>(lazy_build=>1,is=>'rw');
    has load_setting=>(default=>1,is=>'rw');
    has valgrind=>(default=>0,is=>'rw');

    method _build_cflags {
        require SMOP;
        [SMOP::lib_flags(),SMOP::include_flags()];
    }
    method _build_ld_library_path {
        require SMOP;
        ['.',SMOP::ld_library_path()];
    }

    requires 'c_source';

    method compile($ast,$output) {
        die "-o is required when compiling to an executable\n" unless $output;
        my ($c_fh,$c_file) = tempfile();
        binmode($c_fh,":utf8");
        print $c_fh $self->c_source($self->load_setting ? $self->add_setting_load($ast) : $ast);


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

        exec(($self->valgrind ? ('valgrind') : ()),$tmp_executable);
    }

    # load the setting
    method add_setting_load($ast) {
        my $load_CORE = call(load => call(new => FETCH(lookup 'MildewSOLoader')),
        [string 'MildewCORE.setting.so',FETCH(lookup('$LexicalPrelude'))]);
        AST::Block->new(stmts=>[$load_CORE,@{$ast->stmts}],regs=>$ast->regs);
    }
}
