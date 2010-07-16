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
    has gdb=>(default=>0,is=>'rw');
    has wrap_in_block=>(default=>1,is=>'rw');

    method _build_cflags {
        require SMOP;
        [SMOP::lib_flags(),SMOP::include_flags()];
    }
    method _build_ld_library_path {
        require SMOP;
        require Mildew::Setting::SMOP;
        [Mildew::Setting::SMOP::ld_library_path(),SMOP::ld_library_path()];
    }

    requires 'c_source';

    method compile($ast,$output) {
        die "-o is required when compiling to an executable\n" unless $output;
        my ($c_fh,$c_file) = tempfile();
        binmode($c_fh,":utf8");
        my $wrapped_ast = $self->wrap_in_block ? wrap_in_block($ast,$self->enclosing_scope) : $ast;
        print $c_fh $self->c_source($wrapped_ast);


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

        my @debug_aids;
        @debug_aids = 'valgrind' if $self->valgrind;
        @debug_aids = 'gdb' if $self->gdb;
        exec(@debug_aids,$tmp_executable);
    }

    # load the setting
    method enclosing_scope {

        $self->load_setting
        ? call(load => call(new => FETCH(lookup 'MildewSOLoader')),
            [string 'MildewCORE.setting.so',FETCH(lookup('$LexicalPrelude'))]) 
        : reg '$scope';
    }
}
