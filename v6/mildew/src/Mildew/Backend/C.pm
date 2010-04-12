use v5.10;
use MooseX::Declare;
class Mildew::Backend::C {
    use File::Temp qw(tempfile tmpnam);
    method c_source($ast) {
        my $body = "SMOP__Object* mold = " . $self->m0ld_to_c($self->ast_to_m0ld($ast)) . ";\n" . 
        "SMOP__Object* frame = SMOP__Mold__Frame_create(interpreter,mold);";
        my $boilerplate = $self->get_boilerplate;
        $boilerplate =~ s/%%BODY%%/$body/;
        $boilerplate =~ s/%%FUNCS%%//;
        $boilerplate;
    }

    method ast_to_m0ld($ast) {
        my ($front,$back);
    
        # TODO support 'so' and 'js-lib'
    
        #if ($C eq 'so' or $C eq 'js-lib') {
        #    $front = "my \$interpreter;\nmy \$scope;\nmy \$back;\n";
        #    $back = 'my $void = $back."setr"($ret);my $void = $interpreter."goto"($back);';
        #} else {
            $front = "my \$scope = ?SMOP__S1P__LexicalPrelude;\n";
            $back = '';
        #}
    
        my $m0ld = $front.<<'BOILERPLATE_FRONT'.$ast->m0ld('$main').<<'BOILERPLATE_BACK'.$back;
        my $void;
BOILERPLATE_FRONT
        my $AdhocSignature_scalar = $scope."lookup"("AdhocSignature");
        my $AdhocSignature = $AdhocSignature_scalar."FETCH"();
        
        my $sig = $AdhocSignature."new"(:"BIND"(mold {
            my $interpreter;
            my $scope;
            my $capture;
            my $continuation = $interpreter."continuation"();
            my $back = $continuation."back"();
            my $void = $interpreter."goto"($back);
        }),:"ACCEPTS"(mold {
            my $interpreter;
            my $scope;
            my $capture;
            my $continuation = $interpreter."continuation"();
            my $back = $continuation."back"();
            my $void = $interpreter."goto"($back);
        }));
    
        my $Code_scalar = $scope."lookup"("Code");
        my $Code = $Code_scalar."FETCH"();
        my $main_code = $Code."new"(:"outer"($scope),:"mold"($main),:"signature"($sig));
        my $Capture_scalar = $scope."lookup"("capture");
        my $Capture = $Capture_scalar."FETCH"();
        my $capture = $Capture."new"();
        my $ret = $main_code."postcircumfix:( )"($capture);
BOILERPLATE_BACK
}
    method m0ld_to_c($m0ld) {
        #XXX
        my @options;

        use IPC::Open2;
        open2(my $m0ld_exe_out,my $m0ld_exe_in,"../smop/m0ld_exe",@options);
        binmode $m0ld_exe_in, ':utf8';
        print $m0ld_exe_in $m0ld;
        close($m0ld_exe_in);
        local $/;
        binmode $m0ld_exe_out, ':utf8';
        return <$m0ld_exe_out>;
    }

    method compile($ast,$output) {
        die "-o is required when compiling to an executable\n" unless $output;
        my ($c_fh,$c_file) = tempfile();
        binmode($c_fh,":utf8");
        print $c_fh $self->c_source($ast);


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

        local $ENV{LD_LIBRARY_PATH} = 'CORE:../smop/build/lib';
        local $ENV{PERL5LIB} = "../smop/SMOP/blib/lib/:../smop/SMOP/blib/arch:" . ($ENV{PERL5LIB} || '');

        # TODO valgrind and gdb options
        exec($tmp_executable);
    }
}
