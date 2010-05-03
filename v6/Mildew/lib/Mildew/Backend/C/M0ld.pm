use v5.10;
use MooseX::Declare;
class Mildew::Backend::C::M0ld with Mildew::Backend::C {
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
}
