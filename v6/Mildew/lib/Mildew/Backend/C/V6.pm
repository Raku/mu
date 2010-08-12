use v5.10;
use MooseX::Declare;
use Mildew::Setting::SMOP;
class Mildew::Backend::C::V6 extends Mildew::Backend::C::So {
    use Mildew::AST;
    use Mildew::AST::Helpers;
    use File::Temp qw(tempfile tmpnam);



    # false doesn't support setr
    sub wrap_in_block_without_setr {
        my ($ast,$scope) = @_;
        Mildew::AST::Block->new(regs=>['interpreter','scope'],stmts=>[fcall(call(new => FETCH(lookup('Code')),[],[string 'outer'=>($scope // reg '$scope'),string 'signature'=>empty_sig(),string 'mold' => $ast]))]);
    }

    method compile($ast,$output) {
        die "-o is required when compiling to an executable\n" unless $output;
        my ($c_fh,$c_file) = tempfile();
        binmode($c_fh,":utf8");
        my $wrapped_ast = $self->wrap_in_block ? wrap_in_block_without_setr($ast,$self->enclosing_scope) : $ast;
        print $c_fh $self->c_source($wrapped_ast);


        # compile the c source to the shared library
        $ENV{LD_RUN_PATH} = join(':',SMOP::ld_library_path(),Mildew::Setting::SMOP::ld_library_path());
        system("gcc","-fPIC","-g","-xc",@{$self->cflags},"-shared",$c_file,"-o",$output);
    }


    method get_boilerplate {
        <<'END'

#include <smop/main.h>

#include <smop/base.h>
#include <smop/s0native.h>
#include <smop/nagc.h>
#include <smop/interpreter.h>
#include <smop/capture.h>
#include <smop/native.h>
#include <smop/mold.h>
#include <smop/s1p.h>
#include <smop/lost.h>
#include <smop/p6opaque.h>
#include <smop/s1p-oo.h>
#include <smop/yeast.h>
#include <smop/profile.h>
#include <smop/dump.h>
#include <smop/nagc.h>
#include <stdio.h>

/* Your helper function go here */
%%FUNCS%%

int run(int argc, char** argv) {


  SMOP__Object* interpreter = smop_main_get_interpreter();


  /* The frame creation code goes here */
  %%BODY%%

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
    SMOP__NATIVE__idconst_create("goto"),
    SMOP__NATIVE__capture_create(interpreter,
        (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),frame,NULL},
        (SMOP__Object*[]) {NULL})
  );

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                SMOP__NATIVE__idconst_create("loop"),
                SMOP__NATIVE__capture_create(
                    interpreter,
                    (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),NULL}
                    ,(SMOP__Object*[]) {NULL}));

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
    SMOP__NATIVE__idconst_create("goto"),
    SMOP__NATIVE__capture_create(
        interpreter,
        (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),SMOP__NATIVE__bool_false,NULL}
        ,(SMOP__Object*[]) {NULL}));

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
                SMOP__NATIVE__idconst_create("loop") , SMOP__NATIVE__capture_create(
                    interpreter,
                    (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),NULL},
                    (SMOP__Object*[]) {NULL}));

  return 0;
}
END
    }

    method path_to_setting {
        Mildew::Setting::SMOP::ld_library_path() . '/' .
        'MildewCORE.setting.so';
    }
    method run($ast) {
        die "can't run with this backend";
    }
}
