use v5.10;
use MooseX::Declare;
use SMOP;

# TODO seperate the shared library creation from the type of the C backend
class Mildew::Backend::C::So extends Mildew::Backend::OptC {
    use AST;
    use AST::Helpers;
    use File::Temp qw(tempfile tmpnam);

    method compile($ast,$output) {
        die "-o is required when compiling to an executable\n" unless $output;
        my ($c_fh,$c_file) = tempfile();
        binmode($c_fh,":utf8");
        my $ast_with_load = $self->load_setting ? $self->add_setting_load($ast) : $ast;
        # TODO handle_YOU_ARE_HERE only on settings
        print $c_fh $self->c_source($self->handle_YOU_ARE_HERE($ast));

        # compile the c source to the executable
        system("gcc","-fPIC","-g","-xc",@{$self->cflags},"-shared",$c_file,"-o",$output);
    }
    method handle_YOU_ARE_HERE($ast) {
        AST::Block->new(
            regs=>[@{$ast->regs},'YOU_ARE_HERE'],
            stmts=>trailing_return([@{$ast->stmts},reg '$YOU_ARE_HERE'])
        );
    }


    method get_boilerplate {
        return <<'BOILERPLATE';
#include <stdio.h>
#include <smop/base.h>
#include <smop/mold.h>
#include <smop/capture.h>
#include <smop/s0native.h>
#include <smop/native.h>
#include <smop/s1p.h>
#include <smop/yeast.h>
#include <smop/dump.h>
%%FUNCS%%
void init(SMOP__Object* interpreter,SMOP__Object* scope,SMOP__Object* continuation) {
  %%BODY%%
  smop_reg_set(interpreter,frame,0,SMOP_REFERENCE(interpreter,interpreter));
  smop_reg_set(interpreter,frame,1,scope);
  smop_back_set(interpreter,frame,continuation);

  SMOP_DISPATCH(interpreter, SMOP_RI(interpreter),
    SMOP__NATIVE__idconst_create("goto"),
    SMOP__NATIVE__capture_create(interpreter,
        (SMOP__Object*[]) {SMOP_REFERENCE(interpreter,interpreter),frame,NULL},
        (SMOP__Object*[]) {NULL})
  );

}
BOILERPLATE
    }

    method run($ast) {
        die "You can't run a shared library.\n";
    }
}
