use v5.10;
use MooseX::Declare;
use SSA;
use Types;
class Mildew::Backend::OptC with Mildew::Backend::C {
    use File::Temp qw(tempfile tmpnam);
    method c_source($ast) {
        my $ssa_ast = SSA::to_ssa($ast->simplified,{
            '$scope' => Type::Scope->new(outer=> $Mildew::LexicalPreludeType)
        });
        my ($funcs,$expr,$call_init_funcs) = $ssa_ast->emit_c; 
        my $boilerplate = $self->get_boilerplate;
        my $body = 
              $call_init_funcs 
            . "SMOP__Object* yeast = " . $expr . ";\n"
            . "SMOP__Object* frame = SMOP__Yeast__Frame_create(interpreter,yeast);\n"
            .   "smop_dump_print(interpreter,SMOP_DUMP(interpreter,frame));"
            . "yeast_reg_set(interpreter,frame,0,SMOP_REFERENCE(interpreter,interpreter));"
            . "yeast_reg_set(interpreter,frame,1,SMOP_REFERENCE(interpreter,SMOP__S1P__LexicalPrelude));\n";
        $boilerplate =~ s/%%BODY%%/$body/;
        $boilerplate =~ s/%%FUNCS%%/$funcs/;
        $boilerplate;
    }

}
