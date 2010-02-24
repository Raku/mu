use v5.10;
use MooseX::Declare;
class AST::Block::SSA extends AST::Block {
    method emit_c {
        state $unique_func_id = 0;

        my $func_name = 'smop_yeast_' . $unique_func_id++;
        my $i = 0;
        my $code;
        my %labels;

        my %regs;
        my $reg_id = 0;

        my $reg = sub {
            unless ($regs{$_[0]->name}) {
                $regs{$_[0]->name} = ++$reg_id;
            }
            "frame->reg[" . $regs{$_[0]->name} . "]";
        };
        my $value = sub {
            if ($_[0]->isa('AST::Reg')) {
                "SMOP_REFERENCE(interpreter," . $reg->($_[0]) . ")";
            } else {
            }
        };

        for my $block (@{$self->stmts}) {
            if ($block->id) {
                $labels{$block->id} = $i;
            }
            for my $stmt (@{$block->stmts}) {
                $i++; 
            }
        }
        $i = 0;
        for my $block (@{$self->stmts}) {
            for my $stmt (@{$block->stmts}) {
                $code .= "case $i:";
                if ($stmt->isa('AST::Goto')) {
                    $code .= "frame->pc = " . $labels{$stmt->block->id} . ";" . "break;\n"
                } elsif ($stmt->isa('AST::Branch')) {
                    $code .= "frame->pc = "
                        . $reg->($stmt->cond)
                        . " == SMOP__NATIVE__bool_false ? "
                        . $labels{$stmt->then->id}
                        . " : "
                        . $labels{$stmt->else->id}
                        . ";break;\n";
                } elsif ($stmt->isa('AST::Reg')) {
                    next;
                } elsif ($stmt->isa('AST::Assign')) {
                    if ($stmt->rvalue->isa('AST::Call')) {

                        $code .= "/*call*/\n";
                    } else {
                        my $target = $reg->($stmt->lvalue);

                        $code .= "if ($target) SMOP_RELEASE(interpreter,$target);\n"
                        . "$target = " . $value->($stmt->rvalue) . ";\n"
                    }
                } else {
                    $code .= "/*".ref($stmt)."*/\n";
                }
                $i++;
        }
    }
    ("static void " . $func_name . "(SMOP__Object* interpreter,SMOP__Yeast__Frame* frame) {" 
    . "  switch (frame->pc) {"
    . $code
    . "case $i : frame->pc = -1;\n" 
    .  "  }\n","SMOP__Yeast_create(" . (scalar keys %regs)
    . ",(SMOP__Object*[]) {NULL}"
    . ",$func_name)");
    }
}
