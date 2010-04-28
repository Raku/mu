use v5.10;
use MooseX::Declare;
use SSA;
use Types;
class Mildew::Backend::Perlesque with Mildew::Backend {
#    use File::Temp qw(tempfile tmpnam);
    method perlesque_source($ast) {
        my $ssa_ast = SSA::to_ssa($ast->simplified,{
            '$scope' => Type::Scope->new(outer=> $Mildew::LexicalPreludeType)
        });
        $self->emit_block($ssa_ast);
    }
    method emit_block($block) {
        my %regs;
        my $value = sub {
            if ($_[0]->isa('AST::Reg')) {
                $regs{$_[0]->real_name}++;
                $_[0]->real_name;
            } elsif ($_[0]->isa('AST::IntegerConstant')) {
                'P6int.new(' . $_[0]->value . ')';
            } elsif ($_[0]->isa('AST::StringConstant')) {
                my $str = $_[0]->value;
                # TODO properly quote characters
                $str =~ s/(["\\])/\\$1/g;
                $str =~ s/\n/\\n/g;
                'P6Str.new("' . $str . '")';
            } else {
                ref($_[0]);
            }
        };
#            if ($_[0]->isa('AST::Reg')) {
#                if ($_[0]->name =~ /^¢/) {
#                    my $n = $_[0]->name;
#                    $n =~ s/^¢//;
#                    return $n;
#                }
#                unless (defined $regs{$_[0]->real_name}) {
#                    $regs{$_[0]->real_name} = $reg_id++;
#                }
#                "frame->reg[" . $regs{$_[0]->real_name} . "]";
#            } elsif ($_[0]->isa('AST::Block::SSA')) {
#                my ($func,$expr,$init) = $_[0]->emit_c;
#                $funcs .= $func;
#                $call_init_funcs .= $init;
#                $constant->($expr); 
#            } else {
#                die "don't know how to emit: ",ref($_[0]);
#            }
#                #$constant->(ref($_[0]).'???');
#        }
        my $code;
        for my $sub_block (@{$block->stmts}) {
            for my $stmt (@{$sub_block->stmts}) {
                #$code .= "\n/*".$stmt->pretty."*/";
                if ($stmt->isa('AST::Goto')) {
                    $code .= "#goto\n"
                    #$code .= "frame->pc = " . $labels{$stmt->block->id} . ";" . "break;\n"
                } elsif ($stmt->isa('AST::Branch')) {
                    $code .= "#branch\n";
#                    $code .= "frame->pc = "
#                        . $value->($stmt->cond)
#                        . " == SMOP__NATIVE__bool_false ? "
#                        . $labels{$stmt->else->id}
#                        . " : "
#                        . $labels{$stmt->then->id}
#                        . ";break;\n";
                } elsif ($stmt->isa('AST::Reg')) {
                    # make it a noop
                } elsif ($stmt->isa('AST::Assign')) {
                    if ($stmt->rvalue->isa('AST::Call')) {
                        my $type = $stmt->rvalue->capture->invocant->type_info->type;
                        $code .= $type->emit_perlesque_call($stmt,$value) . "\n";

                    } elsif ($stmt->rvalue->isa('AST::Phi')) {
                        # TODO make it a noop
                    } elsif ($stmt->rvalue->isa('AST::InferredTypeTest')) {
                        $code .= "#inferred type test\n"
                        #my $type = $stmt->rvalue->value->type_info->type;
                        #$code .= Emit::Yeast::assign($value->($stmt->lvalue),$value->(AST::IntegerConstant->new(value=>eval($stmt->rvalue->test) ? 1 : 0)));
                        #die if $@;
                    } else {
                        $code .= "# assignment\n";
                        #$code .= Emit::Yeast::assign($value->($stmt->lvalue),$value->($stmt->rvalue));
                    }
                } else {
                    #die "don't know how to emit: ",ref($stmt);
                    $code .= "#".ref($stmt)."\n";
                }
            }
        }

        for (@{$block->regs}) {
            delete $regs{'$'.$_};
        }
        "sub (P6object \$scope) {\n" . (join '',(map {"my P6object $_;\n"} keys %regs)) . "$code}";
    }
    method compile($ast,$output) {
        $self->output($self->perlesque_source($ast)."\n",$output);
    }
}
