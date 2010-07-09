use v5.10;
use MooseX::Declare;
use Mildew::SSA;
use Types;
class Mildew::Backend::OptC with Mildew::Backend::C {
    use File::Temp qw(tempfile tmpnam);
    use String::Escape qw(backslash quote);
    use Getopt::Long qw(GetOptionsFromArray);
    has options=>(is=>'ro',default=>sub {{}});
    has trace=>(is=>'rw');
    has dump=>(is=>'rw');
    method BUILD {
        my ($trace,$dump,$cflags,$ld_library_path,$no_setting,$valgrind);
        GetOptionsFromArray(
            ($self->options->{BACKEND} // []),
            'trace' => \$trace,
            'dump=s' => \$dump,
            'cflags=s' => \$cflags,
            'no-setting' => \$no_setting,
            'ld-library-path=s' => \$ld_library_path,
            'valgrind' => \$valgrind
        ) || die 'incorrent options passed to Mildew::Backend::OptC';
        use YAML::XS;
        $self->trace($trace);
        $self->dump($dump);
        $self->cflags([split(',',$cflags)]) if $cflags;
        $self->load_setting(!$no_setting) if $no_setting;
        $self->ld_library_path([split(',',$ld_library_path)]) if $ld_library_path;
        $self->valgrind($valgrind);
    }
    method c_source($ast) {
        my $ssa_ast = Mildew::SSA::to_ssa($ast->simplified,{
            '$scope' => Type::Scope->new(outer=> $Mildew::LexicalPreludeType)
        });
        my ($funcs,$expr,$call_init_funcs) = $self->emit_block($ssa_ast); 
        my $boilerplate = $self->get_boilerplate;
        my $body = 
              $call_init_funcs 
            . "SMOP__Object* yeast = " . $expr . ";\n"
            . "SMOP__Object* frame = SMOP__Yeast__Frame_create(interpreter,yeast);\n"
            . "yeast_reg_set(interpreter,frame,0,SMOP_REFERENCE(interpreter,interpreter));\n"
            . "yeast_reg_set(interpreter,frame,1,SMOP_REFERENCE(interpreter,SMOP__S1P__LexicalPrelude));\n";
        $boilerplate =~ s/%%BODY%%/$body/;
        $boilerplate =~ s/%%FUNCS%%/$funcs/;
        $boilerplate;
    }

    method yeast($ast) {
        my $ssa_ast = Mildew::SSA::to_ssa($ast->simplified,{
            '$scope' => Type::Scope->new(outer=> $Mildew::LexicalPreludeType)
        });
        $self->emit_block($ssa_ast); 
    }
    method emit_block($block) {
        state $unique_func_id = 0;

        my $func_name = 'smop_yeast_' . $unique_func_id++;
        my $funcs = '';
        my $call_init_funcs = '';
        my $i = 0;
        my $code;
        my %labels;

        my %regs;
        my $reg_id = 0;
        
        my $constant_decls;
        my $init_constants = "static void ${func_name}_init(interpreter) {";

        my $constants_id = 0;
        my $constant = sub {
            my $c = $func_name . "_constant_" . $constants_id++;
            $constant_decls .= "static SMOP__Object* $c;\n";
            $init_constants .= "$c = $_[0];\n";
            $c;
        };

        for (@{$block->regs}) {
            $regs{'$'.$_} = $reg_id++;
        }

        my $value = sub {
            if ($_[0]->isa('AST::Reg')) {
                if ($_[0]->name =~ /^¢|^\?/) {
                    my $n = $_[0]->name;
                    $n =~ s/^¢|^\?//;
                    return $n;
                }
                unless (defined $regs{$_[0]->real_name}) {
                    $regs{$_[0]->real_name} = $reg_id++;
                }
                "frame->reg[" . $regs{$_[0]->real_name} . "]";
            } elsif ($_[0]->isa('AST::StringConstant')) {
                my $str = $_[0]->value;
                # TODO properly quote characters
                $str =~ s/(["\\])/\\$1/g;
                $str =~ s/\n/\\n/g;
                $constant->('SMOP__NATIVE__idconst_createn("' . $str . '",' . length($_[0]->value) . ')');
            } elsif ($_[0]->isa('AST::IntegerConstant')) {
                $constant->('SMOP__NATIVE__int_create(' . $_[0]->value . ')');
            } elsif ($_[0]->isa('AST::Block::SSA')) {
                my ($func,$expr,$init) = $self->emit_block($_[0]);
                $funcs .= $func;
                $call_init_funcs .= $init;
                $constant->($expr); 
            } else {
                die "don't know how to emit: ",ref($_[0]);
                #$constant->(ref($_[0]).'???');
            }
        };

        for my $subblock (@{$block->stmts}) {
            if ($subblock->id) {
                $labels{$subblock->id} = $i;
            }
            for my $stmt (@{$subblock->stmts}) {
                if ($stmt->isa('AST::Assign')) {
                    # TODO - handle profile_info more cleanly
                    if ($stmt->rvalue->isa('AST::Call') && defined $Mildew::profile_info) {
                        $i++; 
                    }
                }
                $i++; 
            }
        }
        $i = 0;
        for my $subblock (@{$block->stmts}) {
            for my $stmt (@{$subblock->stmts}) {
                #$code .= "\n/*".$stmt->pretty."*/\n";
                $code .= "case $i:";
                if ($self->trace) {
                    $code .= "\nprintf(".quote(backslash($stmt->pretty . "\n")).");\n";
                }
                if ($self->dump) {
                    my $file = quote(backslash($self->dump));
                    $code .= "\nsmop_dump_print(interpreter,(SMOP__Object*)frame,$file);\n";
                }

                if ($stmt->isa('AST::Goto')) {
                    $code .= "frame->pc = " . $labels{$stmt->block->id} . ";" . "break;\n"
                } elsif ($stmt->isa('AST::Branch')) {
                    $code .= "frame->pc = "
                        . $value->($stmt->cond)
                        . " == SMOP__NATIVE__bool_false ? "
                        . $labels{$stmt->else->id}
                        . " : "
                        . $labels{$stmt->then->id}
                        . ";break;\n";
                } elsif ($stmt->isa('AST::Reg')) {
                    # make it a noop
                    $code .= ';';
                } elsif ($stmt->isa('AST::Assign')) {
                    if ($stmt->rvalue->isa('AST::Call')) {
                        my $type = $stmt->rvalue->capture->invocant->type_info->type;
                        my ($c,$trailing) = $type->emit_call($i,$stmt,$value);
                        $code .= $c;
                        if ($trailing) {
                            $i++;
                            $code .= "case $i: frame->pc = " . ($i+1) . ';' . $trailing;
                        }
                    } elsif ($stmt->rvalue->isa('AST::Phi')) {
                        # TODO make it a noop
                        $code .= ';';
                    } elsif ($stmt->rvalue->isa('AST::InferredTypeTest')) {
                        my $type = $stmt->rvalue->value->type_info->type;
                        $code .= Emit::Yeast::assign($value->($stmt->lvalue),"SMOP_REFERENCE(interpreter,".$value->(AST::IntegerConstant->new(value=>eval($stmt->rvalue->test) ? 1 : 0).")"));
                        die if $@;
                    } else {
                        $code .= Emit::Yeast::assign($value->($stmt->lvalue),"SMOP_REFERENCE(interpreter,".$value->($stmt->rvalue).")");
                    }
                } else {
                    $code .= "/*".ref($stmt)."*/\n";
                }
                $i++;
            }
        }
        ($funcs . $constant_decls . $init_constants . "}\n" . "static void " . $func_name . "(SMOP__Object* interpreter,SMOP__Yeast__Frame* frame) {" 
        . "  switch (frame->pc) {"
        . $code
        . "case $i : frame->pc = -1;\n" 
        .  "  }}\n","SMOP__Yeast_create(" . (scalar keys %regs)
        . ",(SMOP__Object*[]) {NULL}"
        . ",$func_name)","${call_init_funcs}${func_name}_init(interpreter);");
    }
}
