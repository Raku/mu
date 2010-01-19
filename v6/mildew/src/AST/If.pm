use v5.10;
use MooseX::Declare;
class AST::If extends AST::Base {
    use AST::Helpers;
    has 'cond' => (is => 'ro');
    has 'then' => (is => 'ro');
    has 'else' => (is => 'ro');
    has 'elsif' => (is => 'ro');
    method m0ld($ret) {
        my $id_cond = AST::unique_id;
        my $label_then = AST::unique_label;
        my $label_else = AST::unique_label;
        my $label_endif = AST::unique_label;
        my $cond = $self->cond->m0ld($id_cond);
        my $then = 'noop;';
        $then = $self->then->m0ld($ret) if $self->then;
        my $else = 'noop;';
        if ($self->else) {
            $else = $self->else->m0ld($ret);
        }
        my $elsifs = '';
        if ($self->elsif) {
            foreach my $part (@{$self->elsif}) {
                my $id_elsif_cond = AST::unique_id;
                my $label_elsif_then = AST::unique_label;
                my $label_elsif_else = AST::unique_label;
                my $elsif_cond = $part->cond->m0ld($id_elsif_cond);
                my $elsif_then = $part->then->m0ld($ret);
                $elsifs .= $elsif_cond."\n".
                  'my '.$id_elsif_cond.'_val = '.$id_elsif_cond.'."FETCH"();'."\n".
                  'my '.$id_elsif_cond.'_bool = '.$id_elsif_cond.'_val."true"();'."\n".
                  'if '.$id_elsif_cond.'_bool { goto '.$label_elsif_then.' } else { goto '.$label_elsif_else.' };'."\n".
                  $label_elsif_then.':'."\n".
                  $elsif_then."\n".
                  'goto '.$label_endif.';'."\n".
                  $label_elsif_else.': noop;'."\n"
            }
        }
    
        $cond."\n".
        'my '.$id_cond.'_val = '.$id_cond.'."FETCH"();'."\n".
        'my '.$id_cond.'_bool = '.$id_cond.'_val."true"();'."\n".
        'if '.$id_cond.'_bool { goto '.$label_then.' } else { goto '.$label_else.' };'."\n".
        $label_then.':'."\n".
        $then."\n".
        'goto '.$label_endif.';'."\n".
        $label_else.':'."\n".
        $elsifs.
        $else."\n".
        $label_endif.': noop;'."\n"
    }
    method simplified {


        my $endif = AST::Seq->new(stmts=>[],id=>AST::unique_label);
        my $result = AST::unique_reg;

        my @parts = ($self,$self->elsif ? @{$self->elsif}: ());
        my $branch;
        my @setup;
        for my $part (@parts) {
            my $old_branch = $branch;
            my ($cond,@cond_setup) = call(true=>FETCH($part->cond))->simplified;
            my ($then_val,@then_setup) = $part->then->simplified;
            my $then = AST::Seq->new(id=>AST::unique_label,stmts=>[@then_setup,AST::Assign->new(lvalue=>$result,rvalue=>$then_val),AST::Goto->new(block=>$endif)]);
            $branch = AST::Branch->new(cond=>$cond,then=>$then);
            my $block = AST::Seq->new(id=>AST::unique_label,stmts=>[@cond_setup,$branch,$then]);
            push(@setup,$block);
            $old_branch->else($block) if $old_branch;
        }

        if ($self->else) {
            my ($else_val,@else_setup) = $self->else->simplified;
            my $else = AST::Seq->new(id=>AST::unique_label,stmts=>[@else_setup,AST::Assign->new(lvalue=>$result,rvalue=>$else_val),AST::Goto->new(block=>$endif)]);
            $branch->else($else);
            push(@setup,$else);
        } else {
            $branch->else($endif);
        }

        ($result,@setup,$endif);
    }
    method pretty {
        my $code;
        if ($self->then) {
            $code =
                'if ' . $self->cond->pretty . " {\n"
                . AST::indent($self->then->pretty) . "\n"
                . "}\n";
        } else {
            $code =
                'unless ' . $self->cond->pretty . " {\n"
                . AST::indent($self->else->pretty) . "\n"
                . "}\n";
        }
        if ($self->elsif) {
            foreach my $part (@{$self->elsif}) {
                $code .=
                  'elsif '.$part->cond->pretty . " {\n"
                    . AST::indent($self->then->pretty). "\n"
                    . "}\n";
            }
        }
        if ($self->else) {
            $code .=
              "else {\n"
                . AST::indent($self->else->pretty). "\n"
                . "}\n";
        }
        $code;
    }
}
