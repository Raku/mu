package SSA;
use Scalar::Util qw(refaddr);
use Set::Object ();
use List::MoreUtils qw(uniq);
use Hash::Util::FieldHash qw(idhash);
use Types;
use v5.10;
use strict;
use warnings;
use utf8;

=over 
=cut

sub postorder {
    my ($blocks) = @_;
    idhash my %numbering;
    postorder_numbering($blocks->[0],\%numbering,do {idhash my %visited;\%visited},\(my $number = 0));
    my $postorder =  [sort {$numbering{$a} <=> $numbering{$b}} @{$blocks}];
    (\%numbering,$postorder,[reverse @$postorder]);
}

sub postorder_numbering {
    my ($node,$numbering,$visited,$number) = @_;
    $visited->{$node}++;
    for ($node->jumps) {
        postorder_numbering($_,$numbering,$visited,$number) unless $visited->{$_};
    }
    $numbering->{$node} = $$number++;
}

=item value_to_ssa($registers,$value)

Convert a value to SSA, mapping registers according to $registers

=cut

sub transform_stmt {
    my ($callbacks,$value) = @_;
    for my $class (keys %{$callbacks}) {
        if ($value->isa($class)) {
            return $callbacks->{$class}($value);
        }
    }

    if ($value->isa('AST::Call')) {
        AST::Call->new(identifier=>$value->identifier,capture=>transform_stmt($callbacks,$value->capture));
    } elsif ($value->isa('AST::Capture')) {
        AST::Capture->new(
            ($value->invocant ? (invocant => transform_stmt($callbacks,$value->invocant)) : ()),
            positional => [map {transform_stmt($callbacks,$_)} @{$value->positional}],
            named=>[map {transform_stmt($callbacks,$_)} @{$value->named}]
        );

    } elsif ($value->isa('AST::Assign')) {
        AST::Assign->new(lvalue=>transform_stmt($callbacks,$value->lvalue),rvalue=>transform_stmt($callbacks,$value->rvalue));
    } elsif ($value->isa('AST::Branch')) {
        AST::Branch->new(then=>$value->then,else=>$value->else,cond=>transform_stmt($callbacks,$value->cond));
    } else {
        $value;
    }
}

=item alive_regs($blocks_in_postorder_numbering)

Calculates which registers might be alive in a given block

=cut

sub alive_regs {
    my ($postorder) = @_;
    idhash my %alive_regs;
    for my $block (@{$postorder}) {
        $alive_regs{$block} = Set::Object->new();
        for my $stmt (@{$block->stmts}) {
            if ($stmt->isa('AST::Assign')) {
                if ($stmt->rvalue->isa('AST::Call')) {
                    my $capture = $stmt->rvalue->capture; 
                    $alive_regs{$block}->insert(map {$_->name} grep {$_->isa('AST::Reg')} $capture->invocant,@{$capture->positional},@{$capture->named});
                }
            } elsif ($stmt->isa('AST::Branch')) {
                $alive_regs{$block}->insert($stmt->cond->name) if $stmt->cond->isa('AST::Reg');
            }
        }
    }
    for my $block (@{$postorder}) {
        for my $p ($block->jumps) {
            $alive_regs{$block} = $alive_regs{$block}->union($alive_regs{$p});
        }
    }
    \%alive_regs;
}

=item dominace_frontiers($block,$idoms,$predecessors)

calculates the dominace frontier set

=cut

sub dominance_frontiers {
    my ($blocks,$idoms,$predecessors) = @_;
    idhash my %dominance_frontiers;
    for my $block (@{$blocks}) {
        next unless @{$predecessors->{$block}} >= 2;
        for my $p (@{$predecessors->{$block}}) {
            my $runner = $p;
            while (refaddr $runner != $idoms->{$block}) {
                push @{$dominance_frontiers{$block}},$runner;
                $runner = $idoms->{$runner};
            }
        }
    }
    \%dominance_frontiers;
}

sub predecessors {
    my ($blocks) = @_;
    idhash my %predecessors;
    for my $block (@{$blocks}) {
        $predecessors{$block} = [];
    }
    for my $block (@{$blocks}) {
        for my $target ($block->jumps) {
            push @{$predecessors{$target}},$block;
        }
    }
    \%predecessors;
}
sub idoms {
    my ($rpostorder,$numbering,$predecessors) = @_;
    my @blocks = @{$rpostorder};
    my $start = shift @blocks;

    idhash my %idoms;
    $idoms{$start} = $start;
    my $new_idom;
    my $changed = 1;
    while ($changed) { 
        $changed = 0;
        for my $b (@blocks) {
            my @predecessors = @{$predecessors->{$b}};
            my $new_idom = shift @predecessors;
            for my $p (@predecessors) {
                if (defined $idoms{$p}) {
                    my $finger1 = $p;
                    my $finger2 = $new_idom;
                    while (refaddr $finger1 != refaddr $finger2) {
                        while ($numbering->{$finger1} < $numbering->{$finger2}) {
                            $finger1 = $idoms{$finger1};
                        }
                        while ($numbering->{$finger2} < $numbering->{$finger1}) {
                            $finger2 = $idoms{$finger2};
                        }
                    }
                    $new_idom = $finger1;
                }
            }
            if (!$idoms{$b} or refaddr $idoms{$b} != refaddr $new_idom) {
                $idoms{$b} = $new_idom;
                $changed = 1;
            }
        }
    }
    \%idoms;
}

sub doms {
    my ($mold,$blocks,$types) = @_;

    my ($numbering,$postorder,$rpostorder) = postorder($blocks);

    my $predecessors = predecessors($blocks);
    my $idoms = idoms($rpostorder,$numbering,$predecessors);
    my $dominance_frontiers = dominance_frontiers($blocks,$idoms,$predecessors);
    my $alive_regs = alive_regs($postorder);

    my %unique;
    idhash my %regs;
    for (@{$mold->regs}) {
        my $reg = '$'.$_;
        $regs{$blocks->[0]}{$reg} = AST::Reg->new(
            type_info=>TypeInfo->new($types->{$reg} ? (type=>$types->{$reg}) : () ),
            name=>$reg,
            real_name=>$reg
        );
    }
    # TODO - handle assigning to the value twice in the same block correctly
    for my $block (@{$postorder}) {
        for my $stmt (@{$block->stmts}) {
            if ($stmt->isa('AST::Assign')) {
                my $name = $stmt->lvalue->name;
                my $reg = AST::Reg->new(
                    name=>$name."_".++$unique{$name},
                    real_name=>$name,
                );
                $regs{$block}{$name} = $reg;
                $stmt = AST::Assign->new(lvalue=>$reg,rvalue=>$stmt->rvalue);
            }
        }
    }

    for my $block (@{$rpostorder}) {
        my $idom = $idoms->{$block};
        for my $reg ($alive_regs->{$idom}->members) {
            if ($regs{$block}{$reg}) {
            } elsif ($regs{$idom}{$reg}) {
                $regs{$block}{$reg} = $regs{$idom}{$reg};
            } else {

                my @phi = uniq map {$regs{$_}{$reg} || ()} @{$dominance_frontiers->{$block}};
                if (@phi >= 2) {
                    #die "phi function for $reg: ",join ',',map {$_->name} @phi;
                    my $new_reg = AST::Reg->new(name=>$reg."_".++$unique{$reg},real_name=>$reg);
                    $regs{$block}{$reg} = $new_reg;
                    unshift @{$block->stmts},AST::Assign->new(lvalue=>$new_reg,rvalue=>AST::Phi->new(regs=>\@phi));

                    $new_reg->type_info(TypeInfo::FromAssignment->new(orgin=>$block->stmts->[0]));
                } elsif (@phi) {
                    $regs{$block}{$reg} = $phi[0];
                }
            }
        }

        for my $stmt (@{$block->stmts}) {
            $stmt = transform_stmt({
                'AST::Reg'   => sub {
                    my ($reg) = @_;
                    if ($regs{$block}{$reg->name}) {
                        $regs{$block}{$reg->name};
                    } elsif ($reg->name =~ /^¢/) {
                        my $new_reg = AST::Reg->new(real_name=>$reg->name,name=>$reg->name);
                        $new_reg->type_info(TypeInfo::External->new());
                        $new_reg;
                    } elsif ($reg->name =~ /_\d+$/) {
                        $reg;
                    } else {
                        use Data::Dumper;
                        die $reg->name." is not in ".Dumper($regs{$block});
                    }
                },
                'AST::Block' => sub {
                    my ($block) = @_;
                    SSA::to_ssa($block);
                }
            },$stmt);
        };
    }

}
sub set_reg_orgins {
    my ($blocks) = @_;
    for my $block (@{$blocks}) {
        for my $stmt (@{$block->stmts}) {
            if ($stmt->isa('AST::Assign')) {
                $stmt->lvalue->type_info(TypeInfo::FromAssignment->new(orgin=>$stmt));
                if ($stmt->rvalue->isa('AST::Call')) {
                    my $capture = $stmt->rvalue->capture;
                    for my $reg ($capture->invocant,@{$capture->named},@{$capture->positional}) {
                        next unless $reg->isa('AST::Reg');
                        $reg->type_info->add_usage($stmt);
                    }
                }
            }
        }
    }
}
sub to_ssa {
    my ($mold,$types) = @_;
    my @blocks;
    my %blocks_by_id;
    flatten($mold,\@blocks,\%blocks_by_id);
    fix_jumps(\@blocks,\%blocks_by_id);
    implicit_jumps(\@blocks);
#to_graph(\@blocks);
    doms($mold,\@blocks,$types);
    set_reg_orgins(\@blocks);
    AST::Block::SSA->new(regs=>$mold->regs,stmts=>\@blocks); 
}
sub from_ssa {
    my ($mold) = @_;
    idhash my %unssa;
    for my $block (@{$mold->stmts}) {
        @{$block->stmts} = grep {
            if ($_->isa('AST::Assign') && $_->rvalue->isa('AST::Phi')) {
                $unssa{$_->lvalue} = 1;
                for my $reg (@{$_->rvalue->regs}) {
                    $unssa{$reg} = 1;
                }
                0;
            } else {
                1;
            }
        } @{$block->stmts};
    }
    for my $block (@{$mold->stmts}) {
        for my $stmt (@{$block->stmts}) {
            $stmt = transform_stmt({
                'AST::Reg' => sub {
                    my ($reg) = @_;       
                    $unssa{$reg} ? AST::Reg->new(name=>$reg->real_name) : $reg;
                },
                'AST::Block' => sub {
                     my ($block) = @_;       
                     from_ssa($block);
                     $block;
                }
            },$stmt);
        }
    }
}
sub implicit_jumps {
    my ($blocks) = @_;
    for my $i (0..@{$blocks}-1) {
        if (!$blocks->[$i]->jumps) {
            $blocks->[$i]->next($blocks->[$i+1]);
        }
    }
}
sub to_graph {
    my ($blocks) = @_;
    use Graph::Easy;
    my $graph = Graph::Easy->new();
    for my $block (@{$blocks}) {
        my $new = $graph->add_node($block->id);
        for ($block->jumps) {
            $graph->add_edge($new,$_->id);
        }
    }
    $graph;
}
sub fix_jumps {
    my ($blocks,$blocks_by_id) = @_;
    for my $block (@{$blocks}) {
        for my $stmt (@{$block->stmts}) {
            if ($stmt->isa('AST::Goto')) {
                $stmt->block($blocks_by_id->{$stmt->block->id});
            } elsif ($stmt->isa('AST::Branch')) {
                $stmt->then($blocks_by_id->{$stmt->then->id});
                $stmt->else($blocks_by_id->{$stmt->else->id});
            }
        }
    }
}
sub flatten {
    my ($flattened_thing,$blocks,$blocks_by_id) = @_;
    for (@{$flattened_thing->stmts}) {
        if ($_->isa('AST::Seq')) {
            if ($_->id) {
                my $block = AST::Seq->new(stmts=>[],id=>$_->id);
                push (@{$blocks},$block);
                $blocks_by_id->{$block->id} = $block;
            }
            flatten($_,$blocks,$blocks_by_id);
        } else {
            unless (@{$blocks}) {
                push (@{$blocks},AST::Seq->new(stmts=>[],id=>'start'));
            }
            push(@{$blocks->[-1]->stmts},$_);
        }
    }
}

=back
=cut
1;
