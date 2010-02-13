package SSA;
use Scalar::Util qw(refaddr);
use Set::Object ();
use List::MoreUtils qw(uniq);
use Hash::Util::FieldHash qw(idhash);
use v5.10;
use strict;
use warnings;

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

sub value_to_ssa {
    my ($regs,$value) = @_;
    if ($value->isa('AST::Reg')) {
        use Data::Dumper;
        die $value->name." is not in ".Dumper($regs) unless $regs->{$value->name};
        $regs->{$value->name};
    } elsif ($value->isa('AST::Call')) {
        AST::Call->new(identifier=>$value->identifier,capture=>value_to_ssa($regs,$value->capture));
    } elsif ($value->isa('AST::Capture')) {
        AST::Capture->new(
            ($value->invocant ? (invocant => value_to_ssa($regs,$value->invocant)) : ()),
            positional => [map {value_to_ssa($regs,$_)} @{$value->positional}],
            named=>[map {value_to_ssa($regs,$_)} @{$value->named}]
        );
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
                $alive_regs{$block}->insert($stmt->cond) if $stmt->cond->isa('AST::Reg');
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
                $runner = $idoms->{$runner};
                push @{$dominance_frontiers{$block}},$runner;
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
    my ($mold,$blocks) = @_;

    my ($numbering,$postorder,$rpostorder) = postorder($blocks);

    my $predecessors = predecessors($blocks);
    my $idoms = idoms($rpostorder,$numbering,$predecessors);
    my $dominance_frontiers = dominance_frontiers($blocks,$idoms,$predecessors);
    my $alive_regs = alive_regs($postorder);

    my %unique;
    idhash my %regs;
    for (@{$mold->regs}) {
        $regs{$blocks->[0]}{'$'.$_} = AST::Reg->new(name=>'$'.$_);
    }
    # XXX - assigning to the value twice in the same block
    for my $block (@{$postorder}) {
        for my $stmt (@{$block->stmts}) {
            if ($stmt->isa('AST::Assign')) {
                my $name = $stmt->lvalue->name;
                $unique{$name}++;
                my $reg = AST::Reg->new(name=>$name."_".$unique{$name});
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
                if (@phi > 2) {
                    say "phi function for $reg";
                } elsif (@phi) {
                    $regs{$block}{$reg} = $phi[0];
                }
            }
        }

        for my $stmt (@{$block->stmts}) {
            if ($stmt->isa('AST::Assign')) {
                $stmt = AST::Assign->new(lvalue=>$stmt->lvalue,rvalue=>value_to_ssa($regs{$block},$stmt->rvalue));
            } elsif ($stmt->isa('AST::Branch')) {
                $stmt = AST::Branch->new(then=>$stmt->then,else=>$stmt->else,cond=>value_to_ssa($regs{$block},$stmt->cond));
            } else {
                $stmt = value_to_ssa($regs{$block},$stmt);
            }
        }
    }
}
sub to_ssa {
    my ($mold) = @_;
    my @blocks;
    my %blocks_by_id;
    flatten($mold,\@blocks,\%blocks_by_id);
    fix_jumps(\@blocks,\%blocks_by_id);
    implicit_jumps(\@blocks);
#    to_graph(\@blocks);
    doms($mold,\@blocks);
    AST::Block->new(regs=>$mold->regs,stmts=>\@blocks); 
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
    say $graph->as_ascii;
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
