package SSA;
use Scalar::Util qw(refaddr);
use Set::Object ();
use List::MoreUtils qw(uniq);
use Hash::Util::FieldHash qw(idhash);
use v5.10;
use strict;
use warnings;
sub postorder {
    my ($node,$numbering,$visited,$number) = @_;
    $visited->{$node}++;
    for ($node->jumps) {
        postorder($_,$numbering,$visited,$number) unless $visited->{$_};
    }
    $numbering->{$node} = $$number++;
}
sub doms {
    my ($nodes) = @_;
    idhash my %idoms;
    idhash my %postorder;
    idhash my %predecessors;
    my $start = $nodes->[0];
    $idoms{$start} = $start;
    idhash my %visited;
    postorder($start,\%postorder,do {idhash my %visited;\%visited},\(my $number = 0));
    my @nodes = sort {$postorder{$b} <=> $postorder{$a}} @{$nodes};
    for my $node (@{$nodes}) {
        $predecessors{$node} = [];
    }
    for my $node (@{$nodes}) {
        for my $target ($node->jumps) {
            push @{$predecessors{$target}},$node;
        }
    }
    shift(@nodes);


    my $new_idom;
    my $changed = 1;
    while ($changed) { 
        $changed = 0;
        for my $b (@nodes) {
            my @predecessors = @{$predecessors{$b}};
            my $new_idom = shift @predecessors;
            for my $p (@predecessors) {
                if (defined $idoms{$p}) {
                    my $finger1 = $p;
                    my $finger2 = $new_idom;
                    while (refaddr $finger1 != refaddr $finger2) {
                        while ($postorder{$finger1} < $postorder{$finger2}) {
                            $finger1 = $idoms{$finger1};
                        }
                        while ($postorder{$finger2} < $postorder{$finger1}) {
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
    for my $node (@{$nodes}) {
        say "idom ",$node->id," = ",$idoms{$node}->id;
    }
    idhash my %dominace_frontier;
    for my $node (@{$nodes}) {
        next unless @{$predecessors{$node}} >= 2;
        for my $p (@{$predecessors{$node}}) {
            my $runner = $p;
            while (refaddr $runner != $idoms{$node}) {
                say $node->id," is in ",$runner->id," dominace frontier set";
                $runner = $idoms{$runner};
                push @{$dominace_frontier{$node}},$runner;
            }
        }
    }

    idhash my %alive_regs;
    my @blocks = sort {$postorder{$a} <=> $postorder{$b}} @{$nodes};
    for my $block (@blocks) {
        $alive_regs{$block} = Set::Object->new();
        for my $stmt (@{$block->stmts}) {
            if ($stmt->isa('AST::Assign')) {
                if ($stmt->rvalue->isa('AST::Call')) {
                    my $capture = $stmt->rvalue->capture; 
                    $alive_regs{$block}->insert(map {$_->name} grep {$_->isa('AST::Reg')} $capture->invocant,@{$capture->positional},@{$capture->named});
                }
            } elsif ($stmt->isa('AST::Branch')) {
                say "inserting ",$stmt->cond->name;
                #$alive_regs{$block}->insert($stmt->cond);
            }
        }
    }
    for my $block (@blocks) {
        for my $p ($block->jumps) {
            $alive_regs{$block} = $alive_regs{$block}->union($alive_regs{$p});
        }
    }
    for my $block (@blocks) {
        say $block->id;
        for my $reg ($alive_regs{$block}->members) {
            say "\t",$reg;
        }
    }

    my %unique;
    idhash my %regs;
    for my $block ($start,@nodes) {
        my $idom = $idoms{$block};
        for my $reg ($alive_regs{$idom}->members) {
            if ($regs{$idom}{$reg}) {
                $regs{$block}{$reg} = $regs{$idom}{$reg};
            }
            for (@{$dominace_frontier{$block}}) {
                
            }
        }

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
}
sub to_ssa {
    my ($mold) = @_;
    my @blocks;
    my %blocks_by_id;
    flatten($mold,\@blocks,\%blocks_by_id);
    fix_jumps(\@blocks,\%blocks_by_id);
    implicit_jumps(\@blocks);
    to_graph(\@blocks);
    doms(\@blocks);
    
#    for my $block (@blocks) {
#        say $block->pretty;
#    }
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
1;
