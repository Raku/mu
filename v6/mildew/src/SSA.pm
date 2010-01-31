package SSA;
use Scalar::Util qw(refaddr);
use v5.10;
use strict;
use warnings;
sub postorder {
    my ($node,$output,$visited) = @_;
    $visited->{$node}++;
    for ($node->reachable) {
        postorder($node,$output,{}) unless $visited->{$_};
    }
    push @{$output},$node; 
}
sub doms {
    my ($start) = @_;
    state %doms;
    $doms{refaddr $start} = $start;
    my $changed = 1;
    my @nodes;
    postorder($start,\@nodes,{});
    pop(@nodes);
#    while ($changed) {
#        $changed = 0;
#        for my $b (@nodes) {
#            my $new_idom = ...;
#            for $p (predecessors($b)) {
#                if ($doms{$p}) {
#                }
#            }
#        }
#    }
}
sub to_ssa {
    my ($mold) = @_;
    my $blocks = [];
    flatten($mold,$blocks);
    use Graph::Easy;
    my $graph = Graph::Easy->new();
    my $unnamed = 0;
    my ($prev_node,$prev_block);
    #for (@{$blocks}) {
    #    say $_->pretty;
    #}
    for my $block (@{$blocks}) {
        my $new = $graph->add_node($block->id || "unnamed ".$unnamed++);
        if ($prev_block && !$prev_block->jumps) {
            $graph->add_edge($prev_node,$new);
        }
        for ($block->jumps) {
            $graph->add_edge($new,$_->id);
        }
        $prev_node = $new;
        $prev_block = $block;
    }
    say $graph->as_ascii;
}
sub flatten {
    my ($mold,$blocks) = @_;
    for (@{$mold->stmts}) {
        if ($_->isa('AST::Seq')) {
            if ($_->id) {
                push (@{$blocks},AST::Seq->new(stmts=>[],id=>$_->id));
            }
            flatten($_,$blocks);
        } else {
            unless (@{$blocks}) {
                push (@{$blocks},AST::Seq->new(stmts=>[]));
            }
            push(@{$blocks->[-1]->stmts},$_);
        }
    }

    #doms($ast);
}
1;
