package Transform;
use strict;
use warnings;
use Context;

sub new {
    my ($class,$rules)=(@_);
    return bless {rules=>($rules||[]),
                  context=>undef,
              },$class;
}

=head1 C<$_T->apply($rule_name,@nodes)>

Applies the appropriate rules (named C<$rule_name>) to each node in
turn, and returns the list of results.

The rules are evaluated in I<list> context.

=cut

sub apply {
    my ($self,$name,@nodes)=@_;

    my @result=();
    my $context=Context->new(\@nodes,0);
    for ($context->{position}=0;$context->{position}<@nodes;$context->{position}++) {
        my $rule=$self->find_rule($name,$context)
            or die "Can't find rule $name for @nodes\n";
        my @res;
        {
            local $main::_T=$self;
            local $main::_C=$context;
            @res=$rule->apply($context);
            warn "Rule returned @res\n";
        }
        push @result,@res;
    }
    return @result;
}

=head1 C<$_T->find_rule($rule_name,$context)>

Mostly internal method.

Scans all the known rules, asking each in turn if they match the given
name and context.

Returns the one with the highest priority, or C<undef> if there's a
tie.

=cut

sub find_rule {
    my ($self,$name,$context)=@_;
    warn "Looking for $name\n";
    my @candidates=grep {$_->matches($name,$context)} @{$self->{rules}};
    warn "We have @{[ scalar @candidates ]} candidates\n";
    return unless @candidates;
    my $winner=shift @candidates;
    for my $contestant (@candidates) {
        if ($contestant->{prio} > $winner->{prio}) {
            $winner=$contestant;
        }
        elsif ($contestant->{prio} == $winner->{prio}) {
            warn "Two rules (@{[%$winner]} and @{[%$contestant]}) have the same priority!\n";
            return;
        }
    }
    warn "We have a winner\n";
    return $winner;
}

=head1 C<$_T->find($context,$selector)>

Calls the C<$selector> for each current node in the C<$context>, and
returns the list of return values.

The selector is evaluated in I<list> context.

=cut

sub find {
    my ($self,$context,$selector)=@_;
    my @result=();
    for my $n (@{$context->current_nodes}) {
        my @ret;
        {
            local $_=$n;
            local $main::_POS=undef;
            @ret=$selector->(@{$context->current_nodes});
        }
        push @result,@ret;
    }
    return @result;
}

1;
