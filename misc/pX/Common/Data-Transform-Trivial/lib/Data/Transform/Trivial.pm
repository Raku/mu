package Data::Transform::Trivial;
use strict;
use warnings;
###l4p use Log::Log4perl qw(:easy);
use Data::Transform::Trivial::Context;
use Data::Transform::Trivial::Rule;

$Data::Transform::Trivial::VERSION='0.1';

sub import {
    if (@_>1 and $_[1] eq ':brief') {
        my $pkg=caller(0);
        no strict 'refs';
        *{"$pkg\::Transform::"}=\*Data::Transform::Trivial::;
        *{"$pkg\::Rule::"}=\*Data::Transform::Trivial::Rule::;
    }
}

sub new {
    my ($class,$rules)=(@_);
    return bless {rules=>($rules||[]),
                  context=>undef,
                  callers=>[undef],
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
    my $context=Data::Transform::Trivial::Context->new(\@nodes,0);
    for ($context->{position}=0;$context->{position}<@nodes;$context->{position}++) {
        my $rule=$self->find_rule($name,$context)
            or die "Can't find rule $name for @nodes\n";
        my @res;
        {
            @res=$rule->apply($self,$context);
###l4p             DEBUG "Rule returned @res\n";
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
###l4p     DEBUG "Looking for $name\n";
    my @candidates=grep {$_->matches($self,$name,$context)} @{$self->{rules}};
###l4p     DEBUG "We have @{[ scalar @candidates ]} candidates\n";
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
###l4p     DEBUG "We have a winner\n";
    return $winner;
}

1;
