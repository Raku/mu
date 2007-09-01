package Data::Transform::Trivial;
use strict;
use warnings;
###l4p use Log::Log4perl qw(:easy);
use Data::Transform::Trivial::Context;
use Data::Transform::Trivial::Rule;
use Carp::Clan;

$Data::Transform::Trivial::VERSION='0.1';

=head1 NAME

Data::Transform::Trivial - simple tree transformation engine

=head1 SYNOPSIS

    use Data::Transform::Trivial ':brief';
    my $t=Transform->new(
        Rule->new('name',
                  sub {
                      # matcher
                  },
                  sub {
                      # action
                  }),
        # more rules
    );

    @results=$t->apply('name',$data_struct);

=head1 DESCRIPTION

This module implements a very simple tree transformation engine, whose
semantic is mostly copied from that of XSLT.

A transformation has named rules. Each rule defines which nodes in a
data structure it applies to, and what actions it performs. See
L<Data::Transform::Trivial::Rule> for details.

=head1 Importing

If imported with the C<:brief> tag, as seen in the synopsis, the
package names C<Transform> and C<Rule> will be aliases to
C<Data::Transform::Trivial> and C<Data::Transform::Trivial::Rule>,
respectively.

=cut

sub import {
    if (@_>1 and $_[1] eq ':brief') {
        *Transform::=\*Data::Transform::Trivial::;
        *Rule::=\*Data::Transform::Trivial::Rule::;
    }
}

=head1 METHODS

=head2 C<new(@rules)>

Returns a new transformation, containing the given rules. See
L<Data::Transform::Trivial::Rule> for details on how to build rules.

=cut

sub new {
    my $class=shift;
    return bless {rules=>(@_==1 and ref($_[0]) eq 'ARRAY')?$_[0]:[@_],
                  callers=>[undef],
              },$class;
}

=head2 C<apply($rule_name,@nodes)>

Applies the appropriate rules (named C<$rule_name>) to each node in
turn, and returns the list of results. See L</find_rule> to see how a
rule is selected.

The rules are evaluated in I<list> context.

=cut

sub apply {
    my ($self,$name,@nodes)=@_;

    my @result=();
    my $context=Data::Transform::Trivial::Context->new(\@nodes,0);
    for ($context->{position}=0;$context->{position}<@nodes;$context->{position}++) {
        my $rule=$self->find_rule($name,$context)
            or croak "Can't find rule $name for @nodes";
        my @res;
        {
            @res=$rule->apply($self,$context);
###l4p             DEBUG "Rule returned @res\n";
        }
        push @result,@res;
    }
    return @result;
}

=head2 C<find_rule($rule_name,$context)>

Mostly internal method.

Scans all the known rules, asking each in turn if they match the given
name and context, using the L<Data::Transform::Trivial::Rule/matches>
method.

Returns the one with the highest priority, or C<undef> if there's a
tie. A warning is printed in this case, since it's an error according
to the specification (the L</apply> method will die after seeing the
C<undef>).

=cut

sub find_rule {
    my ($self,$name,$context)=@_;
###l4p     DEBUG "Looking for $name\n";
    my @candidates=sort {$b->{prio} <=> $a->{prio}}
        grep {$_->matches($self,$name,$context)} @{$self->{rules}};
###l4p     DEBUG "We have @{[ scalar @candidates ]} candidates\n";
    return unless @candidates;

    if (@candidates>1 and $candidates[0]{prio}==$candidates[1]{prio}) {
        carp "Two rules (@{[%{$candidates[0]}]} and @{[%{$candidates[1]}]}) have the same priority!";
        return;
    }
###l4p     DEBUG "We have a winner\n";
    return $candidates[0];
}

1;
