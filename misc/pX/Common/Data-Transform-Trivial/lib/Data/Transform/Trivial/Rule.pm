package Data::Transform::Trivial::Rule;
use strict;
use warnings;
use Carp::Clan;
###l4p use Data::Dumper;
###l4p use Log::Log4perl qw(:easy);

=head1 NAME

Data::Transform::Trivial::Rule - a rule in a tree transformation system

=head1 SYNOPSIS

  Rule->new('name',
            sub {
                is_ok($_);
            },
            sub {
                do_something_to($_);
                $_T->apply('another',@other_nodes);
            },
            1);

=head1 DESCRIPTION

A rule is defined by a name, a matching function, an action
function, and a priority.

The name is used to group together rules used to the same end.

The matching function is called to determine if a rule can be applied
to a given node in the data structure being transformed (see
L</matches> for details).

The action function is called to perform the actual data
manipulation. It should return the result nodes, and it can
recursively apply the transformation (see </apply> for details).

The priority is needed to disambiguate rules with the same name that
match the same node (for example, a generic rule and a more specific
one).

=head1 METHODS

=head2 C<new($rule_name,\&matcher_sub,\&action_sub,$priority)>

Creates a new rule object. The subs can be either sub references, or
sub names (strings). In the first case, the subs should be defined in
the calling package, otherwise the "L<exported globals|/Exported
globals>" will not be accessible. Use the names if you really want to
use subs from another package (will die if the sub is not defined).

Defaults:

=over 4

=item C<&matcher_sub>

  sub {1} # match anything

=item C<&action_sub>

  sub {} # do nothing

=item C<$priority>

  1

=back

=cut

sub new {
    my ($class,$name,$matcher,$action,$prio)=(@_);

    my ($action_pkg,$matcher_pkg);
    ($action,$action_pkg)=_qualify($action,sub{});
    ($matcher,$matcher_pkg)=_qualify($matcher,sub{1});

    my $self=bless {name=>$name,
                    matcher=>$matcher,
                    matcher_pkg=>$matcher_pkg,
                    prio=>(defined($prio)?$prio:1),
                    action => $action,
                    action_pkg => $action_pkg,
                }, $class;
    return $self;
}

# internal functions to get the proper package for a sub

# stolen from Carp::Clan
sub _caller_pkg {
    my $i=0;
    while (my $pack=caller($i++)) {
        return $pack unless $pack=~/^Data::Transform::Trivial/;
    }
    return '';
}

sub _qualify {
    my ($ref,$default)=@_;
    my $caller=_caller_pkg();

    # 'undef' gives the default
    if (!defined $ref) { return ($default,$caller) }

    # a string?
    if (!ref($ref)) {
        my ($pack,$name)=($ref=~m{^((?:\w+::)+)(\w+)});
        # take the caller if not qualified
        $pack=defined($pack)?substr($pack,0,-2):$caller;
        my $sub=*{$ref}{CODE};
        if (!defined $sub) {
            croak "Unknown sub $ref";
        }
        return ($sub,$pack);
    }

    # a plain subref: assume caller's package
    return ($ref,$caller);
}

=head2 C<apply($trans,$context)>

Calls the action sub in list context, and returns the results. See
L</Exported Globals> for the list of accessible variables.

=cut

sub apply {
    my ($self,$trans,$context)=@_;
###l4p     DEBUG "Applying $self->{name} ($self->{prio}), position $context->{position}\n";
    local $_=$context->current_nodes->[$context->position];
    my ($caller_P,$caller_T,$caller_L,$caller_OUTER)=do {
        my $pkg=$self->{action_pkg};
        no strict 'refs';
        \*{$pkg.'::_P'},
        \*{$pkg.'::_T'},
        \*{$pkg.'::_L'},
        \*{$pkg.'::_OUTER'},
    };
    local *$caller_T=\$trans;
    local *$caller_L=$context->current_nodes;

    local *$caller_P=\($context->{position});
    push @{$trans->{callers}},$context->current_nodes->[$context->position];
    local *$caller_OUTER=\($trans->{callers}[-2]);
    local *$caller_OUTER=$trans->{callers};
###l4p DEBUG ('@_OUTER:',{filter=>\&Dumper,value=>\@{*$caller_OUTER}});
###l4p DEBUG ('$_OUTER:',{filter=>\&Dumper,value=>\${*$caller_OUTER}});

    my @ret=$self->{action}->();
    pop @{$trans->{callers}};
    return @ret;
}

=head2 C<matches($trans,$rule_name,$context)>

Returns true if this rule's name is C<$rule_name>, and the matcher sub
returns a true value. See L</Exported Globals> for the list of
accessible variables.

=cut

#'

sub matches {
    my ($self,$trans,$name,$context)=@_;
    return unless $name eq $self->{name};
    local $_=$context->current_nodes->[$context->position];
    my ($caller_P,$caller_T,$caller_L,$caller_OUTER)=do {
        my $pkg=$self->{matcher_pkg};
        no strict 'refs';
        \*{$pkg.'::_P'},
        \*{$pkg.'::_T'},
        \*{$pkg.'::_L'},
        \*{$pkg.'::_OUTER'},
    };
    local *$caller_T=\$trans;
    local *$caller_L=$context->current_nodes;
    local *$caller_P=\($context->{position});
    local *$caller_OUTER=@{$trans->{callers}} > 1 ? \($trans->{callers}[-2]) : \undef;
    local *$caller_OUTER=$trans->{callers};
    return $self->{matcher}->();
}

=head1 Exported Globals

These variables can be used in the matcher and action sub of any rule:

=over 4

=item C<$_>

The current node

=item C<@_L>

The current node list (i.e. the argument of the call to
L<Data::Transform::Trivial/apply> that in turn called this rule)

=item C<$_P>

The current position (i.e. the index of C<$_> in C<@_L>)

=item C<$_OUTER>

The node that was current when the last call to
L<Data::Transform::Trivial/apply> was made; C<undef> if this rule was
called during the "top" apply.

=item C<@_OUTER>

The list of the C<$_OUTER>s, recursively from the top; C<$_OUTER[0]>
is C<undef>, and C<$_OUTER[-1]> is C<$_>

=item C<$_T>

The transformation object; useful for recursive calls:
C<$_T->apply('name',@nodes)>

=cut

1;
