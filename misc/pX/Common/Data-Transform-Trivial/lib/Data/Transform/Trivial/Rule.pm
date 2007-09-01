package Data::Transform::Trivial::Rule;
use strict;
use warnings;
use Carp;
###l4p use Data::Dumper;
###l4p use Log::Log4perl qw(:easy);

=head1 C<Rule->new($rule_name,\&matcher_sub,\&action_sub,$priority)>

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
    ($action,$action_pkg)=_qualify($action);
    ($matcher,$matcher_pkg)=_qualify($matcher);

    my $self=bless {name=>$name,
                    matcher=>$matcher,
                    matcher_pkg=>$matcher_pkg,
                    prio=>(defined($prio)?$prio:1),
                    action => $action,
                    action_pkg => $action_pkg,
                }, $class;
    return $self;
}

sub _caller_pkg {
    my $i=0;
    while (my $pack=caller($i++)) {
        return $pack unless $pack=~/^Data::Transform::Trivial/;
    }
    return '';
}

sub _qualify {
    my ($ref)=@_;
    my $caller=_caller_pkg();

    if (!defined $ref) { return (sub{1},$caller) }
    if (!ref($ref)) {
        my ($pack,$name)=($ref=~m{^((?:\w+::)+)(\w+)});
        $pack=defined($pack)?substr($pack,0,-2):$caller;
        my $sub=*{$ref}{CODE};
        if (!defined $sub) {
            croak "Unknown sub $ref";
        }
        return ($sub,$pack);
    }
    return ($ref,$caller);
}

=head1 C<$rule->apply($context)>

Calls the action sub, setting C<$_> to the current node,
C<$main::_POS> to the current position, and C<@_> to the current node
list.

=cut

sub apply {
    my ($self,$trans,$context)=@_;
###l4p     DEBUG "Applying $self->{name} ($self->{prio}), position $context->{position}\n";
    local $_=$context->current_nodes->[$context->position];
    my ($caller_P,$caller_T,$caller_C,$caller_L,$caller_OUTER)=do {
        my $pkg=$self->{action_pkg};
        no strict 'refs';
        \*{$pkg.'::_P'},
        \*{$pkg.'::_T'},
        \*{$pkg.'::_C'},
        \*{$pkg.'::_L'},
        \*{$pkg.'::_OUTER'},
    };
    local *$caller_T=\$trans;
    local *$caller_C=\$context;
    local *$caller_L=$context->current_nodes;

    local *$caller_P=\($context->{position});
    push @{$trans->{callers}},$context->current_nodes->[$context->position];
    local *$caller_OUTER=\($trans->{callers}[-2]);
    local *$caller_OUTER=$trans->{callers};
###l4p DEBUG ('@_OUTER:',{filter=>\&Dumper,value=>\@{*$caller_OUTER}});
###l4p DEBUG ('$_OUTER:',{filter=>\&Dumper,value=>\${*$caller_OUTER}});

    my @ret=$self->{action}->(@{$context->current_nodes});
    pop @{$trans->{callers}};
    return @ret;
}


=head1 C<$rule->matches($rule_name,$context)>

Returns true if this rule's name is C<$rule_name>, and the match sub
returns a true value.

The match sub is called setting C<$_> to the current node,
C<$main::_POS> to the current position, and C<@_> to the current node
list.

=cut

#'

sub matches {
    my ($self,$trans,$name,$context)=@_;
    return unless $name eq $self->{name};
    local $_=$context->current_nodes->[$context->position];
    my ($caller_P,$caller_T,$caller_C,$caller_L,$caller_OUTER)=do {
        my $pkg=$self->{matcher_pkg};
        no strict 'refs';
        \*{$pkg.'::_P'},
        \*{$pkg.'::_T'},
        \*{$pkg.'::_C'},
        \*{$pkg.'::_L'},
        \*{$pkg.'::_OUTER'},
    };
    local *$caller_T=\$trans;
    local *$caller_C=\$context;
    local *$caller_L=$context->current_nodes;
    local *$caller_P=\($context->{position});
    local *$caller_OUTER=@{$trans->{callers}} > 1 ? \($trans->{callers}[-2]) : \undef;
    local *$caller_OUTER=$trans->{callers};
    return $self->{matcher}->(@{$context->current_nodes});
}

1;
