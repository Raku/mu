#!/usr/bin/perl
use strict;
use warnings;
{
package OPP;
sub parse_expression {
    my ($self,$input) = shift;
    $self->parse_expression_1($self->parse_primary($input),0);
}
sub parse_expression_1 {
    my ($self,$lhs,$min_precedence) = @_;
    #print $self->lookahead,"prec:",$self->prec($self->lookahead),"\n";
    while ($self->lookahead ne 'end' && $self->prec($self->lookahead) >= $min_precedence) {
        my $op = $self->next_token;
        my $rhs = $self->parse_primary;
        while ($self->lookahead ne 'end' && ($self->prec($self->lookahead) > $self->prec($op) || ($self->prec($self->lookahead) == $self->prec($op) && $self->assoc($self->lookahead) eq 'right'))) {
            $rhs = $self->parse_expression_1($rhs,$self->prec($self->lookahead));
        }
        $lhs = "($lhs $op $rhs)";
    }
    return $lhs;
}
sub lookahead {
    my $self = shift;
    if ($self->{lookahead}) {
        $self->{lookahead};
    } else {
        $self->{lookahead} = $self->parse_op;
    }
}
sub next_token {
    my $self = shift;
    if ($self->{lookahead}) {
        my $tmp = $self->{lookahead};
        $self->{lookahead} = undef;
        return $tmp;
    } else {
        $self->parse_op;
    }
}
}

{
package OPP::Perl5;
our @ISA;
push(@ISA,'OPP');
sub new {
    my $self = shift;
    bless({},$self);
}
sub parse_primary {
    my $self = shift;
    $self->{str} =~ /\G\s*/gc;
    print pos($self->{str}),"\n";
    return $1 if $self->{str} =~ /\G(\d+)/gc;
}
sub parse_op {
    my $self = shift;
    $self->{str} =~ /\G\s*/gc;
    return $1 if $self->{str} =~ /\G(\S+)/gc;
    return 'end';
}
sub ok {
    my ($self,$input) = @_;
    $self->{str} = $input;
    pos($self->{str}) = 0;
    my $output = $self->parse_expression;
    print "input:$input output:$output\n";
    if (eval($output) == eval($input)) {
        print "ok\n";
    } else {
        print "not ok\n";
    }
}
my %prec = ('=='=>1,'+'=>2,'*'=>3,'/'=>3);
sub prec {
    my ($self,$op) = @_;
    die "undefined precedence of operator $op" unless $prec{$op};
    return $prec{$op};
}
my %assoc;
sub assoc {
    my ($self,$op) = @_;
    return $assoc{$op} || 'left';
}
}

sub say {
    print @_,"\n";
}

my $opp = OPP::Perl5->new();
#$opp->ok("8 / 2 * 4");
$opp->ok("1 + 2 + 3");
#$opp->ok("1 + 2 * 3 + 4 == 5 + 6 == 7 + 8 * 9");


__END__
pseudo-code of the algorithm taken from http://en.wikipedia.org/wiki/Operator-precedence_parser

parse_expression()
     return parse_expression_1 (parse_primary (), 0)

parse_expression_1 (lhs, min_precedence)
    while the next token is a binary operator whose precedence is >= min_precedence
        op := next token
        rhs := parse_primary ()
        while the next token is a binary operator whose precedence is greater
                 than op's, or a right-associative operator
                 whose precedence is equal to op's
            lookahead := next token
            rhs := parse_expression_1 (rhs, lookahead's precedence)
        lhs := the result of applying op with operands lhs and rhs
    return lhs
