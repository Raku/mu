#!/usr/bin/perl
use Test::Simple;
use strict;
use warnings;
my $input = "1 + 2 * 3 + 4 == 5 + 6 == 7 + 8 * 9";
sub parse_primary {
    $input =~ /\s*/gc;
    return $1 if $input =~ /(\d+)/gc;
}
sub say {
    print @_,"\n";
}
{
    my $lookahead;
    sub lookahead_token {
        if ($lookahead) {
            $lookahead;
        } else {
            $lookahead = next_token();
        }
    }
    sub next_token {
        if ($lookahead) {
            #print "lookahead = $lookahead\n";
            my $tmp = $lookahead;
            $lookahead = undef;
            return $tmp;
        } else {
            $input =~ /\s*/gc;
            return $1 if $input =~ /(\S+)/gc;
            return 'end';
        }
    }
}
my %assoc = ('=='=>'right');
my %prec = ('=='=>1,'+'=>2,'*'=>3);
sub parse_expression_1 {
    my ($lhs,$min_precedence) = @_;
    #print "parse_expression_1($lhs,$min_precedence)\n";
    while (lookahead_token() ne 'end' && $prec{lookahead_token()} >= $min_precedence) {
        my $op = next_token();
        die "undefined precedence of operator $op" unless $prec{$op};
        #print "op: $op\n";
        my $rhs = parse_primary();
        while (lookahead_token() ne 'end' && ($prec{lookahead_token()} > $prec{$op} || ($prec{lookahead_token()} == $prec{$op} && $assoc{lookahead_token()} && $assoc{lookahead_token()} eq 'right'))) {
            $rhs = parse_expression_1($rhs,$prec{lookahead_token()});
        }
        $lhs = "($lhs $op $rhs)";
    }
    return $lhs;
}
my $expr = parse_expression_1(parse_primary(),0);
say $expr;
print if $@;

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
