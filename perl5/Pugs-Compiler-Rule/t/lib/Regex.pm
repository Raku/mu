package t::lib::Regex;

#use Smart::Comments;
use Test::Base -Base;
use t::lib::Util;

use Pugs::Compiler::Regex;
use Pugs::Compiler::Token;
use Pugs::Grammar::Base;

our @EXPORT = qw( run_test run_tests );

sub parse_str_list ($);
sub parse_res_section ($);
sub parse_p6_var ($);

sub run_test ($) {
    my $block = shift;
    $t::BloackName = $block->name;
    my ($pattern, $rule);
    if (defined $block->regex) {
        my $regex = $pattern = parse_str_list($block->regex);
        ## $regex
        ## $pattern
        $rule = Pugs::Compiler::Regex->compile($regex);
    } elsif (defined $block->token) {
        my $token = $pattern = parse_str_list($block->token);
        $rule = Pugs::Compiler::Token->compile($token);
    } else {
        die "ERROR: $t::BloackName: neither --- regex nor --- token specified.\n";
    }
    if (defined $block->match) {
        my $match;
        ## match: $block->match
        my @str = parse_str_list($block->match);
        my $i = 1;
        ## @str
        for my $str (@str) {
            no strict 'refs';
            # $match1, $match2, ...
            my $m = ${"match$i"} = $rule->match($str);
            # $match =:= $match1
            my $match = $m if $i == 1;

            ok $m ? 1 : 0, "$t::BloackName - match $i - $pattern <=> '$str'";

            my $meth = "res".$i;
            ### $meth
            my $res = $block->$meth;
            $res = $block->res if $i == 1 and !defined $res;
            ### $res
            if (defined $res) {
                my @res = parse_res_section($res);
                for my $res (@res) {
                    my $p5_var = $res->[0];
                    ### $p5_var
                    my $got = eval $p5_var;
                    is $got, $res->[1], "$t::BloackName - match $i - res $res->[2]";
                }
            }
        } continue {
            $i++;
        }
    }
    if (defined $block->unmatch) {
        my $unmatch;
        my @str = parse_str_list($block->unmatch);
        my $i = 1;
        for my $str (@str) {
            no strict 'refs';
            # $unmatch1, $unmatch2, ...
            my $m = ${"unmatch$i"} = $rule->match($str);
            # $unmatch =:= $unmatch1
            $unmatch = $m if $i == 1;

            ok $m ? 0 : 1, "$t::BloackName - unmatch - $pattern <=> $str";
        } continue {
            $i++;
        }
    }
}

sub run_tests () {
    for my $block (blocks()) {
        run_test($block);
    }
}

sub parse_res_section ($) {
    my $str = shift;
    open my $in, '<', \$str or
        die "ERROR: Can't open '$str' as file: $!";
    my @res;
    while (<$in>) {
        chomp;
        next if /^\s*$/;
        if (/^\s*(.*\S)\s*:\s*(.*)$/) {
            my ($var, $value) = ($1, $2);
            $var = parse_p6_var($var);
            $value = parse_str_list($value);
            push @res, [$var => $value => $_];
        } else {
            die "ERROR: $t::BloackName: syntax error in '--- res': $_";
        }
    }
    close $in;
    return @res;
}

sub parse_p6_var ($) {
    my $var = shift;
    local $_ = $var;
    my $coerce;
    if (s/^\$\(\s*(.*\S)\s*\)$/$1/) { # $(...)
        $coerce = 1;
    }
    s/^\s+|\s+$//g;
    my @token;
    if (m{\G \$ \( \) }xgc) { # $()
        push @token, '$m->()';
    }
    elsif (m{\G \$ / }xgc) { # $/
        push @token, '$m';
    }
    elsif (m{\G \$ (\d+) }xgc) { # $0, $1, ...
        push @token, "\$m->[$1]";
    }
    elsif (m{\G \$ < ([A-Za-z_]\w*) > }xgc) {
        push @token, "\$m->{$1}";
    }
    if (!@token) {
        die "ERROR: $t::BloackName - invalid p6 var name: $var (near the beginning)\n";
    }
    while (1) {
        if (/ \G \.? ( \[ \s* \d+ \s* \] ) /gcx) { # .[i]
            push @token, $1;
        }
        elsif (/ \G \.? \< \s* (.+?) \s* \> /gcx) { # .<key>
            push @token, "{$1}";
        }
        elsif (/ \G \.? ( \{ .*? \} ) /gcx) { # .{'key'}
            push @token, $1;
        }
        elsif (/ \G \. ( [A-Za-z_]\w* (?: \( \s* \) )? )/gcx) { # method()
            push @token, $1;
        }
        elsif (/ \G\S+/gc) {
            die "ERROR: $t::BloackName - res - invalid p6 var name: $var (near $&)\n";
        }
        else {
            last;
        }
    }
    push @token, '()' if $coerce;
    my $p5_var = join '->', @token;
    return $p5_var;
}

1;
