package t::lib::AST;

use Test::Base -Base;
use Data::Dumper;
use Pugs::Grammar::Rule;
use t::lib::Util;

our @EXPORT = qw( run_tests );

my @saved_blocks;

sub run_tests () {
    for my $block (blocks()) {
        run_test($block);
    }
}

$Data::Dumper::Sortkeys = 1;

sub run_test ($) {
    my $block = shift;
    my $name = $block->name;
    my $regex = parse_str_list($block->regex);
    my $len = length($regex);
    my $ast = Pugs::Grammar::Rule->rule($regex)->();
    my $got = Dumper($ast);
    is $block->ast, $got, "$name - " . $block->regex . " - AST okay (len: $len)";
    push @saved_blocks, {
        name => $name,
        regex => $block->regex,
        ast => $got,
    };
}

END {
    #die "HEY!!!";
    return if $0 =~ /t_$/;
    my $outfile = $0 . "_";
    open my $out, "> $outfile" or
        die "Can't open $outfile for writing: $!";
    print $out <<'_EOC_';
use t::lib::AST;

plan tests => 1 * blocks();

run_tests();

_EOC_
    print $out "__DATA__\n\n";

    for my $block (@saved_blocks) {
        print $out <<_EOC_;
=== $block->{name}
--- regex: $block->{regex}
--- ast
$block->{ast}



_EOC_
    }
    close $out;
    $outfile =~ s/.*?([^\\\/]+)$/$1/;
    warn " $outfile generated.\n";
}

