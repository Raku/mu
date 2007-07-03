package t::lib::Emitter;

use Test::Base -Base;
use t::lib::Util;

#use Smart::Comments;
use Pugs::Compiler::Regex;
use Pugs::Compiler::Token;
use Pugs::Grammar::Base;

our @EXPORT = qw( run_test run_tests );
$Pugs::Compiler::Regex::NoCache = 1;

my @updated_tests;
my $count = 0;

sub extract_snippet ($$) {
    my ($perl5, $type) = @_;
    ### $type
    if ($perl5 =~ m{^\s*## <$type>\n(.*?)\n\s*## </$type>\n}ms) {
        my $snippet = $1;
        $snippet =~ s{^(\s*## <)(\w+)>\n.*?\n\s*## </\2>\n}{$1$2 />\n}gms;
        return $snippet;
    }
    undef;
}

sub run_test ($) {
    my $block = shift;
    $t::BlockName = $block->name;
    my $updated_test = {
        name => $block->name,
    };
    my $rule;
    if (defined $block->token) {
        $updated_test->{token} = $block->token;
        my $token = parse_str_list($block->token);
        $rule = Pugs::Compiler::Token->compile($token);
    }
    my $perl5 = $rule->{perl5};
    ### $perl5;
    my @node_types;
    my $nhits = 0;
    while ($perl5 =~ /^\s*## <(\w+)>\n/gms) {
        my $node_type = $1;
        push @node_types, $node_type;
        if (defined (my $expected = $block->$node_type)) {
            $nhits++;
            my $got = extract_snippet($perl5, $node_type);
            $updated_test->{$node_type} = $got;
            $got =~ s/\n+$//g;
            $got =~ s/\$pad\{I\d+\}/\$pad{Ixxxx}/g;
            $expected =~ s/\n+$//g;
            $expected =~ s/\$pad\{I\d+\}/\$pad{Ixxxx}/g;
            is $got, $expected, "$t::BlockName - $node_type ok";
            $count++;
        }
    }
    if ($nhits == 0) {
        warn "$t::BlockName - no node type matched. perl5 source only contains the following node types: ", join(', ', @node_types), "\n";
    }
    push @updated_tests, $updated_test;
}

sub run_tests () {
    for my $block (blocks()) {
        run_test($block);
    }
}

END {
    (my $script = $0) =~ s{.*[/\\]}{}g;
    $script = "t/emitter/${script}_";
    open my $out, "> $script" or
        die "Can't open $script for writing: $!";
    print $out <<"_EOC_";
use t::lib::Emitter;

plan tests => $count;

run_tests;

_EOC_
    print $out "__DATA__\n\n";
    for my $block (@updated_tests) {
        print $out "=== $block->{name}\n";
        delete $block->{name};
        print $out "--- token: $block->{token}\n" if defined $block->{token};
        delete $block->{token};
        for my $section (sort keys %$block) {
            print $out "--- $section\n" . $block->{$section} . "\n";
        }
        print $out "\n\n\n";
    }
    warn "$script generated.\n";
    close $out;
}

1;

