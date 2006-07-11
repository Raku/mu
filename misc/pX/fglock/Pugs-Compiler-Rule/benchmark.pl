# Perl 6 Rule benchmark
# - fglock
#

use strict;
use warnings;
use Data::Dumper;

use Benchmark;
use Pugs::Compiler::Rule;
use Pugs::Compiler::Token;
use Pugs::Compiler::Regex;
use Pugs::Compiler::RegexPerl5;
my $rul = '[a|b](b)c';
my $rpc_rule  = Pugs::Compiler::Rule->compile( $rul );
my $rpc_token = Pugs::Compiler::Token->compile( $rul );
my $rpc_regex = Pugs::Compiler::Regex->compile( $rul );
my $rpc_regexP5 = Pugs::Compiler::RegexPerl5->compile( $rul );
#print $rpc_token->perl5, "\n";

{
    print "P5 regex benchmark:\n";
    my $str = "abcdefg";
    my $match;
    Benchmark::cmpthese(500, {
        substr         => sub{ $match = (substr($str,3,1) eq 'd') for 1..4000},
        regex          => sub{ $match = ($str =~ /^.{3}d/os)      for 1..4000},
    });
}

{
    print "P6 Rule Benchmark:\n";
    my $str = "abcdefg";
    my $match;
    Benchmark::cmpthese(500, {
        P6_Rule    => sub{ $match = $rpc_rule->match($str)    for 1..40},
        P6_Token   => sub{ $match = $rpc_token->match($str)   for 1..40},
        P6_Regex   => sub{ $match = $rpc_regex->match($str)   for 1..40},
        P6_RegexP5 => sub{ $match = $rpc_regexP5->match($str) for 1..40},
        P5_regex_x10 => sub{ 
                   $match = sub{$_[0] =~ /[a|b](b)c/os}->($str) for 1..400},
    });
    #print "Str = $str\n";
}

