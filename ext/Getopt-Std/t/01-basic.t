use v6-alpha;

use Test;

plan 15;

use Getopt::Std;

#&hashify := &Getopt::Std::hashify;

is Getopt::Std::hashify("abc").perl, (a=>0, b=>0, c=>0).perl, "no options";
is Getopt::Std::hashify("a:b:c:").perl, (a=>1, b=>1, c=>1).perl, "all options";
is Getopt::Std::hashify("ab:c:").perl, (a=>0, b=>1, c=>1).perl, "mixed options";

{
    my @cmdline = <-a -b -c -- leavemealone>;
    is_deeply getopts("abcd", @cmdline), {a=>1, b=>1, c=>1}, "basic getopts";
    is +@cmdline, 1, "consumed input up to --";
}

{
    my @cmdline = <-ac -b -- leavemealone>;
    is_deeply getopts("abcd", @cmdline), {a=>1, b=>1, c=>1}, "clustered getopts";
    is +@cmdline, 1, "consumed input up to --";
}

{
    my @cmdline = <-ac -b -- leavemealone>;
    is_deeply getopts("a:bd", @cmdline), {a=>"c", b=>1}, "getopts with opt (no space)";
    is +@cmdline, 1, "consumed input up to --";
}

{
    my @cmdline = <-a c -b -- leavemealone>;
    is_deeply getopts("a:bd", @cmdline), {a=>"c", b=>1}, "getopts with opt (space)";
    is +@cmdline, 1, "consumed input up to --";
}

{
    my @cmdline = <-a -c -b -- leavemealone>;
    is_deeply getopts("a:bd", @cmdline), {a=>"-c", b=>1},
               "getopts with opt that looks like a switch but isn't";
    is +@cmdline, 1, "consumed input up to --";
}

{
    @*ARGS = <-a -b -c -- leavemealone>;
    is_deeply getopts("abcd"), {a=>1, b=>1, c=>1}, "basic getopts defaults to @*ARGS";
    is +@*ARGS, 1, "consumed @*ARGS up to --";
}

