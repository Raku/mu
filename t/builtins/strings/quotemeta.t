use v6-alpha;

# vim: filetype=perl6 :

use Test;

plan 5;

# L<S29/"Perl6::Str" /quotemeta/>

is(quotemeta("HeLLo World-72_1"), "HeLLo\\ World\\-72_1", "simple lc test");
is(quotemeta(""), "", "empty string");

$_ = "HeLLo World-72_1"; 
my $x = .quotemeta;
is($x, "HeLLo\\ World\\-72_1", 'quotemeta uses $_ as default');

{ # test invocant syntax for quotemeta
    my $x = "HeLLo World-72_1";
    is($x.quotemeta, "HeLLo\\ World\\-72_1", '$x.quotemeta works');
    is("HeLLo World-72_1".quotemeta, "HeLLo\\ World\\-72_1", '"HeLLo World-72_1".quotemeta works');
}
