use v6-alpha;

use Test;


BEGIN {
plan 2;
unless try({ eval("1", :lang<perl5>) }) {
    skip_rest('no perl 5 support'); exit;
}
}

use perl5:Carp;

my $err;
lives_ok({ try{ Carp.croak() }; $err = $! }, "Perl 5 exception (die) caught");
like($err, rx:Perl5{Carp}, "Exception is propagated to Perl 6 land");
