use v6-pugs;

use Test;
plan 1;


BEGIN {
unless try({ eval("1", :lang<perl5>) }) {
    skip_rest('no perl 5 support'); exit;
}
}

use perl5:Carp;

lives_ok({ try{ Carp.croak()}  },"Perl 5 exception (die) caught",:todo<bug>);
