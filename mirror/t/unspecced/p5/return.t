#!/usr/bin/pugs

use v6;
use Test;

BEGIN {
    plan(1);

    unless eval 'eval("1", :lang<perl5>)' {
        skip_rest;
        exit;
    }
}

use perl5:Digest::MD5;

unless eval 'eval_perl5("1")' {
    skip_rest;
    exit;
}

use perl5:Digest::MD5;

sub get_dmd5() {
    my $ctx = Digest::MD5.new;
    return($ctx);
}

{
    my $ctx = get_dmd5();
    $ctx.add('test');
    is( $ctx.hexdigest, '098f6bcd4621d373cade4e832627b4f6', 'XS return' );
}
