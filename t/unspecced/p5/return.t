#!/usr/bin/pugs

use v6;
use Test;
use Digest::MD5--perl5;

plan(1);

sub get_dmd5() {
    my $ctx = Digest::MD5.new;
    return($ctx);
}

{
    my $ctx = get_dmd5();
    $ctx.add('test');
    is( $ctx.hexdigest, '098f6bcd4621d373cade4e832627b4f6', 'XS return' );
}
