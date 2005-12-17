#!/usr/bin/pugs

use v6;
use Test;

plan(2);

# XXX: can't catch exception from try { use perl5: }
unless try { use jsperl5:Digest::MD5 <md5_hex>; 1 } {
    skip_rest $!;
    exit;
}

sub get_dmd5() {
    my $ctx = Digest::MD5.new;
    return($ctx);
}

{
    is( md5_hex('test'), '098f6bcd4621d373cade4e832627b4f6', 'perl5 function exported' );
}

{
    my $ctx = get_dmd5();
    $ctx.add('test');
    is( $ctx.hexdigest, '098f6bcd4621d373cade4e832627b4f6', 'XS return' );
}
