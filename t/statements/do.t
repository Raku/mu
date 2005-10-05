#!/usr/bin/pugs

use v6;
use Test;

plan 3;

{
    my $ret = do given 3 {
        when 3 { 1 }
    };
    is($ret, 1, 'do STMT works');
}

{
    my $ret = do { given 3 {
        when 3 { 1 }
    } };
    is($ret, 1, 'do { STMT } works');
}

{
    my $ret = eval 'do 42';
    ok(!$ret, 'do EXPR should not work', :todo);
    # XXX or should it? Feels weird...
}
