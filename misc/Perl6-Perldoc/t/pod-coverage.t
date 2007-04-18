#!perl -T

BEGIN {
    $SIG{__WARN__} = sub {
        my ($message) = @_;
        return if !ref($message) && $message =~ m{Subroutine \w+ redefined};
        warn @_;
    };
}

use Test::More;
eval "use Test::Pod::Coverage 1.04";
plan skip_all => "Test::Pod::Coverage 1.04 required for testing POD coverage" if $@;
all_pod_coverage_ok();
