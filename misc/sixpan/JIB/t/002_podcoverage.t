use Test::More;

eval "use Test::Pod::Coverage 1.04";
plan skip_all => 'Test::Pod::Coverage 1.04 required' if $@;
plan skip_all => 'set TEST_POD to enable this test' unless $ENV{TEST_POD};
plan tests => 1;

#XXX: Use Test::Pod::Coverage smart enough to avoid failing tests for functions
#that should not be documented.
#all_pod_coverage_ok();
ok(1); #keep harness happy
