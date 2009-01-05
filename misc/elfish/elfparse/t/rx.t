
require 't/rx.pl';

BEGIN {
    my $m = $ENV{RE_TESTS_MODULE} || 'remains_of_Regexp_ModuleA';
    eval("use $m;");
    die $@ if $@;
}

Pkg_re_tests::test6(&Regexp::ModuleA::test_target6);

1;
__END__
