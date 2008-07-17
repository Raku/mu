
require 't/re_tests.pl';

BEGIN {
    my $m = $ENV{RE_TESTS_MODULE} || 'remains_of_Regexp_ModuleA';
    eval("use $m;");
    die $@ if $@;
}

Pkg_re_tests::test(&Regexp::ModuleA::test_target);

1;
__END__
