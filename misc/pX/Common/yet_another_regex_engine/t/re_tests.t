
require 't/re_tests.pl';

use Regexp::ModuleA;

Pkg_re_tests::test(&Regexp::ModuleA::test_target);

1;
__END__
