use 5.006;
use inc::Module::Install;

name            'Module-Compile';
all_from        'lib/Module/Compile.pm';

requires        'Digest::SHA1';

use_test_base;

clean_files     't/lib/*.pmc';

WriteAll;
