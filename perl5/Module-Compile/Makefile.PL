use inc::Module::Install;

name            'Module-Compile';
all_from        'lib/Module/Compile.pm';

use_test_base;

clean_files     't/lib/*.pmc';

WriteAll;
