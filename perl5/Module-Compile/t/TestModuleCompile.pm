package t::TestModuleCompile;
use Test::Base -Base;

use lib 't/lib';

use Module::Compile();

package t::TestModuleCompile::Filter;
use base 'Test::Base::Filter';

sub process_pm {
    Module::Compile->pmc_process(shift);
}

sub parse_pm {
    Module::Compile->pmc_parse_blocks(shift);
}

sub yaml_dump {
    require YAML;
    YAML::Dump(@_);
}
