package Indented;

use Module::Compile -base;

sub pmc_compile {
    s/^(sub \w+):(\n(?: +.*?\n)*)/$1 {$2}\n/gm;
}

1;
