#!/usr/bin/pugs

# Tests for magic variables

use v6;
require Test;

plan 5;

# Tests for %*ENV

# It must not be empty at startup.
ok +%*ENV.keys, '%*ENV has keys';

# %*ENV should be copied into another variable.
my %vars = %*ENV;
is +%vars.keys, +%*ENV.keys, '%*ENV was successfully copied into another variable';

# XXX: Should modifying %vars affect the environment? I don't think so, but, of
# course, feel free to change the following test if I'm wrong.
%vars<PATH> = "42";
ok %*ENV<PATH> ne "42",
  'modifying a copy of %*ENV didn\'t affect the environment';

# Similarily, I don't think creating a new entry in %vars should affect the
# environment:
ok !defined(%*ENV<PUGS_ROCKS>), "there's no env variable 'PUGS_ROCKS'";
%vars<PUGS_ROCKS> = "42";
ok !defined(%*ENV<PUGS_ROCKS>), "there's still no env variable 'PUGS_ROCKS'";
