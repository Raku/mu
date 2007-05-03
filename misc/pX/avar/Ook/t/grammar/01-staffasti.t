use Test;
use Ook::Grammar;

plan 8;

# Valid

<'a'> ~~ /<staffasti>/;
is($/<staffasti><stafur>, 'a');

<'\\'> ~~ /<staffasti>/;
is($/<staffasti><stafur>, '\\');

<'''> ~~ /<staffasti>/;
is($/<staffasti><stafur>, '\'');

<'\$07'> ~~ /<staffasti>/;
is($/<staffasti><stafur>, '$07');

<'"'> ~~ /<staffasti>/;
is($/<staffasti><stafur>, '"');

# Invalid

<'ab'> ~~ /<staffasti>/;
is($/<staffasti><stafur>, undef);

<"'"> ~~ /<staffasti>/;
is($/<staffasti><stafur>, undef);

<''''> ~~ /<staffasti>/;
is($/<staffasti><stafur>, undef);

<'""'> ~~ /<staffasti>/;
is($/<staffasti><stafur>, undef);
