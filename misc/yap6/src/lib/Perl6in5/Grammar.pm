package Perl6in5::Grammar;

use Filter::Simple;

FILTER {
    # Convert:  rule rulename {
    # To:       $rulename = execnow (implied sub) {
    # execnow() merely runs the sub, just as if it were declared
    # the normal way in MJD's grammar system.
    s/^\s+rule\s+([A-Za-z_])(\w*)\s+\{/\n\$$1$2 = execnow {/mg;
    s/'(.)'/hit('$1')/mg;
    s/\+\+/ + nothing/mg;
    s/--/ - nothing/mg; # this is kinda broken; try not to use it... :)
};

1;
