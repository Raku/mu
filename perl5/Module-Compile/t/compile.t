use t::TestModuleCompile tests => 1;

filters {
    perl => 'eval',
    pm => 'process_pm',
};

run_is pm => 'pmc';

__DATA__

=== Nested Compilers
--- perl
package Indento;
use Module::Compile -base;

sub pmc_compile {
    s/^(def \w+):(\n(?: +.*?\n)*)/$1 {$2}\n/g;
    PySubs->pmc_compile();
}

package PySubs;

sub pmc_compile {
    s/^def (\w+ \{)/sub $1/g;
}

package Y2K;
use Module::Compile -base;
sub pmc_compile { s/y/k/g }

--- pm
package Foo;
use strict;
use warnings;
# sample main script
drink();
use Indento;
def drink:
    print "Drink ";
    use Y2K;
    print "yool aid, ";
    no Y2K;
    print "yo!"

--- pmc
package Foo;
use strict;
use warnings;
# sample main script
drink();
sub drink {
    print "Drink ";
    print "kool aid, ";
    print "yo!"
}

=== Nested scopes
--- SKIP
--- perl
package Xxx;
use Module::Compile -base;
sub pmc_compile { s/\w/x/g; $_ }

package Yyy;
use Module::Compile -base;
sub pmc_compile { s/\w/y/g; $_ }

--- pm
use Xxx;
use Yyy;
blah blah blah
package A;
blah blah blah
no Xxx;
blah blah blah
no Yyy;
blah blah blah
package B;
blah blah blah
{
    no Xxx;
    no Yyy;
    blah blah blah
}
__END__
blah blah blah

--- pmc
xxxx xxxx xxxx
package A;
xxxx xxxx xxxx
yyyy yyyy yyyy
blah blah blah
package B;
xxxx xxxx xxxx
{
    blah blah blah
}
__END__
blah blah blah



