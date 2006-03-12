use t::TestModuleCompile tests => 1;

use lib 't/lib';

BEGIN { unlink 't/lib/Foo.pmc' }

use Foo;

run_is module => 'pmc';

__DATA__
=== Compile Foo.pm to Foo.pmc
--- module read_file: t/lib/Foo.pmc
--- pmc -trim
# Generated .pmc file - do not edit!
package Foo;


sub greetings {
    print "Hello " . shift;
}

sub farewell {
    print "Goodbye " . shift;
}

1;

__END__

