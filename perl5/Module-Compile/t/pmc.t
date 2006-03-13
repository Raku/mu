use t::TestModuleCompile tests => 1;

use lib 't/lib';

BEGIN { unlink 't/lib/Foo.pmc' }

use Foo;

run_is module => 'pmc';

__DATA__
=== Compile Foo.pm to Foo.pmc
--- module read_file: t/lib/Foo.pmc
--- pmc -trim
# Generated .pmc file by Indented 0 (Module::Compile 0.12) - do not edit!
BEGIN { 1378133008 == do { use 5.006; local ($_, $/) = __FILE__; open _ or die "Cannot open $_: $!"; binmode(_, ':crlf'); unpack('%32L*', <_>) } or die "Cannot load stale .pmc file: ".__FILE__.'c' }
package Foo;


sub greetings {
    print "Hello " . shift;
}

sub farewell {
    print "Goodbye " . shift;
}

1;

__END__

