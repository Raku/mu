use v6;
require Test;

=kwid

Config Tests

=cut

plan 1;

# If this test fails because your osname is not listed here, please add it.
# But don't add other osnames just because you know of them. That way we can
# get a list of osnames that have actually passed tests.
my $osnames = any(
    'darwin' | 
    'linux'
);
ok($?OSNAME eq $osnames, "Test \$?OSNAME == $?OSNAME");
