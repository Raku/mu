#!/usr/bin/perl6

use v6;
use Test;
use Config::Tiny;

plan 18;

# Test trivial creation
my Config::Tiny $Trivial .= new;
ok $Trivial,                 '.new returns true';
ok $Trivial ~~ Config::Tiny, '.new returns a Config::Tiny object';
ok keys $Trivial.cfg == 0,   '.new returns an empty object' );

# Try to read in a config
my Config::Tiny $Config .= read("test.conf");
ok $Config,                  '.read returns true';
ok $Trivial ~~ Config::Tiny, '.read returns a Config::Tiny object';

# Check the structure of the config
my $expected = {
  '_' => { root => 'something' },
  section => {
    one   => 'two',
    Foo   => 'Bar',
    this  => 'Your Mother!',
    blank => '',
  },
  "Section Two" => {
    "something else" => "blah",
    "remove"         => "whitespace",
  },
};
is_deeply $Config.cfg, $expected, 'Config structure matches expected';

# Add some stuff to the trivial config and check write_string() for it
$Trivial.cfg<_>       = { root1 => 'root2' };
$Trivial.cfg<section> = {
  foo   => "bar",
  this  => "that",
  blank => "",
};
$Trivial.cfg<section2> = {
  "this little piggy" => "went to market"
};
my $string = qq:to/END/;
root1=root2

[section]
blank=
foo=bar
this=that

[section2]
this little piggy=went to market
END

# Test read_string
my Config::Tiny $Read .= read_string($string);
ok $Read,                          '.read_string returns true';
is_deeply $Read.cfg, $Trivial.cfg, '.read_string returns expected value';

my $generated = $Trivial.write_string;
ok length $generated,     '.write_string returns something';
ok $generated eq $string, '.write_string returns the correct file contents';

# Try to write a file
my $rv = $Trivial.write("temp-test.conf");
ok $rv, '.write returned true';
ok -e "temp-test.conf", ".write actually created a file";

# Try to read the config back in
$Read = Config::Tiny.read("temp-test.conf");
ok $Read,                 '.read of what we wrote returns true';
ok $Read ~~ Config::Tiny, '.read of what we wrote returns a Config::Tiny object';
ok isa( $Read, 'Config::Tiny' ), '->read of what we wrote returns a Config::Tiny object';

# Check the structure of what we read back in
is_deeply $Read.cfg, $Trivial.cfg, 'What we read matches what we wrote out';

END {
  # Clean up
  unlink "temp-test.conf";
}

#####################################################################
# Bugs that happened we don't want to happen again

# Reading in an empty file, or a defined but zero length string, should yield
# a valid, but empty, object.
my Config::Tiny $Empty .= read_string("");
ok $Empty ~~ Config::Tiny;
ok keys $Empty.cfg == 0, 'Config::Tiny object from empty string, is empty';
