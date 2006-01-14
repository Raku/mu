#!/usr/bin/pugs

use v6;
use Test;
use File::Spec;

plan 17;

use Config::Tiny; pass "(dummy instead of broken use_ok)";
try { chdir "ext/Config-Tiny" }; # Hack if we're run from `make smoke`

# Create a config

my $cfg = Config::Tiny::new();

# Open the config

ok($cfg<read>(catfile('t', 'config.ini')), '... read the config.ini file successfully');
ok(!defined($cfg<err_str>()), '... no errors');

# Reading properties 

is($cfg<data>(){''}<rootproperty>, 'blah', '... got the root property'); 

my $data = $cfg<data>();
isa_ok($data, 'Hash');

is($data<section><one>, 'two', '... got the right value for section/one');
is($data<section><three>, 'four', '... got the right value for section/three');
is($data<section><Foo>, 'Bar', '... got the right value for section/Foo');

ok(!$data<section><empty>, '... got the no value for section/empty');

# Changing data

$cfg<data>()<newsection> = { 'this' => 'that' }; # Add a section - Syntax below is nicer 
$data<section><Foo> = 'Not Bar!'; # Change a value

# Save a config

my $new_config_path = catfile('t', 'config_new.ini');

lives_ok {
    $cfg<write>($new_config_path);
}, '... writing the file did not die';
ok(-e catfile('t', 'config_new.ini'), '... the config_new.ini file exists');

my $new_config_text = slurp($new_config_path);
is($new_config_text, "rootproperty=blah
[newsection]
this=that
[section]
Foo=Not Bar!
empty=
one=two
three=four
", '... got the right config text');

ok(unlink($new_config_path), '... removing the new config file');
ok(!-e $new_config_path, '... the file is really gone');

# check some errors

dies_ok({ $cfg<read>() }, '... dont give it a file at all');

ok(!$cfg<read>('bad_file_name'), '... give it a bad file name');
is($cfg<err_str>(), "Failed to open 'bad_file_name' for reading", '... got the error we expected');
