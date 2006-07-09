use strict;
use inc::Module::Install;

name        'Sub-Multi';
all_from    'lib/Sub/Multi.pm';

requires    'Data::Bind' => '0.26';
requires    'Test::Exception';
requires    'Class::Multimethods::Pure' => '0.11';

sign; WriteAll;
