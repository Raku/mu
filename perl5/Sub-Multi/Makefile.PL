use strict;
use inc::Module::Install;

name        'Sub-Multi';
all_from    'lib/Sub/Multi.pm';

requires    'Data::Bind' => '0.26';
requires    'Test::Exception';

sign; WriteAll;
