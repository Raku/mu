use strict;
use lib '.';
use inc::Module::Install;

name        'Data-Capture';
all_from    'lib/Data/Capture.pm';

requires    'Test::Exception';

sign; WriteAll;
