use strict;
use lib '.';
use inc::Module::Install;

name        'Data-Bind';
all_from    'lib/Data/Bind.pm';

requires    'PadWalker';
requires    'Devel::Caller' => '0.09';
requires    'Devel::LexAlias';
requires    'Class::Accessor::Fast';
requires    'Test::Exception';

sign; WriteAll;
