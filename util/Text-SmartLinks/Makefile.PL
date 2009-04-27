use 5.006;
use strict;
use warnings;
use inc::Module::Install 0.77;


name           'Text-SmartLinks';
license        'artistic2';
author         'Gabor Szabo';
all_from       'lib/Text/SmartLinks.pm';

requires       'perl'                     => '5.006';
requires       'Carp'                     => 0;
requires       'Class::Accessor'          => 0;
requires       'Cwd'                      => 0;
requires       'File::Basename'           => 0;
requires       'File::Find::Rule'         => '0.30';
requires       'File::ShareDir'           => '1.00';
requires       'File::Slurp'              => 0;
requires       'File::Spec'               => 0;
requires       'FindBin'                  => 0;
requires       'Getopt::Long'             => 0;
requires       'Pod::Simple::HTML'        => 0;

test_requires  'Test::More'               => '0.47';
test_requires  'Test::NoWarnings'         => '0.084';


no_index       'directory'                => qw{ t xt eg share inc };

repository     'http://svn.pugscode.org/pugs/util/Text-SmartLinks';
install_script 'script/smartlinks.pl';
install_share;

WriteAll;
