use inc::Module::Install;

# Define metadata
name           'Syntax-Highlight-Perl6';
all_from       'lib/Syntax/Highlight/Perl6.pm';

# Specific dependencies
requires  'Moose'               => '0.79';
requires  'Term::ANSIColor'     => '2.00';
requires  'YAML::Syck'          => '1.07';
test_requires  'Test::Exception'     => '0.27';
test_requires  'Test::More'          => '0.86';

install_script 'script/hilitep6';

WriteAll;
