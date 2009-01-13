use inc::Module::Install;

# Define metadata
name           'Syntax-Highlight-Perl6';
all_from       'lib/Syntax/Highlight/Perl6.pm';

# Specific dependencies
requires  'Moose'               => '0.64';        
requires  'Readonly'            => '1.03';
requires  'Term::ANSIColor'     => '1.12';
requires  'YAML::Syck'          => '1.05';
test_requires  'Test::Exception'     => '0.27';
test_requires  'Test::More'          => '0.42';

install_script 'script/hilitep6';

WriteAll;
