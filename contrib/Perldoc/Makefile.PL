use strict;
use inc::Module::Install;

name('Perldoc');
author('Brian Ingerson <ingy@cpan.org>');
abstract('Perl 6 Documentation System');
license('perl');
version_from('lib/Perldoc.pm');

requires(qw(
    perl 5.6.1
    Data::Dumper 0
    Scalar::Util 0
    Tree::DAG_Node 0
    Spiffy 0
    YAML 0
));

build_requires(qw(
    Test::More 0.54
));

check_nmake();

&WriteAll();
