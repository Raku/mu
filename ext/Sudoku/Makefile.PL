use strict;
use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('Sudoku');
version    ('0.01');
abstract   ('Simple Sudoku solver');
author     ('Moritz Lenz <moritz@faui2k3.org>');
license    ('perl');

WritePugs  (6);

