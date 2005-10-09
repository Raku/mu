use lib "../../", "../../inc";
use inc::Module::Install prefix => '../../inc';

name       ('FA-DFA');
version    ('0.01');
abstract   ('An extremely simple Deterministic Finite Automata');
author     ('David Formosa');
license    ('perl');

WritePugs  (6);
