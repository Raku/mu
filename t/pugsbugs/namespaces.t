#!/usr/bin/pugs

use v6;
use Test;

plan 10;

=kwid

Namespace declarators

TODO:
* copy-paste this, replacing 'package' with 'module', then with 'class';
* and add tests for the lexical scoping behaviors of these declarations;
* somebody once mentioned runtime namespaces...?

=cut

our $GLOBAL = "main global";

is($?PACKAGE, "main", "default package is 'main'");
is($GLOBAL, "main global", "global var");

package A;

Test::is($?PACKAGE, "A", "switching package, file scope");
Test::lives_ok({ $GLOBAL = 1 }, "'our' is lexically scoped, even across namespaces", :todo<feature>);
Test::eval_is('$main::GLOBAL', "main global", "fully qualified name, main::");

eval '$A::GLOBAL = "A global"';

Test::eval_is('$A::GLOBAL', "A global", "fully qualified name, other package");

package B;

Test::is($?PACKAGE, "B", "switching package, file scope");

eval '$B::GLOBAL = "A global"';

Test::eval_is('$A::GLOBAL', "A global", "fully qualified name, other package");
Test::eval_is('$B::GLOBAL', "A global", "fully qualified name, my own package");

eval '$B::UN_OURED = 1';

Test::is(eval('$B::UN_OURED'), undef, "can't refer to global outside scope when not qualified", :todo<feature>);

