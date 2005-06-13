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

is($?PACKAGE, "A", "switching package, file scope", :todo<feature>);
is($GLOBAL, "main global", "'our' is lexically scoped, even across namespaces");
eval_is('$main::GLOBAL', "main global", "fully qualified name, main::", :todo<feature>);
#is($::GLOBAL, "main global", "fully qualified name, main");

eval '$A::GLOBAL = "A global"';

eval_is('$A::GLOBAL', "A global", "fully qualified name, other package", :todo<feature>);

package B;

is($?PACKAGE, "B", "switching package, file scope", :todo<feature>);

eval '$B::GLOBAL = "A global"';

eval_is('$A::GLOBAL', "A global", "fully qualified name, other package", :todo<feature>);
eval_is('$B::GLOBAL', "A global", "fully qualified name, my own package", :todo<feature>);

eval '$B::UN_OURED = 1';

is(eval('$B::UN_OURED'), undef, "can't refer to global outside scope when not qualified");

