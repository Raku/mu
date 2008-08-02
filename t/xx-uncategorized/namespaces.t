use v6;

use Test;

plan 10;

=begin kwid

Namespace declarators

TODO:
* copy-paste this, replacing 'package' with 'module', then with 'class';
* and add tests for the lexical scoping behaviors of these declarations;
* somebody once mentioned runtime namespaces...?

=end kwid

our $GLOBAL = "Main global";

is($?PACKAGE, ::Main::, "default package is ::Main::");
is($GLOBAL, "Main global", "global var");

package A {

Test::is($?PACKAGE, "A", "switching package, file scope");
Test::is(eval('$Main::GLOBAL'), "Main global", "fully qualified name, Main::");
Test::lives_ok({ $GLOBAL = 1 }, "'our' is lexically scoped, even across namespaces");

eval '$A::GLOBAL = "A global"';

Test::is(eval('$A::GLOBAL'), "A global", "fully qualified name, other package");

}

package B {

Test::is($?PACKAGE, "B", "switching package, file scope");

eval '$B::GLOBAL = "A global"';

Test::is(eval('$A::GLOBAL'), "A global", "fully qualified name, other package");
Test::is(eval('$B::GLOBAL'), "A global", "fully qualified name, my own package");

eval '$B::UN_OURED = 1';

Test::is(eval('$B::UN_OURED'), undef, "can't refer to global outside scope when not qualified", :todo<feature>);

}
