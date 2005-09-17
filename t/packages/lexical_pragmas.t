#!/usr/bin/pugs

use v6;
use Test;

plan 25;

BEGIN { @*INC.unshift('t/packages'); }

sub bar {
  is(Pugs::Internals::current_pragma_value('pragma::Demo'), undef, "Scope is lexical, not dynamic");
}


use pragma::Demo 'x1';
is(Pugs::Internals::current_pragma_value('pragma::Demo'), '1', "Outer value is set");
{
  is(Pugs::Internals::current_pragma_value('pragma::Demo'), '1', "Outer value is inherited");
  use pragma::Demo 'x2';
  is(Pugs::Internals::current_pragma_value('pragma::Demo'), '2', "Inner value is set");
  bar();
}
is(Pugs::Internals::current_pragma_value('pragma::Demo'), '1', "Outer value is restored");
use pragma::Demo 'x3';
is(Pugs::Internals::current_pragma_value('pragma::Demo'), '3', "Outer value is overidden");
bar();

{
  is(Pugs::Internals::current_pragma_value('pragma::Demo'), '3', "Outer value is inherited");
  {
    is(Pugs::Internals::current_pragma_value('pragma::Demo'), '3', "Outer value is inherited");
    use pragma::Demo 'x4';
    is(Pugs::Internals::current_pragma_value('pragma::Demo'), '4', "Inner value is overridden");
    bar();
  }   
  is(Pugs::Internals::current_pragma_value('pragma::Demo'), '3', "Inner value is restored");
  use pragma::Demo 'x5';
  is(Pugs::Internals::current_pragma_value('pragma::Demo'), '5', "Inner value is overridden");
  bar();
}
is(Pugs::Internals::current_pragma_value('pragma::Demo'), '3', "Outer value is restored");
is(Pugs::Internals::current_pragma_value('SNONK'), undef, "Absent pragma is undef");
bar();

sub foo {
  check_cpv(3);
  use pragma::Demo 'x6';
  check_cpv(6);
  {
    check_cpv(6);
    use pragma::Demo 'x7';
    check_cpv(7);
    bar();
  }
  check_cpv(6);
  use pragma::Demo 'x8';
  check_cpv(8);
  bar();
}

sub check_cpv ($expected) {
  is(Pugs::Internals::caller_pragma_value('pragma::Demo'), $expected, 
        "Caller pragma value $expected");
}

foo();

