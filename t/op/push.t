use v6;
require Test;

plan(6);

my @foo;
eval 'push @foo, 42';
todo_ok(@foo[0] == 42, "push");

eval '@foo.push(24)';
todo_ok(@foo[0] == 42, "push");
todo_ok(@foo[1] == 24, "push");

eval 'push @foo, 1, 2, 3';
todo_ok(@foo[2] == 1, "push");
todo_ok(@foo[3] == 2, "push");
todo_ok(@foo[4] == 3, "push");
