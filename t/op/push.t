use v6;
require Test;

plan(6);

my @push;
eval 'push @foo, 42';
todo_ok(@push[0] == 42, "push");

eval '@foo.push(24)';
todo_ok(@push[0] == 42, "push");
todo_ok(@push[1] == 42, "push");

eval 'push @foo, 1, 2, 3';
todo_ok(@push[2] == 1, "push");
todo_ok(@push[3] == 2, "push");
todo_ok(@push[4] == 3, "push");
