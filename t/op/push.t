use v6;
require Test;

plan(7);

my @foo;
eval 'push @foo, 42';
todo_is(@foo[0], 42, "push @foo, 42 causes the empty list @foo to have a single element at index 0.");

eval '@foo.push(24)';
todo_is(@foo[0], 42, "@foo.push doesn't overwrite first element");
todo_is(@foo[1], 24, "@foo.push appends an item");

eval 'push @foo, 1, 2, 3';
is(@foo[2], 1, "@foo[2] == 1 after push");
is(@foo[3], 2, "@foo[3] == 2 after push");
is(@foo[4], 3, "@foo[4] == 3 after push");
is(+@foo, 5, "after all the pushes, @foo is 5 elements long.");
