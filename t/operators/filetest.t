#!/usr/bin/pugs

use v6;
require Test;

plan 6;

ok(-d 't/', 'we have a t/ directory');
ok(-f 't/operators/file_test_ops.t', 'we do have a t/operators/file_test_ops.t file');

ok(!(-d 'test/'), 'we dont have a test/ directory');
ok(!(-f 'test/test.t'), 'we dont have a test/test.t file');

# need to tweak the precedence on this
todo_eval_ok(eval "!-d 'test/'", 'we dont have a test/ directory');
todo_eval_ok(eval "!-f 'test/test.t'", 'we dont have a test/test.t file');