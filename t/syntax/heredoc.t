#!/usr/bin/pugs

use v6;
use Test;

# L<S02/Literals/>

eval_is("
q:to/END/
  foo
  END
", "foo\n", "simple heredoc", :todo<feature>);
