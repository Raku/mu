# Ideas for Perl 6 Google Summer of Code 2015 projects


## Rakudo

* An improved REPL (read-eval-print-loop). I.e. one that supports breaking up a statement into several lines
  and executing it only at a 'safe point'. The history support also needs improvements or implementation
  depending on the platform. Finally, there are a bunch of bug reports, that need to be addresses.


## MoarVM

* A tool for bundling MoarVM with bytecode, preferably a general one, but at least giving a way to provide
  Perl 6 fakexecutables, to be able to distribute your code along with all its dependencies so nothing on the
  end users' systems needs to be installed.
* Optimization and JIT-compilation of native calls (today, our calls into C code are not the fastest; with
  some work we can do far better on this. Involves work in the VM itself, also maybe some improvements to
  the NQP native calling API to facilitate it.

## Test suite



## Native library bindings

* An automatic binding generator for gobject based libraries
* An OpenGL binding based on the API description XML file

## Ecosystem



and more...
