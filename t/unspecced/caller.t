use v6;

use Test;

# L<S06/Macros>

=begin pod

Unspecced: do macros introduce a new CALLER frame?  It seems like the
right answer is that closure macros should, but AST macros should not.

=end pod

plan 4;

sub current_line {
    return $?CALLER::LINE;
}

macro ast_compiling_current_line () {
    return quasi :COMPILING { current_line() };
}

macro ast_current_line () {
    return quasi { current_line() };
}

my $closure_line;

macro closure_current_line () {
    $closure_line = $?LINE; return { current_line() };
}


is current_line(), $?LINE,
    'sanity check, caller can get $?LINE';

is ast_compiling_current_line, $?LINE,
    'macros with COMPILING AST do not introduce new CALLER frame';

is ast_current_line, $?LINE,
    'macros with AST do not introduce new CALLER frame';

is closure_current_line, $closure_line,
    'macros with closures *do* introduce a new CALLER frame';
