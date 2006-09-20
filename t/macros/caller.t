use v6-alpha;

use Test;

# L<S06/Macros>

=pod

Unspecced: do macros introduce a new CALLER frame?  It seems like the
right answer is that closure macros should, but AST macros should not.

=cut

sub current_line {
    return $?CALLER::LINE;
}

macro ast_compiling_current_line () {
    return q:code(:COMPILING) { current_line() };
}

macro ast_current_line () {
    return q:code { current_line() };
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
    'macros with closures do not introduce new CALLER frame';
