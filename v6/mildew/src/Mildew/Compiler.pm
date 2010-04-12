use v5.10;
use MooseX::Declare;
use AST;
use VAST;
BEGIN {
    for (<src/VAST/*.pm>) {
	require $_;
    }
}
class Mildew::Compiler {
    use AST::Helpers;
    has parser  => (is=>'ro');
    has backend => (is=>'ro');
    method ast($code) {
        my $ast = $self->parser->parse($code)->emit_m0ld;

        # load the setting
        my $load_CORE = call(load => call(new => FETCH(lookup 'MildewSOLoader')),
        [string 'CORE.mildew.so',FETCH(lookup('$LexicalPrelude'))]);
        unshift @{$ast->stmts},$load_CORE;
        $ast;
    }
    method run($code) {
        $self->backend->run($self->ast($code));
    }
    method compile($code,$output) {
        $self->backend->compile($self->ast($code),$output);
    }
}
