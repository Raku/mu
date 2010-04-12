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
    method run {
        die "USAGE: bread -e 'code'" unless @ARGV == 2 && $ARGV[0] eq '-e';
        my $ast = $self->parser->parse($ARGV[1]);
        my $mold = $ast->emit_m0ld;

        # load the setting
        my $load_CORE = call(load => call(new => FETCH(lookup 'MildewSOLoader')),
        [string 'CORE.mildew.so',FETCH(lookup('$LexicalPrelude'))]);
        unshift @{$mold->stmts},$load_CORE;

        $self->backend->run($mold);
    }
}
