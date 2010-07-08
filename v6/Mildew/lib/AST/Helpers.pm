package AST::Helpers;
use Exporter 'import';
our @EXPORT = qw(string reg integer call FETCH lookup capturize let fcall name_components empty_sig routine code move_CONTROL XXX trailing_return varname lookupf curlies named_and_positional dump lookup_package YYY);
use Carp 'confess';
use Term::ANSIColor qw(:constants);
use PadWalker qw(peek_my);
use YAML::XS qw(Dump);
use utf8;
use strict;

sub YYY {
        use YAML::XS;
#        Mildew::prune($_[0]);
        die Dump($_[0]);
}
sub string($) {
    AST::StringConstant->new(value=>$_[0]);
}

sub reg($) {
    AST::Reg->new(name=>$_[0]);
}

sub integer($) {
    AST::IntegerConstant->new(value=>$_[0]);
}

sub call {
    AST::Call->new(identifier=>string($_[0]),capture=>AST::Capture->new(invocant => $_[1],positional => $_[2]//[],named => $_[3]//[]));
}

sub FETCH {
    my $arg = shift;
    call FETCH => $arg
}

sub lookup {
    my $thing = shift;
    call lookup => reg '$scope',[string $thing];
}
sub lookupf {
    FETCH(lookup(@_));
}

sub curlies {
    my $thing = shift;
    call 'postcircumfix:{ }' => reg '$scope',[string $thing];
}

sub fcall {
    my $func = shift;
    unless (ref $func) {
        $func = FETCH(lookup($func));
    }
    call 'postcircumfix:( )' => $func, [capturize(@_)];
}
sub capturize {
    my ($pos,$named) = @_;
    AST::Call->new(
        identifier => string "new",
        capture => AST::Capture->new(
            invocant => FETCH(lookup("capture")),
            positional => $pos // [],
            named => $named // []
        )
    )
}

sub let {
    my ($value,$block) = @_;
    AST::Let->new(value=>$value,block=>$block);
}

sub empty_sig {
  AST::Call->new
    ( identifier => string 'new',
      capture => AST::Capture->new
      ( invocant => FETCH(lookup('AdhocSignature')),
        positional => [],
        named =>
        [ string 'BIND' => AST::Block->new
          ( regs => [qw(interpreter scope capture)],
            stmts => trailing_return([]))]));
}

sub block_sig {
  AST::Call->new
    ( identifier => string 'new',
      capture => AST::Capture->new
      ( invocant => FETCH(lookup('AdhocSignature')),
        positional => [],
        named =>
        [ string 'BIND' => AST::Block->new
          ( regs => [qw(interpreter scope capture)],
            stmts => trailing_return([
                call BIND => curlies('$_'),[call positional => reg '$capture',[integer 0]] 
            ]))]));
}

sub routine {
  my ($mold, $sig) = @_;
  use YAML::XS;
  my $realcode = $mold->emit_m0ld;
  unshift @{$realcode->stmts},
    call(STORE => call('postcircumfix:{ }' => reg '$scope', [ string '&?ROUTINE' ]), [ call(continuation => reg '$interpreter') ]),
    call(STORE => call('postcircumfix:{ }' => reg '$scope', [ string '&?BLOCK' ]), [ call(continuation => reg '$interpreter') ]),
    call(set_control => call(continuation => reg '$interpreter'),
	 [
	  call new => FETCH(lookup('Code')),[],
	  [ 
            string 'signature' => block_sig(),
            string 'outer' => reg '$scope',
	    string 'mold' =>
	    AST::Block->new
	    ( regs => ['interpreter','scope'],
	      stmts =>
	      [ call( "setr" =>
		      ( call "back" => (call "continuation" => reg '$interpreter' )),
		      [ call( handle_return =>
			      call('new' => FETCH(lookup('ControlExceptionReturn'))),
			      [ FETCH(lookup('$_')),FETCH(lookup('&?ROUTINE')) ] )]),

		call( "goto" => reg '$interpreter',
		      [ call("back" => call("continuation" => reg '$interpreter'))])])]]);

  call new => FETCH(lookup('Code')),[],
    [ string 'mold' => $realcode,
      string 'outer' => reg '$scope',
      string 'signature' => $sig ];
}

sub code {
  my ($mold,$sig) = @_;
  my $realcode = $mold->emit_m0ld;
  unshift @{$realcode->stmts},
    call(STORE=> call('postcircumfix:{ }' => reg '$scope', [ string '&?BLOCK' ]), [ call(continuation => reg '$interpreter') ]);

    use YAML::XS;
  call new => FETCH(lookup('Code')),[],
    [ string 'mold' => $realcode,
      string 'outer' => reg '$scope',
      string 'signature' => ($sig ? $sig : empty_sig )];
}

sub move_CONTROL {
    my $statementlist = shift;
    my @statementlist;
    use v5.10;
    for (@{$statementlist}) {
        my $sc = $_->{statement_control};
        if (defined $sc && ($sc->isa('VAST::statement_control__S_CATCH') || $sc->isa('VAST::statement_control__S_CONTROL'))) {
            unshift @statementlist,$_;
        } else {
            push @statementlist,$_;
        }
    }
    return @statementlist;
}

sub XXX {
    my $where = '';
    my $m = peek_my(1)->{'$m'};
    if ($m && ref ${$m}) {
        my $back = ${$m}->{POS} > 200 ? 200 : ${$m}->{POS};
        my ($before,) = substr($::ORIG,${$m}->{POS}-$back,$back) =~ /( (?:.*\n)? (?:.*\n)? .* \n? )$/x;
        my ($after,) = substr($::ORIG,${$m}->{POS}) =~ /^(.* (?:\n.*)? (?:\n.*)? \n?)/x;
        $where = GREEN.$before.RED.$after.RESET;
        shift;
    }
    confess  "unimplemented: \n".$where.(join ' ',@_);
}

sub trailing_return {
    my ($stmts,) = @_;
    my @stmts = (@{$stmts});
    $stmts[-1] = call(setr => call(back=>call(continuation => reg '$interpreter')),[$stmts[-1]]) if $stmts[-1];
    [@stmts,call(goto => reg '$interpreter',[call back=>call(continuation => reg '$interpreter')])];
}

sub varname {
    my $var = shift;
    ($var->{sigil}{TEXT} || '') . $var->{desigilname}{longname}{name}{identifier}{TEXT};
}
sub name_components {
    my $m = shift;
    if ($m->{sublongname}) {
        $m->{sublongname}->components;
    } elsif ($m->{morename}) {
        ($m->{identifier}{TEXT},map {$_->{TEXT}} @{$m->{morename}[0]{identifier}});
    } elsif ($m->{desigilname}) {
        $m->{desigilname}{longname}->components;
    } else {
        XXX;
    }
}

sub named_and_positional {
    [grep { ref $_ ne 'AST::Pair' } @_],[map { $_->key, $_->value } grep { ref eq 'AST::Pair' } @_]
}


sub lookup_package {
    my $package = lookup(shift(@_).'::');
    for my $part (@_) {
        $package = call('postcircumfix:{ }'=>FETCH($package),[string($part.'::')]);
    }
    $package;
}

1;
