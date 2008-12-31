package AST::Helpers;
use Exporter 'import';
our @EXPORT = qw(string reg integer call FETCH lookup capturize let
                 routine code move_CONTROL XXX trailing_return varname EXPR);
use Carp 'confess';
use AST;
use strict;

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

sub capturize {
    my ($pos,$named) = @_;
    AST::Call->new(
        identifier => string "capturize",
        capture => AST::Capture->new(
            invocant => reg '?SMOP__S1P__Capturize',
            positional => $pos // [],
            named => $named // []
        )
    )
}

sub let {
    my ($value,$block) = @_;
    AST::Let->new(value=>$value,block=>$block);
}

sub routine {
  my ($mold, $sig) = @_;
  my $realcode = $mold->emit_m0ld;
  unshift @{$realcode->stmts},
    call(STORE => call('postcircumfix:{ }' => reg '$scope', [ string '&?ROUTINE' ]), [ call(continuation => reg '$interpreter') ]),
    call(STORE => call('postcircumfix:{ }' => reg '$scope', [ string '&?BLOCK' ]), [ call(continuation => reg '$interpreter') ]),
    call(set_control => call(continuation => reg '$interpreter'),
	 [
	  call new => FETCH(lookup('Code')),[],
	  [ string 'outer' => reg '$scope',
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
      ( $sig ? ( string 'signature' => $sig->emit_m0ld_ahsig ) : () )];
}

sub code {
  my ($mold,$sig) = @_;
  my $realcode = $mold->emit_m0ld;
  unshift @{$realcode->stmts},
    call(STORE=> call('postcircumfix:{ }' => reg '$scope', [ string '&?BLOCK' ]), [ call(continuation => reg '$interpreter') ]);

  call new => FETCH(lookup('Code')),[],
    [ string 'mold' => $realcode,
      string 'outer' => reg '$scope',
      ( $sig ? ( string 'signature' => $sig->emit_m0ld_ahsig ) : () )];
}

sub move_CONTROL {
    my $statementlist = shift;
    my @control;
    my @statementlist = 
      grep { !( exists $_->{statement_control} &&
		exists $_->{statement_control}{sym} &&
		($_->{statement_control}{sym} =~ /^(CONTROL|CATCH)$/) &&
	        (push @control, $_) ) }
	  @{$statementlist};
    if (@control) {
      unshift @statementlist, @control;
    }
    return @statementlist;
}

sub XXX {
    confess join ' ', 'unimplemented: ', @_;
}

sub trailing_return {
    my ($stmts,) = @_;
    $stmts->[-1] = call(setr => call(back=>call(continuation => reg '$interpreter')),[$stmts->[-1]]) if $stmts->[-1];
    [@{$stmts},call(goto => reg '$interpreter',[call back=>call(continuation => reg '$interpreter')])];
}

sub varname {
    my $m = shift;
    if ($m->{desigilname}) {
        $m->{sigil}{TEXT}.($m->{twigil}[0]{TEXT} || '').$m->{desigilname}{longname}->canonical;
    } elsif ($m->{sublongname}) {
        $m->{sigil}{TEXT}.($m->{twigil}[0]{TEXT} || '').$m->{sublongname}{subshortname}{desigilname}{longname}->canonical;
    } else {
        XXX;
    }
}

sub EXPR {
    my $m = shift;
    if ($m->{noun}) {
        my $noun = $m->{noun}->emit_m0ld;
        if ($m->{POST}) {
            for (@{$m->{POST}}) {
                if ($_->{dotty}) {
                    $noun = $_->{dotty}->emit_m0ld($noun);
		} elsif ($_->{postop}) {
		    if (my $pc = $_->{postop}{postcircumfix}) {
			if (ref $pc->{sym} eq 'ARRAY' &&
			    $pc->{sym}[0] eq '<' &&
			    $pc->{sym}[1] eq '>') {
			    my $nib = join '', @{$pc->{nibble}{nibbles}};
			    $noun = call 'postcircumfix:{ }' => $noun, [ string $nib ];
			} else {
			    XXX;
			}
		    } else {
			XXX;
		    }
                } else {
                    XXX;
                }
            }
            $noun;
        } else {
            $noun;
        }
    } else {
        XXX;
    }
} 

1;
