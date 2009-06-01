package AST::Helpers;
use Exporter 'import';
our @EXPORT = qw(string reg integer call FETCH lookup capturize let fcall name_components empty_sig
                 routine code move_CONTROL XXX trailing_return varname EXPR lookupf);
use Carp 'confess';
use AST;
use Term::ANSIColor qw(:constants);
use PadWalker qw(peek_my);
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
    $stmts->[-1] = call(setr => call(back=>call(continuation => reg '$interpreter')),[$stmts->[-1]]) if $stmts->[-1];
    [@{$stmts},call(goto => reg '$interpreter',[call back=>call(continuation => reg '$interpreter')])];
}

sub varname {
    my $m = shift;
    my @components = name_components($m);
    my $ret = @components[-1];
    $ret;
}
sub name_components {
    my $m = shift;
    if ($m->{sublongname}) {
        my $shortname = $m->{sublongname}{subshortname};
        if ($shortname->{desigilname}) {
            my $longname = $shortname->{desigilname}{longname};
            my $nibbles = $longname->{colonpair}[0]{v}{nibble}{nibbles}[0];
            my @components = ($longname->{name}{identifier}{TEXT},map {$_->{identifier}[0]{TEXT}} @{$longname->{name}{morename}});
            $components[-1] .= ':' . $nibbles if $nibbles;
            $components[-1] = $m->{sigil}{TEXT}.($m->{twigil}[0]{TEXT} || '').$components[-1];

            @components;
        } elsif ($shortname->{category}) {
            use YAML::XS;
            my $single_variant = '';
            if ($shortname->{colonpair}[1]) {
               if ($shortname->{colonpair}[1]{signature}) {
                   # TODO handle whitespace sensibly
                   $single_variant = ':(' . $shortname->{colonpair}[1]{signature}->{MATCH}->Str . ')';
               } else {
                   XXX;
               } 
            }
            my $ret = $m->{sigil}{TEXT}.$shortname->{category}{TEXT}.':'.$shortname->{colonpair}[0]{postcircumfix}{nibble}{nibbles}[0] . $single_variant;
            return $ret;
        }
    } elsif ($m->{desigilname}) {
        $m->{sigil}{TEXT}.($m->{twigil}[0]{TEXT} || '').$m->{desigilname}{longname}->canonical;
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
			} elsif (ref $pc->{sym} eq 'ARRAY' &&
			    $pc->{sym}[0] eq '(' &&
			    $pc->{sym}[1] eq ')')  {
                            my @args = $pc->{semiarglist}->emit_m0ld;
                            my @positional = grep { ref $_ ne 'AST::Pair' } @args;
                            my @named = map { $_->key, $_->value } grep { ref eq 'AST::Pair' } @args;
                            $noun = call 'postcircumfix:( )' => FETCH($noun),[capturize(\@positional,\@named)];
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
    } elsif ($m->{chain}) {
        if (scalar @{$m->{chain}} == 3) {
           fcall '&infix:'.$m->{chain}[1]{infix}{sym},[$m->{chain}[0]->emit_m0ld,$m->{chain}[2]->emit_m0ld];
        } else {
            XXX;
        }
    } else {
        XXX;
    }
} 

1;
