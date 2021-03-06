package Pugs::Emitter::Rule::Parsec;

# p6-rule parsec emitter

use strict;
use warnings;
use Pugs::Grammar::MiniPerl6;
use Data::Dumper;
$Data::Dumper::Indent = 1;

our $sigspace = 0;
our $capture_counter = 0;
sub get_capture_var { return 'capture_' . $capture_counter++; }

sub rule_rename($){
    my $orig_name = shift;
    return 'rule' . (uc substr $orig_name, 0, 1) . substr $orig_name, 1;
}

sub to_genparser_string($) {
    my $char_parser = shift;
    return "($char_parser >>= \\c -> return [c])";
}

sub call_constant {
    my $str = shift;
    $str =~ s/\\/\\\\/g;
    $str =~ s/"/\\"/g;
    return 'string "' . $str . '"';
}

sub emit {
    my ($grammar, $ast, $param) = @_;
    local $sigspace = $param->{sigspace};   # XXX - $sigspace should be lexical
    local $capture_counter = 0;
    emit_rule( $ast, '' ) . "\n";
}

sub emit_rule {
    my $n = $_[0];
    my $tab = $_[1];
    die "unknown node: ", Dumper( $n )
        unless ref( $n ) eq 'HASH';
    #print "NODE ", Dumper($n);
    my ( $k, $v ) = each %$n;
    # XXX - use real references
    no strict 'refs';
    my $code = &$k( $v, $tab );
    return $code;
}

#rule nodes

sub non_capturing_group {
    return emit_rule( $_[0], $_[1] );
}

sub quant {
    my $term = $_[0]->{'term'};
    my $quantifier = $_[0]->{quant};
    my $spacing =
	( $sigspace &&
	  ($_[0]->{ws1} ne '' && $_[0]->{ws2} ne '')
	);
    my $tab = $spacing ? $_[1] . '  ' : $_[1];
    my $rul = emit_rule( $term, $tab );

    my $ws = metasyntax('?ws', $tab);
    $rul = "$ws\n$tab$rul" if $sigspace && $_[0]->{ws1} ne '';
    $rul = "$rul\n$tab$ws" if $sigspace && $_[0]->{ws2} ne '';
    $rul = "do\n$tab$rul" if $spacing;
    return $rul 
        if $quantifier eq '';

    # *  +  ?
    my $qual = '';
    return "option \"\" \$ $rul" if $quantifier eq '?';

    $qual = 'many'     if $quantifier eq '*';
    $qual = 'many1'    if $quantifier eq '+';

    die "quantifier not implemented: $quantifier" if $qual eq '';

    my $final_rul =
	"(($qual \$ $rul) >>= \\arr -> return \$ foldr (++) \"\" arr)";

    return "$final_rul >>\n$tab$ws" if $sigspace and $_[0]->{ws3} ne '';
    return $final_rul;
}

sub alt {
    my @s;
    my @alt = @{$_[0]};

    # clean up alternative body
    # XXX maybe can be done earlier (e.g. in parser)
    foreach(@alt){
	if(ref eq 'HASH'){
	    my($k) = keys %$_;

	    if($k eq 'alt'){
		my @sub_alt = @{$_->{alt}};
		$_ = $sub_alt[0];
		push @alt, @sub_alt[1 .. $#sub_alt];
	    }
	}
    }

    my $indent = $_[1] . '  ';
    foreach(@alt){ 
        my $tmp = emit_rule( $_, $indent );
        push @s, $tmp if $tmp;   
    }
    return "do\n$indent" . join "\n$indent<|>\n$indent", @s;
}

sub concat {
    my @inner = @{$_[0]};

    # clean up concatenation body
    # XXX maybe can be done earlier (e.g. in parser)
    foreach(@inner){
	if(ref eq 'HASH'){
	    my($k) = keys %$_;

	    if($k eq 'concat'){
		my @sub_inner = @{$_->{concat}};
		$_ = $sub_inner[0];
		push @inner, @sub_inner[1 .. $#sub_inner];
	    }
	}
    }

    my $indent = $_[1] . '  ';
    my $result = 'do';
    foreach(@inner){ 
        my $tmp = emit_rule( $_, $indent );
	$result .= "\n$indent" . $tmp if $tmp;
    }
    return $result;
}

sub dot {
    return to_genparser_string("anyChar");
}

sub variable {}

use vars qw( %special_chars );
BEGIN {
    %special_chars = ( 
r => "char '\\r'",
n => "char '\\n'",
t => "char '\\t'",
e => "char '\\033'",
f => "char '\\f'",
w => "(alphaNum <|> char '_')",
d => 'digit',
s => 'space',
W => "satisfy (\\x -> x /= '_' && not \$ isAlphaNum x)",
D => 'noneOf "0123456789"',
S => 'noneOf " \\v\\f\\t\\r\\n"',
);

    while(my ($k, $v) = each %special_chars){
	next if
	    $k eq uc $k or
	    exists $special_chars{uc $k} or
	    $v !~ /^char/;
	my $chars = substr $v, 6;
	chop $chars;
	$special_chars{uc $k} = "noneOf \"$chars\"";
    }
}
sub special_char {
    my $char = substr($_[0],1);
    return to_genparser_string($special_chars{$char})
	if exists $special_chars{$char};
    $char = '\\\\' if $char eq '\\';
    return "string \"$char\"";
}

sub match_variable {}

sub closure {
    my $miniperl6 = substr $_[0], 1, length($_[0]) - 2;
    my $haskell   = Pugs::Grammar::MiniPerl6->ProductionRule($miniperl6);
    $haskell =~ s/\n/\n$_[1]/sg;
    # print ">>> MiniPerl6\n$miniperl6\n===\n$haskell\n<<< Haskell\n";
    return "$haskell";
}

sub capturing_group {
    my $program = $_[0];

    $program = emit_rule( $program, $_[1] . '  ' )
        if ref( $program );

    return &get_capture_var . ' <- ' . $program;
}

sub named_capture {
    my $name    = $_[0]{ident};
    my $program = $_[0]{rule};

    return "$name <- " . metasyntax($program->{metasyntax}, $_[1] . '  ', 1)
	if exists $program->{metasyntax};

    return "$name <- " . emit_rule($program, $_[1] . '  ');
}

sub negate {
    my $body = $_[0];
    return not_after($body->{after}, $_[1]) if exists $body->{after};
    return not_before($body->{before}, $_[1]) if exists $body->{before};
    return '';
}

sub before {
    my $program = $_[0]{rule};
    return 'lookAhead (' . emit_rule($program, $_[1] . '    ') . ')';
}

sub not_before {
    my $program = $_[0]{rule};
    return 'notFollowedBy $ (' .
	emit_rule($program, $_[1] . '    ') .
	") >> return ' '";
    # notFollowedBy :: Show tok => GenParser tok st tok -> GenParser tok st ()
    #   tok = Char in the context so that we have to cast it
}

sub after {}
sub not_after {}
sub colon {}

sub constant {
    return "string \"$_[0]\"";
}

use vars qw( %char_class );
BEGIN {
    %char_class = ( 
alpha => 'letter',
alnum => 'alphaNum',
ascii => 'satisfy isAscii',
blank => 'oneOf " \\t"',
cntrl => 'satisfy isCotrol',
digit => 'digit',
graph => "satisfy (\\x -> isPrint x && x /= ' ')",
lower => 'lower',
print => 'satisfy isPrint',
punct => "satisfy (\\x -> isPrint x && x /= ' ' && not (isAlphaNum x))",
space => 'space',
upper => 'upper',
word  => "(alphaNum <|> char '_')",
xdigit => 'hexDigit',
);
}

sub metasyntax {
    # <cmd>
    my $cmd = $_[0];   
    my $prefix = substr( $cmd, 0, 1 );

    my $named_capturing = !$_[2];
    my $negative_lookahead = 0;

    if ( $prefix eq '@' ) {
        # XXX - wrap @array items - see end of Pugs::Grammar::Rule
        # TODO - param list
        return 
            "$_[1] do {\n" . 
            "$_[1]    my \$match;\n" . 
            "$_[1]    for my \$subrule ( $cmd ) {\n" . 
            "$_[1]        \$match = " . 
                call_subrule( '$subrule', '', () ) . ";\n" .
            "$_[1]        last if \$match;\n" . 
            "$_[1]    }\n" .
            "$_[1]    my \$bool = (!\$match != 1);\n" . 
            "$_[1]    \$pos = \$match->to if \$bool;\n" . 
            "$_[1]    \$bool;\n" . 
            "$_[1] }";
    }
    if ( $prefix eq '$' ) {
        if ( $cmd =~ /::/ ) {
            # call method in fully qualified $package::var
            # ...->match( $rule, $str, $grammar, $flags, $state )  
            # TODO - send $pos to subrule
            return 
                "$_[1]         do {\n" .
                "$_[1]           push \@match,\n" . 
                "$_[1]             $cmd->match( \$s, \$grammar, {p => \$pos}, undef );\n" .
                "$_[1]           \$pos = \$match[-1]->to;\n" .
                "$_[1]           !\$match[-1] != 1;\n" .
                "$_[1]         }"
        }
        # call method in lexical $var
        # TODO - send $pos to subrule
        return 
                "$_[1]         do {\n" .
                "$_[1]           my \$r = Pugs::Runtime::Rule::get_variable( '$cmd' );\n" . 
                "$_[1]           push \@match,\n" . 
                "$_[1]             \$r->match( \$s, \$grammar, {p => \$pos}, undef );\n" .
                "$_[1]           \$pos = \$match[-1]->to;\n" .
                "$_[1]           !\$match[-1] != 1;\n" .
                "$_[1]         }"
    }
    if ( $prefix eq q(') ) {   # single quoted literal ' 
        $cmd = substr( $cmd, 1, -1 );
        return call_constant( $cmd );
    }
    if ( $prefix eq q(") ) {   # interpolated literal "
        $cmd = substr( $cmd, 1, -1 );
        warn "<\"...\"> not implemented";
        return;
    }
    if ( $prefix =~ /[-+[]/ ) {   # character class 
	   if ( $prefix eq '-' ) {
	       my $str = substr $cmd, 2, length($cmd) - 3;
	       $str =~ s/\\>/>/g; # XXX
	       $str =~ s/\\/\\\\/g;
	       $str =~ s/"/\\"/g;

	       return to_genparser_string("noneOf \"$str\"");
	   } 
	   elsif ( $prefix eq '+' ) {
	       $cmd = substr($cmd, 2);
	   }

	   my $str = substr $cmd, 1, length($cmd) - 2;
	   $str =~ s/\\>/>/g; # XXX
	   $str =~ s/\\/\\\\/g;
	   $str =~ s/"/\\"/g;

	   return to_genparser_string("oneOf \"$str\"");
    }
    if ( $prefix eq '?' ) {   # non_capturing_subrule / code assertion
        $cmd = substr( $cmd, 1 );
        if ( $cmd =~ /^{/ ) {
            warn "code assertion not implemented";
            return;
        }
	$prefix = substr( $cmd, 0, 1 );
	$named_capturing = 0;
    }
    if ( $prefix eq '!' ) {   # negated_subrule / code assertion 
        $cmd = substr( $cmd, 1 );
        if ( $cmd =~ /^{/ ) {
            warn "code assertion not implemented";
            return;
        }
	$prefix = substr( $cmd, 0, 1 );
	$negative_lookahead = 1;
	warn "<$cmd> not implemented";
	return;
    }
    if ( $cmd eq '.' ) {
            warn "<$cmd> not implemented";
            return;
    }
    if ( $prefix =~ /[_[:alnum:]]/ ) {  
        # "before" and "after" are handled in a separate rule
	if ( $cmd eq 'ws' ){
	    return 'perl6WhiteSpace';
	    # assuming function:
	    # perl6WhiteSpace = do cls <- getPrevCharClass
	    #                      let mod = if cls == WordClass then many1 else many
	    #                      do mod whiteSpace
	    #                         <|>
	    #                         (satisfy (\c -> charClassOf c /= WordClass) >> return "")

	}
        if ( $cmd eq 'cut' ) {
            warn "<$cmd> not implemented";
            return;
        }
        if ( $cmd eq 'commit' ) {
            warn "<$cmd> not implemented";
            return;
        }
        if ( $cmd eq 'prior' ) {
            warn "<$cmd> not implemented";
            return;
        }
        if ( $cmd eq 'null' ) {
            warn "<$cmd> not implemented";
            return;
        }
        if ( $char_class{$cmd} ) {
            # XXX - inlined char classes are not inheritable, but this should be ok
	    return to_genparser_string($char_class{$cmd});
        }
        # capturing subrule
        # <subrule ( param, param ) >
        my ( $subrule, $param_list ) = split( /[\(\)]/, $cmd );
        $param_list = '' unless defined $param_list;
        my @param = split( ',', $param_list );
	return ($named_capturing ? "$subrule <- " : '') .
	    rule_rename($subrule) . join '', map { " ($_)" } @param;
    }
    die "<$cmd> not implemented";
}

1;
