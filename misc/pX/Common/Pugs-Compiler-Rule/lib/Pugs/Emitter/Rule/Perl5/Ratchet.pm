package Pugs::Emitter::Rule::Perl5::Ratchet;

# p6-rule perl5 emitter for ":ratchet" (non-backtracking)
# see: RuleInline.pl, RuleInline-more.pl for a program prototype

# XXX - cleanup unused nodes

use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;

# XXX - reuse this sub in metasyntax()
sub call_subrule {
    my ( $subrule, $tab, @param ) = @_;
    # TODO - send $pos to subrule
    $subrule = "\$grammar->" . $subrule unless $subrule =~ / :: | \. | -> /x;
    $subrule =~ s/\./->/;   # XXX - source filter
    return 
        "$tab     $subrule( \$s, { p => \$pos, args => {" . join(", ",@param) . "} }, \$_[1] )";
}

sub call_constant {
    my $const = $_[0];
    my $len = length( $const );
    $const = $_[0] eq '\\' ? '\\\\' : $_[0];  # XXX - generalize
    return
    "$_[1] ( ( substr( \$s, \$pos, $len ) eq '$const' ) 
$_[1]     ? do { \$pos += $len }
$_[1]     : 0
$_[1] )";
}

sub call_perl5 {
    my $const = $_[0];
    return
    "$_[1] ( ( substr( \$s, \$pos ) =~ m/^$const/s )  
$_[1]     ? do { \$pos += 1 }    # XXX - get pos from regex
$_[1]     : 0
$_[1] )";
}

sub emit {
    my ($grammar, $ast) = @_;
    # runtime parameters: $grammar, $string, $state, $arg_list
    # rule parameters: see Runtime::Rule.pm
    return 
        "sub {\n" . 
        "    my \$grammar = shift;\n" .
        "    my \$s = shift;\n" .
        "    my \@match;\n" .
        #"    print \"match arg_list = \$_[1]\n\";\n" .
        #"    print \"match arg_list = \@{[\%{\$_[1]} ]}\n\" if defined \$_[1];\n" .
        "    my \$pos = \$_[1]{p};\n" .
        "    \$pos = 0 unless defined \$pos;   # TODO - .*? \$match \n" .
        #"    print \"match pos = \$pos\n\";\n" .
        "    my \$from = \$pos;\n" .
        "    my \$bool = 1;\n" .
        "    my \$capture;\n" .
        "    my \$m = bless \\{ \n" .
        "      str => \$s, from => \\\$from, to => \\\$pos, \n" .
        "      bool => \\\$bool, match => \\\@match, \n" .
        "      capture => \\\$capture, \n" .
        "    }, 'Pugs::Runtime::Match::Ratchet';\n" .
        "    \$bool = 0 unless\n" .
        emit_rule( $ast, '    ' ) . 
        "    ;\n" .
        "    return \$m;\n" .
        "}\n";
}

sub emit_rule {
    my $n = $_[0];
    my $tab = $_[1] . '  ';
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

sub capturing_group {
    return named_capture(
            { ident => '', rule => $_[0] }, 
            $_[1],    
    );
}        
sub non_capturing_group {
    return emit_rule( $_[0], $_[1] );
}        
sub quant {
    my $term = $_[0]->{'term'};
    my $quantifier = $_[0]->{quant};
    $quantifier = '' unless defined $quantifier;
    my $rul = emit_rule( $term, $_[1] . "  " );
    return $rul
        if $quantifier eq '';
    # *  +  ?
    # TODO: *? +? ??
    # TODO: quantifier + capture creates Array
    return 
        "$_[1] (\n$rul\n" .
        "$_[1] ||\n" .
        "$_[1]   1\n" .
        "$_[1] )"
        if $quantifier eq '?';
    return 
        "$_[1] do { while \n$rul {}; 1 }"
        if $quantifier eq '*';
    return
        "$_[1] (\n$rul\n" .
        "$_[1] &&\n" .
        "$_[1]   do { while \n$rul {}; 1 }\n" .
        "$_[1] )"
        if $quantifier eq '+';
    die "quantifier not implemented: $quantifier";
}        
sub alt {
    my @s;
    for ( @{$_[0]} ) { 
        my $tmp = emit_rule( $_, $_[1].'  ' );
        push @s, $tmp if $tmp;   
    }
    return 
        "$_[1] do {
$_[1]   my \$pos1 = \$pos;
$_[1]   do {
" . join( "\n$_[1]   } || do { \$pos = \$pos1;\n", @s ) . "
$_[1]   }
$_[1] }";
}        
sub concat {
    my @s;
    for ( @{$_[0]} ) { 
        my $tmp = emit_rule( $_, $_[1] );
        push @s, $tmp if $tmp;   
    }
    return "$_[1] (\n" . join( "\n$_[1] &&\n", @s ) . "\n$_[1] )";
}        
sub code {
    return "$_[1] $_[0]\n";  
}        
sub dot {
    "$_[1] ++\$pos   # . - TODO - check str boundaries\n"
}
sub variable {
    my $name = "$_[0]";
    my $value = undef;
    # XXX - eval $name doesn't look up in user lexical pad
    # XXX - what &xxx interpolate to?
    
    if ( $name =~ /^\$/ ) {
        # $^a, $^b
        if ( $name =~ /^ \$ \^ ([^\s]*) /x ) {
            my $index = ord($1)-ord('a');
            #print "Variable #$index\n";
            #return "$_[1] constant( \$_[7][$index] )\n";
            
            my $code = 
            "    ... sub { 
                #print \"Runtime Variable args[\", join(\",\",\@_) ,\"] \$_[7][$index]\\n\";
                return constant( \$_[7][$index] )->(\@_);
            }";
            $code =~ s/^/$_[1]/mg;
            return "$code\n";
        }
        else {
            $value = eval $name;
        }
    }
    
    $value = join('', eval $name) if $name =~ /^\@/;
    if ( $name =~ /^%/ ) {
        # XXX - runtime or compile-time interpolation?
        return "$_[1] ... hash( \\$name )\n" if $name =~ /::/;
        return "$_[1] ... hash( get_variable( '$name' ) )\n";
    }
    die "interpolation of $name not implemented"
        unless defined $value;

    return call_constant( $value, $_[1] );
}
sub special_char {
    my $char = substr($_[0],1);
    for ( qw( r n t e f w d s ) ) {
        return call_perl5(   "\\$_",  $_[1] ) if $char eq $_;
        return call_perl5( "[^\\$_]", $_[1] ) if $char eq uc($_);
    }
    $char = '\\\\' if $char eq '\\';
    return call_constant( $char, $_[1] );
}
sub match_variable {
    my $name = $_[0];
    my $num = substr($name,1);
    #print "var name: ", $num, "\n";
    my $code = 
    "    ... sub { 
        my \$m = Pugs::Runtime::Match->new( \$_[2] );
        return constant( \"\$m->[$num]\" )->(\@_);
    }";
    $code =~ s/^/$_[1]/mg;
    return "$code\n";
}
sub closure {
    my $code = $_[0]; 
    
    # XXX XXX XXX - source-filter - temporary hacks to translate p6 to p5
    # $()<name>
    $code =~ s/ ([^']) \$ \( \) < (.*?) > /$1 \$_[0]->[$2] /sgx;
    # $<name>
    $code =~ s/ ([^']) \$ < (.*?) > /$1 \$_[0]->{$2} /sgx;
    # $()
    $code =~ s/ ([^']) \$ \( \) /$1 \$_[0]->() /sgx;
    # $/
    $code =~ s/ ([^']) \$ \/ /$1 \$_[0] /sgx;
    #print "Code: $code\n";
    
    return 
        "$_[1] ( sub $code->( \$m ) || 1 )" 
        unless $code =~ /return/;
        
    return
        "$_[1] ( ( \$capture = sub $code->( \$m ) ) 
$_[1]   && return \$m )";
}
sub named_capture {
    my $name    = $_[0]{ident};
    my $program = $_[0]{rule};

    $program = emit_rule( $program, $_[1].'      ' )
        if ref( $program );

    return "$_[1] do{ 
$_[1]     my \$bool = 1;
$_[1]     my \@tmp = ( 
$_[1]       name => '$name', 
$_[1]       from => \$pos, 
$_[1]     );
$_[1]     push \@tmp, (
$_[1]       match => do{
$_[1]         my \@match;
$_[1]         \$bool = 0 unless
" .             $program . "
$_[1]         ;
$_[1]         \\\@match 
$_[1]       },
$_[1]     );
$_[1]     push \@tmp, ( 
$_[1]       bool => \$bool, 
$_[1]       to => \$pos, 
$_[1]     ); 
$_[1]     push \@match, { \@tmp };
$_[1]     \$bool;
$_[1] }";
}
sub before {
    my $program = $_[0]{rule};
    return 
        "$_[1] ... before( \n" . 
        emit_rule($program, $_[1]) . 
        "$_[1] )\n";
}
sub colon {
    my $str = $_[0];
    return "$_[1] # : no-op\n"
        if $str eq ':';
    return "$_[1] ( \$pos >= length( \$s ) ) \n" 
        if $str eq '$';
    die "'$str' not implemented";
}
sub constant {
    call_constant( @_ );
}

use vars qw( %char_class );
BEGIN {
    %char_class = map { $_ => 1 } qw( 
alpha
alnum
ascii
blank
cntrl
digit
graph
lower
print
punct
space
upper
word
xdigit
);
}

sub metasyntax {
    # <cmd>
    my $cmd = $_[0];   
    my $prefix = substr( $cmd, 0, 1 );
    if ( $prefix eq '@' ) {
        # XXX - wrap @array items - see end of Pugs::Grammar::Rule
        return 
            "$_[1] ... alternation( \\$cmd )\n";
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
                "$_[1]         }"
    }
    if ( $prefix eq q(') ) {   # single quoted literal ' 
        $cmd = substr( $cmd, 1, -1 );
        return call_constant( $cmd, $_[1] );
    }
    if ( $prefix eq q(") ) {   # interpolated literal "
        $cmd = substr( $cmd, 1, -1 );
        warn "<\"...\"> not implemented";
        return;
    }
    if ( $prefix =~ /[-+[]/ ) {   # character class 
	   if ( $prefix eq '-' ) {
	       $cmd = '[^' . substr($cmd, 2);
	   } 
       elsif ( $prefix eq '+' ) {
	       $cmd = substr($cmd, 2);
	   }
	   # XXX <[^a]> means [\^a] instead of [^a] in perl5re

	   return "$_[1] ... perl5( q!$cmd! )\n" unless $cmd =~ /!/;
	   return "$_[1] ... perl5( q($cmd) )\n"; # XXX if $cmd eq '!)'
    }
    if ( $prefix eq '?' ) {   # non_capturing_subrule / code assertion
        $cmd = substr( $cmd, 1 );
        if ( $cmd =~ /^{/ ) {
            warn "code assertion not implemented";
            return;
        }
        return call_subrule( $cmd, $_[1] );
    }
    if ( $prefix eq '!' ) {   # negated_subrule / code assertion 
        $cmd = substr( $cmd, 1 );
        if ( $cmd =~ /^{/ ) {
            warn "code assertion not implemented";
            return;
        }
        return 
            "$_[1] ... negate( '$_[0]', \n" .
            call_subrule( $_[0], $_[1]."  " ) .
            "$_[1] )\n";
    }
    if ( $cmd eq '.' ) {
            warn "<$cmd> not implemented";
            return;
    }
    if ( $prefix =~ /[_[:alnum:]]/ ) {  
        # "before" is handled in a separate rule, because it requires compilation
        # if ( $cmd =~ /^before\s+(.*)/s ) {
        if ( $cmd =~ /^after\s+(.*)/s ) {
            warn "<after ...> not implemented";
            return;
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
        if ( exists $char_class{$cmd} ) {
            # XXX - inlined char classes are not inheritable, but this should be ok
            return
                "$_[1] ( ( substr( \$s, \$pos, 1 ) =~ /[[:$cmd:]]/ ) 
$_[1]     ? ++\$pos
$_[1]     : 0
$_[1] )";
        }
        # capturing subrule
        # <subrule ( param, param ) >
        my ( $subrule, $param_list ) = split( /[\(\)]/, $cmd );
        $param_list = '' unless defined $param_list;
        my @param = split( ',', $param_list );
        # TODO - send $pos to subrule
        return named_capture(
            { ident => $subrule, 
              rule => 
                "$_[1]         push \@match,\n" . 
                    call_subrule( $subrule, $_[1]."      ", @param ) . ";\n" .
                "$_[1]         \$pos = \$match[-1]->to"
            }, 
            $_[1],    
        );
    }
    die "<$cmd> not implemented";
}

1;
