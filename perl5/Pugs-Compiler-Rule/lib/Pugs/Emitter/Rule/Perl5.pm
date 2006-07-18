package Pugs::Emitter::Rule::Perl5;

# p6-rule perl5 emitter

# XXX - cleanup unused nodes

use strict;
use warnings;
use Data::Dump::Streamer;
$Data::Dump::Streamer::Indent = 1;

# XXX - reuse this sub in metasyntax()
sub call_subrule {
    my ( $subrule, $tab, @param ) = @_;
    $subrule = "\$_[4]->" . $subrule unless $subrule =~ / :: | \. | -> /x;
    $subrule =~ s/\./->/;   # XXX - source filter
    return 
        "$tab sub{ \n" .
        "$tab     # param: @param \n" .
        "$tab     $subrule( \$_[0], { p => 0, args => {" . join(", ",@param) . "} }, \$_[1] );\n" .
        "$tab }\n";
}

sub emit {
    my ($grammar, $ast) = @_;
    # runtime parameters: $grammar, $string, $state, $arg_list
    # rule parameters: see Runtime::Rule.pm
    return 
        "do {\n" .
        "    package Pugs::Runtime::Rule;\n" .
        "    my \$matcher = \n" . 
        emit_rule( $ast, '    ' ) . "  ;\n" .
        "  sub {\n" . 
        "    my \$grammar = shift;\n" .
        "    my \$tree;\n" .
        "    my \$pos = \$_[2]{p};\n" .
        "    \$pos = 0 unless defined \$pos;   # TODO - .*? \$match \n" .
        "    my \$s = \$_[0]; # TODO - substr( \$_[0], \$pos );\n" .
        # "    print \"\\nVariables: args[\", join(\",\",\@_) ,\"] \\n\";\n" .
        # "    print \"         : \@{\$_[2]}} \\n\" if defined \$_[2];\n" .
        # "    \$_[0] = '' unless defined \$_[0];\n" .
        "    my \$match = rule_wrapper( \$_[0], \n" . 
        "        \$matcher->( \$s, \$_[1], \$tree, \$tree, \$grammar, 0, \$s, \$_[2] )\n" .
        "    );\n" .
        "    return Pugs::Runtime::Match->new( \$match );\n" .
        "  }\n" .
        "}\n";
}

sub emit_rule {
    my $n = $_[0];
    my $tab = $_[1] . '  ';
    die "unknown node: ", Dump( $n )
        unless ref( $n ) eq 'HASH';
    #print "NODE ", Dump($n);
    my ($k) = keys %$n;
    my $v = $$n{$k};
    #my ( $k, $v ) = each %$n;
    # XXX - use real references
    no strict 'refs';
    my $code = &$k( $v, $tab );
    return $code;
}

#rule nodes

sub capturing_group {
    return 
        "$_[1] capture( '',\n" .
        emit_rule( $_[0], $_[1].'  ' ) . 
        "$_[1] )\n" .
        '';
}        
sub non_capturing_group {
    return emit_rule( $_[0], $_[1] );
}        
sub quant {
    my $term = $_[0]->{'term'};
    my $quantifier = $_[0]->{quant};
    $quantifier = '' unless defined $quantifier;
    my $sub = { 
            '*' =>'greedy_star',     
            '+' =>'greedy_plus',
            '*?'=>'non_greedy_star', 
            '+?'=>'non_greedy_plus',
            '?' =>'optional',
            '??'=>'null_or_optional',
            ''  => '',
        }->{$quantifier};
    die "quantifier not implemented: $quantifier" 
        unless defined $sub;
    return emit_rule( $term, $_[1] ) 
        if $sub eq '';
    return 
        "$_[1] capture( '*quantifier*',\n" . 
        "$_[1]     $sub(\n" .
        emit_rule( $term, $_[1] . ("    "x2) ) . 
        "$_[1]     )\n" .
        "$_[1] )\n" .
        '';
}        
sub alt {
    my @s;
    for ( @{$_[0]} ) { 
        my $tmp = emit_rule( $_, $_[1] );
        push @s, $tmp if $tmp;   
    }
    return "$_[1] alternation( [\n" . 
           join( ',', @s ) .
           "$_[1] ] )\n";
}        
sub concat {
    my @s;
    for ( @{$_[0]} ) { 
        my $tmp = emit_rule( $_, $_[1] );
        push @s, $tmp if $tmp;   
    }
    return 
        "$_[1] concat( \n" . 
        join( ',', @s ) .
        "$_[1] )\n";
}        
sub code {
    return "$_[1] $_[0]\n";  
}        
sub dot {
    return call_subrule( 'any', $_[1] );
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
            "    sub { 
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
        return "$_[1] hash( \\$name )\n" if $name =~ /::/;
        return "$_[1] hash( get_variable( '$name' ) )\n";
    }
    die "interpolation of $name not implemented"
        unless defined $value;

    return "$_[1] constant( '" . $value . "' )\n";
}
sub special_char {
    my $char = substr($_[0],1);
    for ( qw( r n t e f w d s ) ) {
        return "$_[1] perl5( '\\$_' )\n" if $char eq $_;
        return "$_[1] perl5( '[^\\$_]' )\n" if $char eq uc($_);
    }
    $char = '\\\\' if $char eq '\\';
    return "$_[1] constant( q!$char! )\n" unless $char eq '!';
    return "$_[1] constant( q($char) )\n";
}
sub match_variable {
    my $name = $_[0];
    my $num = substr($name,1);
    #print "var name: ", $num, "\n";
    my $code = 
    "    sub { 
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
        "$_[1] sub {\n" . 
        "$_[1]     $code( \@_ );\n" . 
        "$_[1]     return { bool => 1, tail => \$_[0] }\n" .
        "$_[1] }\n"
        unless $code =~ /return/;
        
    return
        "$_[1] abort(\n" .
        "$_[1]     sub {\n" . 
        "$_[1]         return { bool => 1, tail => \$_[0], return => sub $code };\n" .
        "$_[1]     }\n" .
        "$_[1] )\n";
}
sub named_capture {
    my $name    = $_[0]{ident};
    my $program = $_[0]{rule};
    return 
        "$_[1] capture( '$name', \n" . 
        emit_rule($program, $_[1]) . 
        "$_[1] )\n";
}
sub before {
    my $program = $_[0]{rule};
    return 
        "$_[1] before( \n" . 
        emit_rule($program, $_[1]) . 
        "$_[1] )\n";
}
sub colon {
    my $str = $_[0];
    return "$_[1] alternation( [ null(), fail() ] ) \n"
        if $str eq ':';
    return "$_[1] end_of_string() \n" 
        if $str eq '$';
    die "'$str' not implemented";
}
sub constant {
    my $char = $_[0] eq '\\' ? '\\\\' : $_[0];
    return "$_[1] constant( q!$char! )\n" unless $char =~ /!/;
    return "$_[1] constant( q($char) )\n";
}
sub metasyntax {
    # <cmd>
    my $cmd = $_[0];   
    my $prefix = substr( $cmd, 0, 1 );
    if ( $prefix eq '@' ) {
        # XXX - wrap @array items - see end of Pugs::Grammar::Rule
        return 
            "$_[1] alternation( \\$cmd )\n";
    }
    if ( $prefix eq '$' ) {
        if ( $cmd =~ /::/ ) {
            # call method in fully qualified $package::var
            return 
                "$_[1] sub { $cmd->match( \@_[0, 4], {p => 0}, \$_[1] ) }\n";
        }
        # call method in lexical $var
        return 
            "$_[1] sub { \n" . 
            "$_[1]     my \$r = get_variable( '$cmd' );\n" . 
            "$_[1]     \$r->match( \@_[0, 4], {p => 0}, \$_[1] );\n" .
            "$_[1] }\n";
    }
    if ( $prefix eq q(') ) {   # single quoted literal ' 
        $cmd = substr( $cmd, 1, -1 );
        return "$_[1] constant( q!$cmd! )\n" unless $cmd =~ /!/;
        return "$_[1] constant( q($cmd) )\n";
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

	   return "$_[1] perl5( q!$cmd! )\n" unless $cmd =~ /!/;
	   return "$_[1] perl5( q($cmd) )\n"; # XXX if $cmd eq '!)'
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
            "$_[1] negate( '$_[0]', \n" .
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
        # capturing subrule
        # <subrule ( param, param ) >
        my ( $subrule, $param_list ) = split( /[\(\)]/, $cmd );
        $param_list = '' unless defined $param_list;
        my @param = split( ',', $param_list );
        return             
            "$_[1] capture( '$subrule', \n" . 
            call_subrule( $subrule, $_[1]."  ", @param ) . 
            "$_[1] )\n";
    }
    die "<$cmd> not implemented";
}

1;
