package Pugs::Emitter::Rule::Perl5;

use Pugs::Emitter::Rule::Perl5::Ratchet;

# p6-rule perl5 emitter

use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;

our $capture_count;
our $capture_to_array;
our %capture_seen;

sub call_subrule {
    my ( $subrule, $tab, @param ) = @_;
    $subrule = "\$_[4]->" . $subrule unless $subrule =~ / :: | \. | -> /x;
    $subrule =~ s/\./->/;   # XXX - source filter

    return 
"$tab sub{ 
$tab     my \$prior = \$::_V6_PRIOR_;
$tab     my \$param = { \%{ \$_[7] || {} }, args => {" . 
            join(", ",@param) . "} };
$tab     \$_[3] = $subrule( \$_[0], \$param, \$_[3],  );
$tab     \$::_V6_PRIOR_ = \$prior;
$tab }
";
}

sub emit {
    my ($grammar, $ast) = @_;
    # runtime parameters: $grammar, $string, $state, $arg_list
    # rule parameters: see Runtime::Rule.pm
    local $capture_count = -1;
    local $capture_to_array = 0;
    local %capture_seen = ();
    #print "emit capture_to_array $capture_to_array\n";
    # print "emit: ", Dumper($ast);
    
    return 
"do {
  package Pugs::Runtime::Regex;
  my \$matcher = \n" . emit_rule( $ast, '    ' ) . ";
  my \$rule; 
  \$rule = sub {" . 
        # grammar, string, state, args
        #"    print \"match args: \",Dumper(\@_);\n" .
        "
    my \$tree;
    if ( defined \$_[3]{p} ) {
      \$matcher->( \$_[1], \$_[2], \$tree, \$tree, \$_[0], \$_[3]{p}, \$_[1], \$_[3] );
    }
    else {
      for my \$pos ( 0 .. length( \$_[1] ) - 1 ) {
        my \$param = { \%{\$_[3]}, p => \$pos };
        \$matcher->( \$_[1], \$_[2], \$tree, \$tree, \$_[0], \$pos, \$_[1], \$param );
        last if \$tree;
      }
      \$tree = Pugs::Grammar::Base->no_match(\@_)
        unless defined \$tree;
    }
    my \$cap = \$tree->data->{capture};
    if ( ref \$cap eq 'CODE' ) {
      \$::_V6_MATCH_ = \$tree;
      \$tree->data->{capture} = \\(\$cap->( \$tree ));
    };
    if ( \$tree ) { \$::_V6_PRIOR_ = \$rule }
    return \$tree;
  }
}
";
}

sub emit_rule {
    my $n = $_[0];
    my $tab = $_[1] . '  ';
    die "unknown node: ", Dumper( $n )
        unless ref( $n ) eq 'HASH';
    #print "NODE ", Dumper($n);
    my ($k) = keys %$n;
    my $v = $$n{$k};
    # XXX - use real references
    no strict 'refs';
    my $code = &$k( $v, $tab );
    return $code;
}

#rule nodes

sub capturing_group {
    my $program = $_[0];
    
    $capture_count++;
    {
        $capture_seen{$capture_count}++;
        local $capture_count = -1;
        local $capture_to_array = 0;
        local %capture_seen = ();
        $program = emit_rule( $program, $_[1].'      ' )
            if ref( $program );
    }
    
    return 
        "$_[1] positional( $capture_count, " .
        ( $capture_to_array || ( $capture_seen{$capture_count} > 1 ? 1 : 0 ) ) .  
        ", \n" .
        $program . 
        "$_[1] )\n";
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
        
    my $rul;
    {
        my $cap = $capture_to_array;
        local $capture_to_array = $cap || ( $quantifier ne '' ? 1 : 0 );
        $rul = emit_rule( $term, $_[1] . '  ' );
    }

    return $rul 
        if $sub eq '';
    return 
        "$_[1] $sub(\n" .
        $rul . 
        "$_[1] )\n";
}        
sub alt {
    my @s;
    
    my $count = $capture_count;
    my $max = -1;
    for ( @{$_[0]} ) { 
        $capture_count = $count;

        my $_capture_count      = $capture_count;
        my $_capture_to_array   = $capture_to_array;
        my %_capture_seen       = ( %capture_seen );
        local $capture_count    = $_capture_count;
        local $capture_to_array = $_capture_to_array;
        local %capture_seen     = ( %_capture_seen );

        my $tmp = emit_rule( $_, $_[1].'  ' );
        # print ' ',$capture_count;
        $max = $capture_count 
            if $capture_count > $max;
        push @s, $tmp if $tmp;   
    }
    $capture_count = $max;
    
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
        my \$m = \$_[2];
        #print 'var: ',\$m->perl;
        #print 'var: ',\$m->[$num];
        return constant( \"\$m->[$num]\" )->(\@_);
    }";
    $code =~ s/^/$_[1]/mg;
    return "$code\n";
}
sub closure {
    my $code = $_[0]; 
    
    if ( ref( $code ) ) {
        if ( defined $Pugs::Compiler::Perl6::VERSION ) {
            # perl6 compiler is loaded
            my $perl5 = Pugs::Emitter::Perl6::Perl5::emit( 'grammar', $code, 'self' );
            return "
                sub { 
                    \$::_V6_MATCH_ = \$_[0]; 
                    local \$::_V6_SUCCEED = 1;
                    my \$capture = sub { $perl5 }->();
                    \$_[3] = Pugs::Runtime::Match->new( { 
                        bool  => \\\$::_V6_SUCCEED, 
                        str   => \\(\$_[0]),
                        from  => \\(\$_[7]{p} || 0),
                        to    => \\(\$_[7]{p} || 0),
                        match => [],
                        named => {},
                        capture => \\\$capture,
                    } )
                }\n"
                unless $perl5 =~ /return/;
            return "
                sub { 
                    \$_[3] = Pugs::Runtime::Match->new( { 
                        bool  => \\1, 
                        str   => \\(\$_[0]),
                        from  => \\(\$_[7]{p} || 0),
                        to    => \\(\$_[7]{p} || 0),
                        match => [],
                        named => {},
                        capture => sub { $perl5 },
                        abort => 1,
                    } )
                }\n";
        }        
    }

    # XXX XXX XXX - source-filter - temporary hacks to translate p6 to p5
    # $()<name>
    $code =~ s/ ([^']) \$ \( \) < (.*?) > /$1 \$_[0]->[$2]/sgx;
    # $<name>
    $code =~ s/ ([^']) \$ < (.*?) > /$1 \$_[0]->{$2}/sgx;
    # $()
    $code =~ s/ ([^']) \$ \( \) /$1 \$_[0]->()/sgx;
    # $/
    $code =~ s/ ([^']) \$ \/ /$1 \$_[0]/sgx;
    #print "Code: $code\n";
    
    return 
        "$_[1] sub {\n" . 
        "$_[1]     $code( \@_ );\n" . 
        "$_[1]     \$_[3] = Pugs::Runtime::Match->new( { 
            bool  => \\1, 
            str   => \\(\$_[0]),
            from  => \\(\$_[7]{p} || 0),
            to    => \\(\$_[7]{p} || 0),
            match => [],
            named => {},
        } )\n" .
        "$_[1] }\n"
        unless $code =~ /return/;
        
    return
        "$_[1]     sub {\n" . 
        # "print Dumper(\@_);\n" . 
        "$_[1]         \$_[3] = Pugs::Runtime::Match->new( { 
            bool  => \\1, 
            str   => \\(\$_[0]),
            from  => \\(\$_[7]{p} || 0),
            to    => \\(\$_[7]{p} || 0),
            match => [],
            named => {},
            capture => sub $code,
            abort => 1,
        } );\n" .
        "$_[1]     }\n";
}
sub named_capture {
    my $name    = $_[0]{ident};
    my $program = $_[0]{rule};
    $capture_seen{$name}++;
    return 
        "$_[1] named( '$name', " .
        ( $capture_to_array || ( $capture_seen{$name} > 1 ? 1 : 0 ) ) .  
        ", \n" .
        emit_rule($program, $_[1]) . 
        "$_[1] )\n";
}
sub negate {
    my $program = $_[0]{rule};
    return 
        "$_[1] negate( \n" . 
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
    return "$_[1] at_start() \n"
        if $str eq '^';
    return "$_[1] alternation( [ null(), failed_abort() ] ) \n"
        if $str eq ':';
    return "$_[1] at_end_of_string() \n" 
        if $str eq '$';
    return "$_[1] at_line_start() \n"
        if $str eq '^^';
    return "$_[1] at_line_end() \n"
        if $str eq '$$';
    die "'$str' not implemented";
}
sub modifier {
    my $str = $_[0];
    die "modifier '$str' not implemented";
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

    if ( $prefix eq '%' ) {
        # XXX - runtime or compile-time interpolation?
        my $name = substr( $cmd, 1 );
        $capture_seen{$name}++;
        return "$_[1] named( '$name', " .
            ( $capture_to_array || ( $capture_seen{$name} > 1 ? 1 : 0 ) ) .  
            ", hash( \\$cmd ) )\n" 
            if $cmd =~ /::/;
        return "$_[1] named( '$name', " .
            ( $capture_to_array || ( $capture_seen{$name} > 1 ? 1 : 0 ) ) .  
            ", hash( get_variable( '$cmd' ) ) )\n";
    }

    if ( $prefix eq '$' ) {
        if ( $cmd =~ /::/ ) {
            # call method in fully qualified $package::var
            return 
            "$_[1] sub { \n" . 
            # "$_[1]     print 'params: ',Dumper(\@_);\n" . 
            "$_[1]     \$_[3] = $cmd->match( \$_[0], \$_[4], \$_[7], \$_[1] );\n" .
            "$_[1] }\n";
        }
        # call method in lexical $var
        return 
            "$_[1] sub { \n" . 
            #"$_[1]     print 'params: ',Dumper(\@_);\n" . 
            "$_[1]     my \$r = get_variable( '$cmd' );\n" . 
            "$_[1]     \$_[3] = \$r->match( \$_[0], \$_[4], \$_[7], \$_[1] );\n" .
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
        $cmd =~ s/\.\./-/g;
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
    if ( $prefix =~ /[_[:alnum:]]/ ) {
        if ( $cmd eq 'cut' ) {
            warn "<$cmd> not implemented";
            return;
        }
        if ( $cmd eq 'commit' ) {
            warn "<$cmd> not implemented";
            return;
        }        
        # capturing subrule
        # <subrule ( param, param ) >
        my ( $name, $param_list ) = split( /[\(\)]/, $cmd );
        $param_list = '' unless defined $param_list;
        my @param = split( ',', $param_list );
        $capture_seen{$name}++;
        #print "subrule ", $capture_seen{$name}, "\n";
        #print "param: ", Dumper(\@param);
        return             
            "$_[1] named( '$name', " .
            ( $capture_to_array || ( $capture_seen{$name} > 1 ? 1 : 0 ) ) .  
            ", \n" .
            call_subrule( $name, $_[1]."  ", @param ) . 
            "$_[1] )\n";
    }
    die "<$cmd> not implemented";
}

1;
