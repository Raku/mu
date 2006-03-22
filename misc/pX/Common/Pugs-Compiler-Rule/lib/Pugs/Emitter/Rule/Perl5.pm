package Pugs::Emitter::Rule::Perl5;

# p6-rule perl5 emitter

# XXX - cleanup unused nodes

use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;

# XXX - reuse this sub in metasyntax()
sub call_subrule {
    my $name = $_[0];
    $name = "\$grammar->" . $_[0] unless $_[0] =~ / :: | \. | -> /x;
    $name =~ s/\./->/;   # XXX - source filter
    return 
        "$_[1] sub{ \n" .
        "$_[1]     $name( \@_ );\n" .
        "$_[1] }\n";
}

sub emit {
    my ($grammar, $ast) = @_;
    return 
        "sub {\n" . 
        "    my \$grammar = shift;\n" .
        "    package Pugs::Runtime::Rule;\n" .
        "    my \$tree;\n" .
        "    rule_wrapper( \$_[0], \n" . 
        emit_rule( $ast, '  ' ) . 
        "        ->( \$_[0], undef, \$tree, \$tree )\n" .
        "    );\n" .
        "}\n";
}

sub emit_rule {
    my $n = $_[0];
    my $tab = $_[1] . '  ';
    die "unknown node: ", Dumper( $n )
        unless ref( $n ) eq 'HASH';
    my ( $k, $v ) = each %$n;
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
    return "$_[1] $sub(\n" .
           emit_rule( $term, $_[1] ) . "$_[1] )\n";
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
    my $value = "sub { die 'not implemented: $name' }\n";
    $value = eval $name if $name =~ /^\$/;
    $value = join('', eval $name) if $name =~ /^\@/;

    # XXX - what hash/code interpolate to?
    # $value = join('', eval $name) if $name =~ /^\%/;

    return "$_[1] constant( '" . $value . "' )\n";
}
sub special_char {
    my $char = substr($_[0],1);
    for ( qw( r n t e f w d ) ) {
        return "$_[1] perl5( '\\$_' )\n" if $char eq $_;
        return "$_[1] perl5( '[^\\$_]' )\n" if $char eq uc($_);
    }
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
    #print "Code: $code\n";
    
    return 
        "$_[1] sub {\n" . 
        "$_[1]     $code;\n" . 
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
sub colon {
    my $num = $_[0];
    return "$_[1] alternation( [ null(), abort() ] ) \n"
        if $num == 1;
    die (':' x $num) . " not implemented";
}
sub constant {
    return "$_[1] constant( q($_[0]) )\n";
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
                "$_[1] sub { $cmd->match( \@_ ) }\n";
        }
        # call method in lexical $var
        return 
            "$_[1] sub { \n" . 
            "$_[1]     my \$r = get_variable( '$cmd' );\n" . 
            "$_[1]     \$r->match( \@_ );\n" .
            "$_[1] }\n";
    }
    if ( $prefix eq q(') ) {   # single quoted literal 
        $cmd = substr( $cmd, 1, -1 );
        return 
            "$_[1] constant( q($cmd) )\n";
    }
    if ( $prefix eq q(") ) {   # interpolated literal 
        $cmd = substr( $cmd, 1, -1 );
        warn "<\"...\"> not implemented";
        return;
    }
    if ( $prefix =~ /[-+[]/ ) {   # character class 
        warn "character classes not implemented";
        return;
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
            "$_[1] )\n";;
    }
    if ( $cmd eq '.' ) {
            warn "<$cmd> not implemented";
            return;
    }
    if ( $prefix =~ /[_[:alnum:]]/ ) {  
        if ( $cmd =~ /^before\s+(.*)/s ) {
            warn "<before ...> not implemented";
            return;
        }
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
        return 
            "$_[1] capture( '$cmd', \n" . 
            call_subrule( $cmd, $_[1]."  " ) . 
            "$_[1] )\n";
    }
    die "<$cmd> not implemented";
}

1;
