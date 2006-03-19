package Pugs::Emitter::Rule::Perl5;

# p6-rule perl5 emitter

# XXX - cleanup unused nodes

use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;

sub call_subrule {
    my $name = $_[0];
    $name = "\$grammar->" . $_[0] unless $_[0] =~ /::/;
    return 
        "$_[1] sub{ \n" .
        # $_[1] . '    warn "Trying $method\n";' . "\n" .
        # "    my \$match = \n" .
        "$_[1]     $name( \@_ );\n" .
        # $_[1] . '    warn $match ? "Match\n" : "No match\n";' . "\n" .
        "$_[1] }\n";
}

sub emit {
    my ($grammar, $ast) = @_;
    #print "emit ast: ",do{use Data::Dumper; Dumper($ast)};
    my $source = 
        "sub {\n" . 
        "    my \$grammar = shift;\n" .
        #"    print 'GRAMMAR: \$grammar', \"\\n\";\n" .
        "    package Pugs::Runtime::Rule;\n" .
        "    rule_wrapper( \$_[0], \n" . 
        emit_rule( $ast ) . 
        "        ->( \@_ )\n" .
        "    );\n" .
        "}\n" .
        "";
    #print $source;
    return $source;
}

sub emit_rule {
    my $n = $_[0];
    my $tab = $_[1] || '    '; 
    $tab .= '  ';
    #print "emit_rule(): ", ref($n)," ",Dumper( $n ), "\n";
    if ( ref( $n ) eq 'HASH' ) 
    {
        my ( $k, $v ) = each %$n;
        # print "$tab $k => $v \n";
        # XXX - use real references
        no strict 'refs';
        my $code = &$k( $v, $tab );
        return $code;
    }
    local $Data::Dumper::Deepcopy = 1;
    die "unknown node: ", Dumper( $n );
}

#rule nodes

sub capturing_group {
    return 
       "$_[1] capture( '',\n" .
       emit_rule( $_[0], $_[1].'    ' ) . 
       "$_[1] )\n" .
       '';
}        
sub non_capturing_group {
    return emit_rule( $_[0], $_[1] );
}        
sub quant {
    # print "*** \$_[0]:\n",Dumper $_[0];
    my $term = $_[0]->{'term'};
    my $quantifier = $_[0]->{quant};
    my $sub = { 
            '*' =>'greedy_star',     
            '+' =>'greedy_plus',
            '*?'=>'non_greedy_star', 
            '+?'=>'non_greedy_plus',
            '?' =>'optional',
            '??' =>'null_or_optional',
            ''  => '',
        }->{$quantifier};
    # print "*** \$quantifier:\n",Dumper $quantifier;
    die "quantifier not implemented: $quantifier" 
        unless defined $sub;
    return emit_rule( $term, $_[1] ) 
        if $sub eq '';
    return "$_[1] $sub(\n" .
           emit_rule( $term, $_[1] ) . "$_[1] )\n";
}        
sub alt {
    # local $Data::Dumper::Indent = 1;
    # print "*** \$_[0]:\n",Dumper $_[0];
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
    # local $Data::Dumper::Indent = 1;
    # print "*** \$_[0]:\n",Dumper $_[0];
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
#sub term {
#    return emit_rule( $_[0], $_[1] );
#}        
sub code {
    # return "$_[1] # XXX code - compile '$_[0]' ?\n";
    return "$_[1] $_[0]  # XXX - code\n";  
}        
sub dot {
    return call_subrule( 'any', $_[1] );
}
sub subrule {
    return 
        "$_[1] capture( '$_[0]', \n" .
        call_subrule( $_[0], $_[1]."    " ) .
        "$_[1] )\n";;
}
sub non_capturing_subrule {
    call_subrule( $_[0], $_[1] );
}
sub negated_subrule {
    return 
        "$_[1] negate( '$_[0]', \n" .
        call_subrule( $_[0], $_[1]."    " ) .
        "$_[1] )\n";;
}
sub constant {
    my $literal = shift;
    my $name = quotemeta( "$literal" );
    return "$_[0] constant( \"$name\" )\n";
}
sub variable {
    my $name = "$_[0]";
    # print "var name: ", $_[0], "\n";
    my $value = "sub { die 'not implemented: $name' }\n";
    $value = eval $name if $name =~ /^\$/;
    $value = join('', eval $name) if $name =~ /^\@/;

    # XXX - what hash/code interpolate to?
    # $value = join('', eval $name) if $name =~ /^\%/;

    return "$_[1] constant( '" . $value . "' )\n";
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
        emit_rule($program) . 
        "$_[1] )\n";
}
sub colon {
    my $num = $_[0];
    return "$_[1] alternation( [ null(), abort() ] ) \n"
        if $num == 1;
    die (':' x $num) . " not implemented";
}
sub metasyntax {
    # <cmd>
    my $cmd = $_[0];   
    my $prefix = substr( $cmd, 0, 1 );
    if ( $prefix eq '@' ) {
        return "$_[1] alternation( \\$cmd )\n";
    }
    if ( $prefix eq '$' ) {
        return "$_[1] $cmd->( \@_ )\n";
    }
    if ( $prefix =~ /[_[:alnum:]]/ ) {
        return "$_[1] sub { \$grammar->$cmd( \@_ ) }\n";
    }
    die "<$cmd> not implemented";
}

1;
