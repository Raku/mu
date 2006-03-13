use v5;

# Pugs::Emitter::Rule::Perl5 - fglock
#
# p6-rule perl5 emitter
#

use strict;
use warnings;

package Pugs::Emitter::Rule::Perl5;
use Data::Dumper;

my $namespace = '';    # 'grammar1::';

#------ match functions

=for reference - see S05 "Return values from matches"
    $/    
        the match 
        Inside a closure, refers to the current match, even if there is another
        match inside the closure
    $/()  
        just the capture - what's returned in { return ... }
    $/0
        the first submatch
Alternate names:        
    $()   
        the same as $/()
Submatches:
    $/[0] 
        the first submatch
    $0 
        the same as $/[0]
    $()<a>
        If the capture object return()-ed were a hash:
        the value $x in { return { a => $x ,} }
Named captures:
    $a := (.*?)  
        $a is something in the outer lexical scope (TimToady on #perl6)
            XXX - E05 says it is a hypothetical variable, referred as $0<a>
    $<a> := (.*?)  
        $<a> is a named capture in the match
    let $a := (.*?)
        $a is a hypothetical variable
                (\d+)     # Match and capture one-or-more digits
                { let $digits := $1 }
=cut

sub match::get {
    my $match =   $_[0];
    my $name =    $_[1];
    
    return $match->{capture}  if $name eq '$/()' || $name eq '$()';
    
    # XXX - wrong, but bug-compatible with previous versions
    return $match->{capture}  if $name eq '$/<>' || $name eq '$<>';
    
    return $match->{match}    if $name eq '$/';
    
    #print "get var $name\n";
    
    # XXX - wrong, but bug-compatible with previous versions
    # $/<0>, $/<1>
    # $<0>, $<1>
    if ( $name =~ /^ \$ \/? <(\d+)> $/x ) {
        my $num = $1;
        my $n = 0;
        for ( @{ match::get( $match, '$/()' ) } ) {
            next unless ref($_) eq 'HASH';
            if ( $num == $n ) {
                my (undef, $capture) = each %$_;
                #print "cap\n", Dumper( $capture );
                return $capture;
            }
            $n++;
        }
        return;
    }
    
    # $/()<name>
    # $()<name>
    if ( $name =~ /^ \$ \/? \( \) <(.+)> $/x ) {
        my $n = $1;
        for ( @{ match::get( $match, '$/()' ) } ) {
            next unless ref($_) eq 'HASH';
            if ( exists $_->{$n} ) {
                #print "cap\n", Dumper( $_->{$n} );
                return $_->{$n};
            }
        }
        return;
    }

    # XXX - wrong, but bug-compatible with previous versions
    # $/<name>
    # $<name>
    if ( $name =~ /^ \$ \/? <(.+)> $/x ) {
        my $n = $1;
        for ( @{ match::get( $match, '$()' ) } ) {
            next unless ref($_) eq 'HASH';
            if ( exists $_->{$n} ) {
                #print "cap\n", Dumper( $_->{$n} );
                return $_->{$n};
            }
        }
        return;
    }

    die "match variable $name is not implemented";
    # die "no submatch like $name in " . Dumper( $match->{match} );
}

sub match::str {
    my $match = $_[0];
    #print "STR: ", ref( $match ), " ", Dumper( $match ), "\n";
    return join( '', map { match::str( $_ ) } @$match )
        if ref( $match ) eq 'ARRAY';
    return join( '', map { match::str( $_ ) } values %$match )
        if ref( $match ) eq 'HASH';
    return $match;
}

#------ rule emitter

sub emit {
    my $str = shift;
    my $cmd =  
        "do {\n" . 
        "    package Pugs::Grammar::Rule;\n" .
        "    my \$matcher = \n" .
        emit_rule( $str ) . ";\n" .
        "    return sub {\n" .
        "        my \$match = \$matcher->( \@_ );\n" .
        #"        warn 'matched----', Dumper( \$match );\n" .
        "        return unless \$match->{bool};\n" .
        "        if ( \$match->{return} ) {\n" .
        #"            warn 'pre-return: ', Dumper( \$match );\n" .
        "            my \%match2 = \%\$match;\n" .
        "            \$match2{capture} = \$match->{return}( Match->new( \$match ) );\n" .
        "            delete \$match2{return};\n" .
        "            return \\\%match2;\n" .
        "        }\n" .
        #"        print Dumper( \$match );\n" .
        "        my \$len = length( \$match->{tail} );\n" .
        "        my \$head = \$len?substr(\$_[0], 0, -\$len):\$_[0];\n" .
        "        \$match->{capture} = \$head;\n" .
        "        return \$match;\n" .
        "    }\n" .
        "}\n";
    #print $cmd;
    return $cmd;
}

sub emit_rule_node {
    #warn "rule node...";
    ## die "unknown node type: $_[0]" unless ${$_[0]}; 
    no strict 'refs';
    my $code = &{$_[0]}($_[1],$_[2]);
    #print Dumper(@_,$code),"\n";
    return $code;
}
sub emit_rule {
    my $n = $_[0];
    my $tab = $_[1] || '    '; 
    $tab .= '  ';
    local $Data::Dumper::Indent = 0;
    #print "emit_rule(): ", ref($n)," ",Dumper( $n ), "\n";

    # XXX - not all nodes are actually used

    if ( ref($n) eq '' ) {
        # XXX - this should not happen, but it does
        return '';
    }
    if ( ref( $n ) eq 'ARRAY' ) {
        my @s;
        for ( @$n ) {
            #print "emitting array item ", Dumper($_), "\n";
            my $tmp = emit_rule( $_, $tab );
            #print "emitted: ", $tmp, "\n";
            push @s, $tmp . "$tab ,\n" if $tmp;
        }
        return $s[0] if @s == 1;
        return "$tab Pugs::Runtime::Rule2::concat(\n" . 
               ( join '', @s ) . 
               "$tab )\n";
    }
    elsif ( ref( $n ) eq 'HASH' ) 
    {
        #warn "hash...";
        my ( $k, $v ) = each %$n;
        # print "$tab $k => $v \n";
        return '' unless defined $v;  # XXX a bug?
        emit_rule_node($k,$v,$tab);
    }
    else 
    {
        die "unknown node: ", Dumper( $n );
    }
}

#package node;

#rule nodes
sub rule {
    return emit_rule( $_[0], $_[1] );
}        
sub capturing_group {
    return 
       "$_[1] Pugs::Runtime::Rule2::capture( '',\n" .
       emit_rule( $_[0], $_[1].'    ' ) . 
       "$_[1] )\n" .
       '';
}        
sub non_capturing_group {
    return emit_rule( $_[0], $_[1] );
}        
sub star {
    local $Data::Dumper::Indent = 1;
    my $term = $_[0]->[0]{'term'};
    #print "*** \$term:\n",Dumper $term;
    my $quantifier = $_[0]->[1]{'literal'}[0];
    my $sub = { 
            '*' =>'greedy_star',     
            '+' =>'greedy_plus',
            '*?'=>'non_greedy_star', 
            '+?'=>'non_greedy_plus',
            '?' =>'optional',
            '??' =>'null_or_optional',
        }->{$quantifier};
    # print "*** \$quantifier:\n",Dumper $quantifier;
    die "quantifier not implemented: $quantifier" 
        unless $sub;
    return "$_[1] ruleop::$sub(\n" .
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
    return "$_[1] ruleop::alternation( [\n" . 
           join( '', @s ) .
           "$_[1] ] )\n";
}        
sub term {
    return emit_rule( $_[0], $_[1] );
}        
sub code {
    # return "$_[1] # XXX code - compile '$_[0]' ?\n";
    return "$_[1] $_[0]  # XXX - code\n";  
}        
sub dot {
    return "$_[1] \\&{'${namespace}any'}\n";
}
sub subrule {
    my $name = $_[0];
    $name = $namespace . $_[0] unless $_[0] =~ /::/;
    return "$_[1] Pugs::Runtime::Rule2::capture( '$_[0]', \\&{'$name'} )\n";
}
sub non_capturing_subrule {
    return "$_[1] \\&{'$namespace$_[0]'}\n";
}
sub negated_subrule {
    return "$_[1] ruleop::negate( \\&{'$namespace$_[0]'} )\n";
}
sub constant {
    my $literal = shift;
    my $name = quotemeta( match::str( $literal ) );
    return "$_[0] ruleop::constant( \"$name\" )\n";
}
sub variable {
    my $name = match::str( $_[0] );
    # print "var name: ", match::str( $_[0] ), "\n";
    my $value = "sub { die 'not implemented: $name' }\n";
    $value = eval $name if $name =~ /^\$/;
    $value = join('', eval $name) if $name =~ /^\@/;

    # XXX - what hash/code interpolate to?
    # $value = join('', eval $name) if $name =~ /^\%/;

    return "$_[1] ruleop::constant( '" . $value . "' )\n";
}
sub closure {
    my $code = match::str( $_[0] ); 
    
    # XXX XXX XXX - source-filter - temporary hacks to translate p6 to p5
    # $()<name>
    $code =~ s/ ([^']) \$ \( \) < (.*?) > /$1 match::get( \$_[0], '\$()<$2>' ) /sgx;
    # $<name>
    $code =~ s/ ([^']) \$ < (.*?) > /$1 match::get( \$_[0], '\$<$2>' ) /sgx;
    # $()
    $code =~ s/ ([^']) \$ \( \) /$1 match::get( \$_[0], '\$()' ) /sgx;
    #print "Code: $code\n";
    
    return "$_[1] sub {\n" . 
           "$_[1]     $code;\n" . 
           "$_[1]     return { bool => 1, tail => \$_[0] }\n" .
           "$_[1] }\n"
        unless $code =~ /return/;
        
    return
           "$_[1] ruleop::abort(\n" .
           "$_[1]     sub {\n" . 
           "$_[1]         return { bool => 1, tail => \$_[0], return => sub $code };\n" .
           "$_[1]     }\n" .
           "$_[1] )\n";
}
sub runtime_alternation {
    my $code = match::str( match::get( 
        { capture => $_[0] }, 
        '$<variable>'
    ) );
    return "$_[1] ruleop::alternation( \\$code )\n";
}
sub named_capture {
    my $name = match::str( match::get( 
        { capture => $_[0] }, 
        '$<ident>'
    ) );
    my $program = emit_rule(
            match::get( 
                { capture => $_[0] }, 
                '$<rule>'
            ), $_[1] );
    return "$_[1] Pugs::Runtime::Rule2::capture( '$name', \n" . $program . "$_[1] )\n";
}

1;
