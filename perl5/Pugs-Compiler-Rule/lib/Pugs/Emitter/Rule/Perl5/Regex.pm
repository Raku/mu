package Pugs::Emitter::Rule::Perl5::Regex;

# p6-rule perl5 emitter for emitting perl5 regexes

=for TODO

    plug into the :ratchet emitter

    @ (non-)interpolation (test)
    aliased, named captures
    nested captures
    quantified captures
    ranges

    die() on captures that would have wrong numbering
    
BUGS:
    - nested captures are not detected
    - set operations on character classes are not detected
    
=cut

use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;

our $capture_count;
our $capture_to_array;

sub emit {
    my ($grammar, $ast, $param) = @_;
    my $sigspace = $param->{sigspace};   
    local $capture_count = -1;
    local $capture_to_array = 0;
    #print "rule: ", Dumper( $ast );
    die "sigspace not supported in P5 mode (can't call <ws> subrule)"
        if $sigspace;
    my $p5regex = '(?m)' . emit_rule( $ast );
    # print ":P5/$p5regex/ \n";
    return $p5regex;
}

sub emit_rule {
    my $n = $_[0];
    die "unknown node: ", Dumper( $n )
        unless ref( $n ) eq 'HASH';
    #print "NODE ", Dumper($n);
    my ($k) = keys %$n;
    my $v = $$n{$k};
    # XXX - use real references
    no strict 'refs';
    #print "NODE ", Dumper($k), ", ", Dumper($v);
    my $code = &$k( $v, '' );
    return $code;
}

#rule nodes

sub non_capturing_group {
    return "(?:" . emit_rule( $_[0] ) . ")";
}        
sub quant {
    my $term = $_[0]->{'term'};
    my $quantifier = $_[0]->{quant}  || '';
    my $greedy     = $_[0]->{greedy} || '';   # + ?
    $greedy = '' if $greedy eq '+';
    #print "QUANT: ",Dumper($_[0]);
    # TODO: fix grammar to not emit empty quantifier
    die "ranges not implemented"
        if ref( $quantifier );
    my $rul;
    {
        #print "Term: ", Dumper($term), "\n";
        my $cap = $capture_to_array;
        local $capture_to_array = $cap || ( $quantifier ne '' );
        $rul = emit_rule( $term );
    }
    my $quant = $quantifier . $greedy;
    return "(?:$rul)$quant" if $quant;
    return $rul;    
}        
sub alt {
    my @s;
    my $count = $capture_count;
    my $max = -1;
    for ( @{$_[0]} ) { 
        $capture_count = $count;
        my $tmp = emit_rule( $_ );
        # print ' ',$capture_count;
        $max = $capture_count 
            if $capture_count > $max;
        push @s, $tmp;    # if $tmp;   
    }
    $capture_count = $max;
    # print " max = $capture_count\n";
    return 
        "(?:" . join( "|", @s ) . ")";
}        
sub alt1 { &alt }
sub concat {
    return join( "",
        map { emit_rule( $_ ) } @{$_[0]}
    );
}        
sub code {
    die "code not implemented";
}        
sub dot { 
    '(?:\n\r?|\r\n?|.)'
}

sub variable {
    die "variable interpolation not implemented";
}
sub special_char {
    my $char = substr($_[0],1);
    return     '(?:\n\r?|\r\n?)'
        if $char eq 'n';
    return     '(?!\n\r?|\r\n?).'
        if $char eq 'N';
    for ( qw( r t e f w d s ) ) {
        return "\\$_"    if $char eq $_;
        return "[^\\$_]" if $char eq uc($_);
    }
    return '\\' . $char;
}
sub match_variable {
    die "no match variables yet";
}
sub closure {
    die "no closures";
}
sub capturing_group {
    my $program = $_[0];
    die "capture to array not implemented"
        if $capture_to_array;
    $capture_count++;
    {
        local $capture_count = -1;
        local $capture_to_array = 0;
        $program = emit_rule( $program )
            if ref( $program );
    }
    return "(" . $program . ")"
}        
sub capture_as_result {
    die "return objects not implemented";
}        
sub named_capture {
    die "no named captures";
}
sub negate {
    die "no negate";
}
sub before {
    my $program = $_[0]{rule};
    $program = emit_rule( $program )
        if ref( $program );
    return "(?=" . $program . ")";
}
sub not_before {
    my $program = $_[0]{rule};
    $program = emit_rule( $program )
        if ref( $program );
    return "(?!" . $program . ")";
}
sub after {
    my $program = $_[0]{rule};
    $program = emit_rule( $program )
        if ref( $program );
    return "(?<=" . $program . ")";
}
sub not_after {
    my $program = $_[0]{rule};
    $program = emit_rule( $program )
        if ref( $program );
    return "(?<!" . $program . ")";
}
sub colon {
    my $str = $_[0];
    return '\z' 
        if $str eq '$';
    return '\A' 
        if $str eq '^';
    return '$' 
        if $str eq '$$';
    return '^' 
        if $str eq '^^';
    die "'$str' not implemented";
}
sub modifier {
    my $str = $_[0];
    die "modifier '$str' not implemented";
}
sub constant {
    return ""
        unless length($_[0]);
    return '\\/' if $_[0] eq '/';  
    return $_[0];  
}

use vars qw( %char_class );
BEGIN {
    %char_class = map { $_ => 1 } qw( 
        alpha alnum ascii blank
        cntrl digit graph lower
        print punct space upper
        word  xdigit
    );
}

sub metasyntax {
    # <cmd>
    my $cmd = $_[0];   
    my $prefix = substr( $cmd, 0, 1 );
    if ( $prefix eq q(') ) {   # single quoted literal ' 
        $cmd = substr( $cmd, 1, -1 );
        $cmd =~ s/([\$\@\%\[\]\+\*\(\)\?\/])/\\$1/g;
        return $cmd;
    }
    if ( $prefix =~ /[-+[]/ ) {   # character class 
        $cmd =~ s/\.\./-/g;
        if ( $prefix eq '-' ) {
           $cmd = '[^' . substr($cmd, 2);
        } 
        elsif ( $prefix eq '+' ) {
           $cmd = substr($cmd, 1);
        }
        $cmd =~ s/\s+|\n//g;
        # XXX <[^a]> means [\^a] instead of [^a] in perl5re
        return $cmd;
    }
    if ( $prefix eq '?' ) {   # non_capturing_subrule / code assertion
        $cmd = substr( $cmd, 1 );
        if ( exists $char_class{$cmd} ) {
            # XXX - inlined char classes are not inheritable, but this should be ok
            return "[[:$cmd:]]";
        }
    }
    if ( $prefix =~ /[_[:alnum:]]/ ) {  
        if ( $cmd eq 'null' ) {
            return ""
        }
    }
    die "<$cmd> not implemented";
}

1;
