package Pugs::Emitter::Perl6::Perl5;

# p6-ast to perl5 emitter

use strict;
use warnings;
use Data::Dumper;
$Data::Dumper::Indent = 1;

sub emit {
    my ($grammar, $ast) = @_;
    # runtime parameters: $grammar, $string, $state, $arg_list
    # rule parameters: see Runtime::Rule.pm
    return 
        "do{\n" .
        _emit( $ast, '    ' ) . "\n" .
        "}";
}

sub _emit {
    my $n = $_[0];
    my $tab = $_[1];
    #print "_emit: ", Dumper( $n );
    die "unknown node: ", Dumper( $n )
        unless ref( $n ) eq 'HASH';
        
    return $n->{int} 
        if exists $n->{int};
        
    return $n->{scalar} 
        if exists $n->{scalar};
        
    return $n->{array} 
        if exists $n->{array};
        
    return $n->{hash} 
        if exists $n->{hash};
        
    return '"' . $n->{double_quoted} . '"' 
        if exists $n->{double_quoted};
            
    return '\'' . $n->{single_quoted} . '\'' 
        if exists $n->{single_quoted};
            
    return assoc_list( $n, $tab )
        if exists $n->{assoc} && $n->{assoc} eq 'list';
    return infix( $n, $tab )
        if exists $n->{fixity} && $n->{fixity} eq 'infix';
    return prefix( $n, $tab )
        if exists $n->{fixity} && $n->{fixity} eq 'prefix';
    return postfix( $n, $tab )
        if exists $n->{fixity} && $n->{fixity} eq 'postfix';
    return circumfix( $n, $tab )
        if exists $n->{fixity} && $n->{fixity} eq 'circumfix';
    return default( $n, $tab );
}

sub assoc_list {
    my $n = $_[0];
    my $tab = $_[1];
    # print "list emit_rule: ", Dumper( $n );

    if ( $n->{op1} eq ';' ||
         $n->{op1} eq ',' ) {
        return join ( $n->{op1} . "\n", 
            map { _emit( $_, $tab ) } @{$n->{list}} 
        );
    }
    
    return "$tab die 'not implemented list-op: " . $n->{op1} . "'";
}

sub default {
    my $n = $_[0];
    my $tab = $_[1];
    #print "emit: ", Dumper( $n );
    
    if ( $n->{op1} eq 'call' ) {
        if ( $n->{sub}{bareword} eq 'use' &&
            $n->{param}{cpan_bareword} eq 'v6-pugs' ) {
            return "$tab # use v6-pugs";
        }
        return $tab . $n->{sub}{bareword} . ' ' . _emit( $n->{param}, '  ' ) 
            if $n->{sub}{bareword} eq 'print';
        return $tab . 'print ' . _emit( $n->{param}, '  ' ) . ', "\n"'
            if $n->{sub}{bareword} eq 'say';
        return $tab . $n->{sub}{bareword} . '(' . _emit( $n->{param}, '  ' ) . ')';
    }
    
    if ( $n->{op1} eq 'method_call' ) {    
        if ( $n->{method}{bareword} eq 'print' ) {
            return $tab . ' print ' . _emit( $n->{self}, '  ' );
        }
        if ( $n->{method}{bareword} eq 'say' ) {
            return $tab . ' print ' . _emit( $n->{self}, '  ' ) . ', "\n"';
        }
        return $tab . $n->{sub}{bareword} .
            '(' .
            join ( ";\n", 
                map { _emit( $_, $tab ) } @{$n->{param}} 
            ) .
            ')';
    }

    return "$tab die 'not implemented syntax: " . Dumper( $n ) . "'";
}

sub infix {
    my $n = $_[0];
    my $tab = $_[1];
    # print "infix: ", Dumper( $n );
    
    if ( $n->{op1}{op} eq '~' ) {
        return _emit( $n->{exp1}, $tab ) . ' . ' . _emit( $n->{exp2}, $tab );
    }
    if ( $n->{op1}{op} eq '+'  ||
         $n->{op1}{op} eq '-'  ||
         $n->{op1}{op} eq '==' ||
         $n->{op1}{op} eq '=' ) {
        return _emit( $n->{exp1}, $tab ) . $n->{op1}{op} . _emit( $n->{exp2}, $tab );
    }
    
    return "$tab die 'not implemented infix: " . Dumper( $n ) . "'";
}

sub circumfix {
    my $n = $_[0];
    my $tab = $_[1]; 
    # print "infix: ", Dumper( $n );
    
    if ( $n->{op1}{op} eq '(' &&
         $n->{op2}{op} eq ')' ) {
        return '(' . _emit( $n->{exp1}, $tab ) . ')';
    }
    
    return "$tab die 'not implemented circumfix: " . Dumper( $n ) . "'";
}

sub prefix {
    my $n = $_[0];
    my $tab = $_[1];
    # print "prefix: ", Dumper( $n );
    
    if ( $n->{op1}{op} eq 'my' ||
         $n->{op1}{op} eq 'our' ) {
        return $n->{op1}{op} . ' ' . _emit( $n->{exp1}, $tab );
    }
    if ( $n->{op1}{op} eq '++' ||
         $n->{op1}{op} eq '--' ) {
        return $n->{op1}{op} . _emit( $n->{exp1}, $tab );
    }
    
    return "$tab die 'not implemented prefix: " . Dumper( $n ) . "'";
}

sub postfix {
    my $n = $_[0];
    my $tab = $_[1];
    # print "postfix: ", Dumper( $n );

    if ( $n->{op1}{op} eq '++' ||
         $n->{op1}{op} eq '--' ) {
        return _emit( $n->{exp1}, $tab ) . $n->{op1}{op};
    }
    
    return "$tab die 'not implemented postfix: " . Dumper( $n ) . "'";
}

1;
