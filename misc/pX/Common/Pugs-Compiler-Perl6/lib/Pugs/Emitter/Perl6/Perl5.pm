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
    my $tab = $_[1] . '  ';
    #print "_emit: ", Dumper( $n );
    die "unknown node: ", Dumper( $n )
        unless ref( $n ) eq 'HASH';

    if ( $n->{assoc} eq 'list' ) {
        return assoc_list( $n, $tab );
    }
    if ( $n->{call} ) {
        return sub_call( $n, $tab );
    }
    if ( $n->{method_call} ) {
        return method_call( $n, $tab );
    }
    return assoc_none( $n, $tab );
}
sub assoc_list {
    my $n = $_[0];
    my $tab = $_[1] . '  ';
    #print "list emit_rule: ", Dumper( $n );

    if ( $n->{op1} eq ';' ) {
        return join ( ";\n", 
            map { _emit( $_ ) } @{$n->{list}} 
        );
    }

    return "$tab die 'not implemented list-op: " . $n->{op1} . "'";
}
sub assoc_none {
    my $n = $_[0];
    my $tab = $_[1] . '  ';
    #print "none emit_rule: ", Dumper( $n );
    return "$tab die 'not implemented op: " . $n->{op1} . "'";
}
sub sub_call {
    my $n = $_[0];
    my $tab = $_[1] . '  ';
    #print "call: ", Dumper( $n );

    if ( $n->{call}{sub}{bareword} eq 'use' &&
        $n->{call}{param}{call}{sub}{bareword} eq 'P6' ) {
        return "$tab # use P6";
    }

    # XXX
    return $tab . $n->{call}{sub}{bareword} .
        '(' .
        join ( ";\n", 
            map { _emit( $_ ) } @{$n->{call}{param}} 
        ) .
        ')';
}
sub method_call {
    my $n = $_[0];
    my $tab = $_[1] . '  ';
    #print "method call: ", Dumper( $n );

    if ( $n->{method_call}{method}{bareword} eq 'say' &&
        $n->{method_call}{self}{double_quoted} ) {
        return $tab . ' print "' .
            $n->{method_call}{self}{double_quoted} .
            '", "\n"';
    }

    # XXX
    return $tab . $n->{method_call}{sub}{bareword} .
        '(' .
        join ( ";\n", 
            map { _emit( $_ ) } @{$n->{method_call}{param}} 
        ) .
        ')';
}
1;
