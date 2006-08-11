
use v6-alpha;

use v6::Grammar::Native;

say "1..8";
{
    my $s = '1';
    v6::Grammar::Native.num( $s );
    print "not " unless $/;
    say "ok 1";
}

{
    my $s = "'abc'";
    v6::Grammar::Native.str( $s );
    print "not " unless $/;
    say "ok 2";
}

{
    # non-matches
    my $s = 'abc';
    v6::Grammar::Native.num( $s );
    print "not " if $/;
    say "ok 3";

    v6::Grammar::Native.str( $s );
    print "not " if $/;
    say "ok 4";
}

{
    # node types
    my $s = '1';
    my $node = ${ v6::Grammar::Native.num( $s ) };
    #say $node.ref;
    print "not " if $node.ref ne 'v6::AST::NBit';
    say "ok 5";
}

{
    # node types
    my $s = "'abc'";
    my $node = ${ v6::Grammar::Native.str( $s ) };
    #say $node.ref;
    print "not " if $node.ref ne 'v6::AST::NStr';
    say "ok 6";
}

use v6::Emitter::Native::Perl;

{
    # node types
    my $s = "'abc'";
    my $node = ${ v6::Grammar::Native.str( $s ) };
    say 'string: ', v6::Emitter::Native::Perl::emit( $node.ref );
    print "not " if $node.ref ne 'v6::AST::NStr';
    say "ok 6";
}

{
    # node types
    my $s = '1';
    my $node = ${ v6::Grammar::Native.num( $s ) };
    say 'number: ', v6::Emitter::Native::Perl::emit( $node.ref );
    print "not " if $node.ref ne 'v6::AST::NBit';
    say "ok 5";
}

