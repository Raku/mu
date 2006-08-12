
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

use v5;
sub Sub::Multi::dispatch {
    my $class = shift;
    my $subs = shift;
    my @compat;
    for my $variant (@$subs) {
        my $cv = Data::Bind::_get_cv($variant);
        push @compat, $variant if *$cv->{sig}->is_compatible(@_);
    }
    warn 'I hate vapour ware' unless @compat;
    if (@compat != 1) {
        warn 'I hate ambiguous software';
    }
    goto $compat[0];
}
use v6;

{
    # node types
    my $s = "'abc'";
    my $node = ${ v6::Grammar::Native.str( $s ) };
    my $out = v6::Emitter::Native::Perl::emit( $node );
    print "not " if $s ne $out;
    say "ok 7 # $out";
}

{
    # node types
    my $s = '1';
    my $node = ${ v6::Grammar::Native.num( $s ) };
    my $out = v6::Emitter::Native::Perl::emit( $node );
    print "not " if $s ne $out;
    say "ok 8 # $out";
}

