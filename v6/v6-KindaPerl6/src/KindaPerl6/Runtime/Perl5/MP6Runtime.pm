# MP6 Runtime

$INC{'MiniPerl6/Perl5/Runtime.pm'} = 1;
$INC{'MiniPerl6/Perl5/Match.pm'}   = 1;

use strict;
use warnings;

package MiniPerl6::Perl5::Match;

sub new {
    return KindaPerl6::Perl5::Match->new( @_[ 1 .. $#_ ] );
}

package KindaPerl6::Grammar;

no warnings 'uninitialized';

sub space {
    my $grammar = $_[0];
    my $str     = $_[1];
    my $pos     = $_[2];
    my $MATCH;
    $MATCH = KindaPerl6::Perl5::Match->new( 'str' => $str, 'from' => $pos, 'to' => $pos, );
    $MATCH->bool(
        substr( $str, $MATCH->to() ) =~ m/^([[:space:]])/
        ? ( 1 + $MATCH->to( length($1) + $MATCH->to() ) )
        : 0
    );
    $MATCH;
}

sub digit {
    my $grammar = $_[0];
    my $str     = $_[1];
    my $pos     = $_[2];
    my $MATCH;
    $MATCH = KindaPerl6::Perl5::Match->new( 'str' => $str, 'from' => $pos, 'to' => $pos, );
    $MATCH->bool(
        substr( $str, $MATCH->to() ) =~ m/^([[:digit:]])/
        ? ( 1 + $MATCH->to( length($1) + $MATCH->to() ) )
        : 0
    );
    $MATCH;
}

sub word {
    my $grammar = $_[0];
    my $str     = $_[1];
    my $pos     = $_[2];
    my $MATCH;
    $MATCH = KindaPerl6::Perl5::Match->new( 'str' => $str, 'from' => $pos, 'to' => $pos, );
    $MATCH->bool(
        substr( $str, $MATCH->to() ) =~ m/^([[:word:]])/
        ? ( 1 + $MATCH->to( length($1) + $MATCH->to() ) )
        : 0
    );
    $MATCH;
}

sub backslash {
    my $grammar = $_[0];
    my $str     = $_[1];
    my $pos     = $_[2];
    my $MATCH;
    $MATCH = KindaPerl6::Perl5::Match->new( 'str' => $str, 'from' => $pos, 'to' => $pos, );
    $MATCH->bool(
        substr( $str, $MATCH->to(), 1 ) eq '\\'    # '
        ? ( 1 + $MATCH->to( 1 + $MATCH->to() ) )
        : 0
    );
    $MATCH;
}

sub newline {
    my $grammar = $_[0];
    my $str     = $_[1];
    my $pos     = $_[2];
    my $MATCH;
    $MATCH = KindaPerl6::Perl5::Match->new( 'str' => $str, 'from' => $pos, 'to' => $pos, );
    return $MATCH
        unless ord( substr( $str, $MATCH->to() ) ) == 10
            || ord( substr( $str, $MATCH->to() ) ) == 13;
    $MATCH->bool(
        substr( $str, $MATCH->to() ) =~ m/(?m)^(\n\r?|\r\n?)/
        ? ( 1 + $MATCH->to( length($1) + $MATCH->to() ) )
        : 0
    );
    $MATCH;
}

sub not_newline {
    my $grammar = $_[0];
    my $str     = $_[1];
    my $pos     = $_[2];
    my $MATCH;
    $MATCH = KindaPerl6::Perl5::Match->new( 'str' => $str, 'from' => $pos, 'to' => $pos, 'bool' => 0 );
    return $MATCH
        if ord( substr( $str, $MATCH->to() ) ) == 10
            || ord( substr( $str, $MATCH->to() ) ) == 13;
    $MATCH->to( 1 + $MATCH->to );
    $MATCH->bool(1);
    $MATCH;
}

1;

package Main;

sub print {
    print join( '', @_ );
}

sub say {
    print join( '', @_, "\n" );
}

sub chars {
    length( $_[0] );
}

sub newline {
    "\n";
}

sub quote {
    '"';
}

sub singlequote {
    "'";
}

sub backslash {
    "\\";
}

sub isa {
    my $ref = ref( $_[0] );
    ( $ref eq 'ARRAY' && $_[1] eq 'Array' )
        || ( $ref eq 'HASH'
        && $_[1] eq 'Hash' )
        || ( $ref eq ''
        && $_[1] eq 'Str' )
        || $ref eq $_[1]
        || ( ref( $_[1] )
        && $ref eq ref( $_[1] ) );
}

sub perl {
    local $Data::Dumper::Terse = 1;
    my $can = UNIVERSAL::can( $_[0] => 'perl' );
    if ($can) {
        $can->( $_[0] );
    }
    else {
        Data::Dumper::Dumper( $_[0] );

        #require Data::Dump::Streamer;
        #Data::Dump::Streamer($_[0]);
    }
}

sub yaml {
    my $can = UNIVERSAL::can( $_[0] => 'yaml' );
    if ($can) {
        $can->( $_[0] );
    }
    else {
        require YAML::Syck;
        YAML::Syck::Dump( $_[0] );
    }
}

sub join {
    my $can = UNIVERSAL::can( $_[0] => 'join' );
    if ($can) {
        $can->(@_);
    }
    else {
        join( $_[1], @{ $_[0] } );
    }
}

my %table = (
    '$' => '',
    '@' => 'List_',
    '%' => 'Hash_',
    '&' => 'Code_',
);

sub mangle_name {
    my ( $sigil, $twigil, $name, $namespace ) = @_;

    #print "mangle: ($sigil, $twigil, $name, [ @$namespace ] )\n" if $namespace;
    $name = CORE::join( '::', @$namespace, $name ) if $namespace;
    $name =~ s/ ([^a-zA-Z0-9_:] | (?<!:):(?!:)) / '_'.ord($1).'_' /xge;
    my @name = split( /::/, $name );
    $name[-1] = $table{$sigil} . $name[-1];

    #print "name: @name \n";
    if (   $twigil eq '*'
        && @name == 1 )
    {
        unshift @name, 'GLOBAL';
    }
    return '$' . CORE::join( '::', @name );    # XXX - no twigil
}

sub mangle_name_lisp {
    my ( $sigil, $twigil, $name, $namespace ) = @_;

    #print "mangle: ($sigil, $twigil, $name, [ @$namespace ] )\n" if $namespace;
    $name = CORE::join( '::', @$namespace, $name ) if $namespace;
    $name =~ s/ ([^a-zA-Z0-9_:] | (?<!:):(?!:)) / '_'.ord($1).'_' /xge;
    my @name = split( /::/, $name );
    $name[-1] = $table{$sigil} . $name[-1];

    #print "name: @name \n";
    if (   $twigil eq '*'
        && @name == 1 )
    {
        unshift @name, 'GLOBAL';
    }
    $name[-1] = 'kp6-' . $name[-1];
    return $name[-1] if @name == 1;
    return 'kp6-' . CORE::join( '::', @name );    # XXX - no twigil
}

sub mangle_name_ruby {
    my ( $sigil, $twigil, $name, $namespace ) = @_;

    my %table = (
        '$' => 's_',
        '@' => 'a_',
        '%' => 'h_',
        '&' => 'c_',
        );

    #print "mangle: ($sigil, $twigil, $name, [ @$namespace ] )\n" if $namespace;
    $name = CORE::join( '::', @$namespace, $name ) if $namespace;
    $name =~ s/ ([^a-zA-Z0-9_:] | (?<!:):(?!:)) / '_'.ord($1).'_' /xge;
    my @name = split( /::/, $name );
    $name[-1] = $table{$sigil} . $name[-1];

    #print "name: @name \n";
    if (   $twigil eq '*'
        && @name == 1 )
    {
        unshift @name, 'GLOBAL';
    }
    if (   $twigil eq '.' )
    {
	$name[-1] = 'ci' . $name[-1];
    }
    return CORE::join( '::', @name );
}

sub mangle_ident {
    my ($name) = @_;
    $name =~ s/ ([^a-zA-Z0-9_]) / '_'.ord($1).'_' /xge;
    return $name;
}

sub mangle_string {
    my $s = shift;
    $s =~ s/(\\|\')/\\$1/g;
    $s;
}

sub mangle_perl5rx_metasyntax {
    my $s = shift;
    if ( $s =~ /\./ ) {
        $s =~ s/(\.)/::_rule_/;    # '$KindaPerl6::Grammar.ws' -> ::_rule_ws
    }
    else {
        $s = '_rule_' . $s;        # '$_rule_ws'
    }
    $s;
}

sub indent {
    my $s = shift;
    $s =~ s/^/    /mg;
    $s . "\n";
}

sub Dump {
    die "Dump is not implemented";

    #require Data::Dump::Streamer;
    #Data::Dump::Streamer::Dump( @_ );
}

sub get_compiler_target_runtime {
    $ENV{KP6_TARGET_RUNTIME} || "KindaPerl6::Runtime::Perl5::Runtime";
}

sub expand_macro {

    # XXX TODO
    #print "# HERE! @_ \n";
    #GLOBAL::say( ::DISPATCH( $_[0], 'WHAT' ) );
}

sub emit_ruby_kludge_commas {
    my $src = shift;
    $src =~ s/\n([\s\t]*),,,/,\n$1/g;
    $src =~ s/,,,/,/g;
    $src;
}

sub emit_yaml {
    my $node = shift;
    eval("require YAML::Syck;") or die $!;
    local $YAML::Syck::ImplicitTyping = 1;
    local $YAML::Syck::ImplicitUnicode = 1;
    #binmode(STDOUT, ":utf8");
    #print STDOUT
    YAML::Syck::Dump($node);
    #"\n";
}

1;

=head1 AUTHORS

The Pugs Team E<lt>perl6-compiler@perl.orgE<gt>.

=head1 SEE ALSO

The Perl 6 homepage at L<http://dev.perl.org/perl6>.

The Pugs homepage at L<http://pugscode.org/>.

=head1 COPYRIGHT

Copyright 2007 by Flavio Soibelmann Glock and others.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

See L<http://www.perl.com/perl/misc/Artistic.html>

=cut
