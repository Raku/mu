package Main;

use lib '../v6-MiniPerl6/lib5', 'lib5';
use strict;

BEGIN {
    $Main::_V6_COMPILER_NAME    = 'KindaPerl6';
    $Main::_V6_COMPILER_VERSION = '0.001';
}

use KindaPerl6::Runtime::Perl5::Runtime;  
use KindaPerl6::Grammar;
use KindaPerl6::Traverse;
use KindaPerl6::Grammar::Regex;
use KindaPerl6::Runtime::Perl5::Compiler;  


# --- command line options

=pod

Example:

  # process the ast by expanding 'token' into plain Perl;
  # then replace method calls with MO calls;
  # then emit Perl 5 code
 
  perl kp6-perl5.pl --do Token MetaClass EmitPerl5  < examples/token.pl  | perltidy

=cut

my @visitor_sequence;
my ($dumpast, $perl5, $perl6);
my @visitors;

{
    use Getopt::Long;
    GetOptions(
        'ast'	    => \$dumpast,
        'perl5'	    => \$perl5,
        'perl6'	    => \$perl6,
        'do:s{1,}'  => \@visitor_sequence,
    );

    if ( $perl6 ) {
        push @visitor_sequence, qw( EmitPerl6 )
            unless @visitor_sequence && $visitor_sequence[-1] eq 'EmitPerl6';
    }
    elsif ( $dumpast ) {
        push @visitor_sequence, qw( Perl )
            unless @visitor_sequence && $visitor_sequence[-1] eq 'Perl';
    }
    elsif ( $perl5 ) {
        push @visitor_sequence, qw( EmitPerl5 )
            unless @visitor_sequence && $visitor_sequence[-1] eq 'EmitPerl5';
    }
    elsif ( ! @visitor_sequence ) {
        # this is the default sequence
        push @visitor_sequence, qw( Token MetaClass EmitPerl5 )
    }

    push @visitor_sequence, 'Perl' 
        unless $visitor_sequence[-1] eq 'Perl'
            || $visitor_sequence[-1] =~ /^Emit/;

    for ( @visitor_sequence ) {
        my $module_name = 'KindaPerl6::Visitor::' . $_;
        eval "require $module_name";
        die "Can't load $_ plugin: $@" if $@;
        push @visitors, $module_name->new();
    }
} 
# --- /command line options


my $source = join('', <> );
my $pos = 0;

COMPILER::env_init;
while ( $pos < length( $source ) ) {
    #say( "Source code:", $source );
    my $p = KindaPerl6::Grammar->comp_unit($source, $pos);
    #say( Main::perl( $$p ) );
    my $ast = $$p;
    #print Dump( $ast );
    unless (ref $ast && $ast->isa("CompUnit")) {
        die "Syntax Error\n";
    }
    $ast = $ast->emit( $_ ) for @visitors;
    say $ast;
    $pos = $p->to;
}
# emit CHECK blocks
for ( @COMPILER::CHECK ) { 
    my ( $ast, $pad ) = @$_;
    unshift @COMPILER::PAD, $pad;
    my $ast = COMPILER::begin_block( $ast );
    $ast = $ast->emit( $_ ) for @visitors;
    say $ast;
    shift @COMPILER::PAD;
}
