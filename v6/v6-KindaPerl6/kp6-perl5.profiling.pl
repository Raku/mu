package Main;

use lib 'lib5';
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
my ($dumpast, $perl5, $perl6, $perl5rx,$profile);
my @visitors;

{
    use Getopt::Long;
    GetOptions(
        'ast'	    => \$dumpast,
        'perl5'	    => \$perl5,
        'perl5rx'   => \$perl5rx,
        'perl6'	    => \$perl6,
        'profile'   => \$profile,
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
    elsif ( $perl5rx ) {
        push @visitor_sequence, qw( RegexCapture MetaClass EmitPerl5Regex )
            unless @visitor_sequence && $visitor_sequence[-1] eq 'EmitPerl5Regex';
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

use Time::HiRes qw(time);

my $source = join('', <> );
my $pos = 0;

my %time_usage;
COMPILER::env_init;
while ( $pos < length( $source ) ) {

    my $start = time();
    my $p = KindaPerl6::Grammar->comp_unit($source, $pos);
    #warn "parsing took:",time()-$start,"\n";
    $time_usage{parsing} += time()-$start;

    my $ast = $$p;

    unless (ref $ast && $ast->isa("CompUnit")) {
        # Compilation failed, show the user where
        die report_error(\$source, $pos);
    }
    for (@visitors) {
        my $start = time();
        $ast = $ast->emit( $_ );
        #warn ref $_," took:",time()-$start,"\n";
        $time_usage{ref $_} += time()-$start;
    }
    print $ast;
    $pos = $p->to;
}
# emit CHECK blocks
#my $start = time();
for ( @COMPILER::CHECK ) { 
    my ( $ast, $pad ) = @$_;
    unshift @COMPILER::PAD, $pad;
    my $ast = COMPILER::begin_block( $ast );
    $ast = $ast->emit( $_ ) for @visitors;
    print $ast;
    shift @COMPILER::PAD;
}
#warn "check blocks took:",time()-$start,"\n";
if ($profile) {
    for (keys %time_usage) {
        warn "$_ took $time_usage{$_} time\n";
    }
}

# Helper sub to show the user where the parser bailed out.
sub report_error
{
    my ($source, $pos) = @_;

    # Is this the first line? We'll have to special case if it is
    my $first_line = 0;

    # So we died, find out what line we were on
    my $source_uptohere = substr $$source, 0, $pos;

    # Find how many lines we've been through
    my $lines = ($source_uptohere =~ tr/\n//) + 1;

    # The column is distance from the last newline to $pos :)
    my $last_n_pos = rindex $source_uptohere, "\n";

    if ($last_n_pos == -1) {
        # No previous newline, this is the first line
        $first_line = 1;
        $last_n_pos = 0;
    }

    my $column = $pos - $last_n_pos;

    # Print out the offending newline
    my $next_n_pos = index $$source, "\n", $last_n_pos + 1;
    my $line_length = $next_n_pos - $last_n_pos;
    my $line = substr $$source, $last_n_pos, $line_length;

    # print out an arrow pointing to the column
    my $whitespace = " " x $column;

    "syntax error at position $pos, line $lines column $column:"
    . ($first_line ? "\n" : "")
    . $line . "\n"
    . $whitespace . "^ HERE\n";
}
