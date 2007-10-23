package Pugs::Runtime::Tracer;

use strict;
use warnings;
use base 'Exporter';
#use Smart::Comments;

our @EXPORT = qw(
   trace_begin trace_end trace
   expand_tracing_code
);

sub trace_begin ($$$$) {
   my ($name, $from, $to, $pos) = @_;
   trace(">>BEGIN $name<< $from..$to at $pos\n");
}

sub trace_end ($$$) {
   my ($name, $res, $pos) = @_;
   trace(">>END $name<< ", $res ? 'success' : 'fail', " at $pos\n");
}

sub trace ($@) {
   if (!defined $::PCR_TRACE_FH) {
       $::PCR_TRACE_FH = \*STDOUT;
   }
   print $::PCR_TRACE_FH @_;
}

sub expand_tracing_code {
    my $s = shift;
    open my $in, '<', \$s or die;
    my (@names, $name, $new, @has_pos);
    while (<$in>) {
        chomp;
        if (/^\s*## <(\w+)>$/) {
            $name = $1;
            push @names, $name;
            push @has_pos, 0;
            ### begin: $name
            $new .= $_ . "\n";
        } elsif (/^(\s*)## pos: (\d+) (\d+)/) {
            my ($tab, $from, $to) = ($1, $2, $3);
            $has_pos[-1] = 1;
            $new .= <<"_EOC_";
$_
$tab do {
$tab   trace_begin('$name', $from, $to, \$pos);
$tab   my \$retval =
_EOC_
        } elsif (/^(\s*)## <\/(\w+)>$/) {
            my ($tab, $n) = ($1, $2);
            $name = pop @names;
            my $has_pos = pop @has_pos;
            ### end: $n . "<=>" . $name
            if (!defined $name || $n ne $name) {
                die "ERROR: unexpected closing tag </$n>";
            } elsif ($has_pos) {
                $new .= <<"_EOC_";
$_
$tab ;
$tab   trace_end('$name', \$retval, \$pos);
$tab   \$retval;
$tab }
_EOC_
            }
            if (!$has_pos) {
                warn "No pos info found for <$n>";
            }
        } else {
            $new .= $_ . "\n";
        }
    }
    return $new;
}

1;

