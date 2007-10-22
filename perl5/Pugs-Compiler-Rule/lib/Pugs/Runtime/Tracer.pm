package Pugs::Runtime::Tracer;

use strict;
use warnings;
use base 'Exporter';

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
       open $::PCR_TRACE_FH, "> $::PCR_TRACE_FILE" or
           die "Can't open tracing output file $::PCR_TRACE_FILE: $!";
   }
   print $::PCR_TRACE_FH @_;
}

sub expand_tracing_code {
    my $s = shift;
    open my $in, '<', \$s or die;
    my ($name);
    my $new;
    while (<$in>) {
        chomp;
        if (/^\s*## <(\w+)>$/) {
            $new .= $_ . "\n";
            $name = $1;
        } elsif (/^(\s*)## pos: (\d+) (\d+)/) {
            my ($tab, $from, $to) = ($1, $2, $3);
            $new .= <<"_EOC_";
$_
$tab do {
$tab   trace_begin('$name', $from, $to, \$pos);
$tab   my \$retval =
_EOC_
        } elsif (/^(\s*)## <\/(\w+)>$/) {
            my ($tab, $n) = ($1, $2);
            if (!defined $name or $n ne $name) {
                die "ERROR: unexpected close tag </$n>";
            } else {
                $new .= <<"_EOC_";
$_
$tab ;
$tab   trace_end('$name', \$retval, \$pos);
$tab   return \$retval;
$tab }
_EOC_
            }
        } else {
            $new .= $_ . "\n";
        }
    }
    return $new;
}

1;

