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
        if (/\s+## <(\w+)>$/) {
            $name = $1;
            push @names, $name;
            push @has_pos, 0;
            ### begin: $name
            $new .= $_ . "\n";
        } elsif (/(\s+)## pos: (\d+) (\d+)$/) {
            my ($tab, $from, $to) = ($1, $2, $3);
            $has_pos[-1] = 1;
            $new .= <<"_EOC_";
$_
$tab do {
$tab   Pugs::Runtime::Tracer::trace_begin('$name', $from, $to, \$pos);
$tab   my \$retval =
_EOC_
        } elsif (/(\s+)## <\/(\w+)>$/) {
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
$tab   Pugs::Runtime::Tracer::trace_end('$name', \$retval, \$pos);
$tab   \$retval;
$tab }
_EOC_
            }
            if (!$has_pos) {
                #warn "No pos info found for <$n>";
            }
        } else {
            $new .= $_ . "\n";
        }
    }
    return $new;
}

1;
__END__

=head1 NAME

Pugs::Runtime::Tracer - tracer runtime for Pugs::Compiler::Rule

=head1 SYNOPSIS

    use Pugs::Runtime::Tracer;
    trace("blah blah blah");
    trace_begin($regex_name, $regex_pos_from, $regex_pos_to, $input_pos);
    trace_end($regex_name, $success, $input_pos);
    $perl5_code_with_tracing_code = expand_tracing_code($perl5_code);

=head1 DESCRIPTION

This module provides tracing facilities for both PCR compile-time and run-time.

=head1 SEE ALSO

=over

=item *

L<compile_p6grammar.pl>

=item *

"A graphical tracer for Perl 6 regexes based on PCR"

L<http://pugs.blogs.com/pugs/2007/10/a-graphical-tra.html>.

=back

=head1 AUTHOR

Agent Zhang E<lt>agentzh@yahoo.cnE<gt>.

=head1 COPYRIGHT

Copyright 2007 by Yahoo! China Inc. L<http://cn.yahoo.com>.

=head1 LICENSE

This module is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

