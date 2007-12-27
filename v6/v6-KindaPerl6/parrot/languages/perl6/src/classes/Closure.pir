## $Id: /mirror/parrot-trunk/languages/perl6/src/classes/Sub.pir 23393 2007-12-19T05:28:04.259601Z pmichaud  $

=head1 NAME

src/classes/Closure.pir - methods for the Closure class

=head1 Methods

=over 4

=cut

.namespace ['Closure']

.sub 'onload' :anon :load :init
    $P1 = get_hll_global ['Perl6Object'], 'make_proto'
    $P1('Perl6Closure', 'Closure')
.end

#.sub 'set_outer' :method
#    .param pmc outer
#    say "Outer!!!"
#.end


=back

=cut

# Local Variables:
#   mode: pir
#   fill-column: 100
# End:
# vim: expandtab shiftwidth=4:
