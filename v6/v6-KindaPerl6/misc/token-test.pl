# compile with:
# perl kp6-perl5.pl --perl5rx < examples/token-test.pl | perltidy

# run with:
# perl -I lib5regex -I compiled/perl5/lib

class X {
  my $zzz;

  method y { 42 };  # just testing

  token ws { \s+ };

  token word { \w+ };

  token x {
    4
    (2)
    [(3)(4)]+
    $<xyz> := (.)
    [ $<abc> := (.) ]+

    $<rep> := (.)
    $<rep> := (.)

    <?ws>
    <before . >
    <word>
    <after \w >
    { 42 }
    { return 123 }
  };

  $_ := '423434XabRR  xyz';
  X.x();
  say 'match: ', $/.perl;
  say 'result: ', $/.result;
}

=begin

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

=end