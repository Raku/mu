
say '1..4';

my $s = Gather.new( sub { take 42 } );

say 'ok 1 - load Gather.pm';

if ( $s[0] == 42 ) {
  say 'ok 2 - take works';
}
else {
  say 'not ok 2';
};


$s = Gather.new( sub {
    my $i = 0;
    while 1 { take $i; $i = $i + 1 };
  } );

if ( $s[5] == 5 ) {
  say 'ok 3 - lazy take works';
}
else {
  say 'not ok 3';
};

$s = gather { take 42 };
if ( $s[0] == 42 ) {
  say 'ok 4 - gather syntax works';
}
else {
  say 'not ok 4';
};


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

