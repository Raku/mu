my sub is ($a,$b) {
    if ($a == $b) {
        say "ok";
    } else {
        say "not ok # "~$a~" != "~$b;;
    }
};
sub gets_arg($arg) {
    if ($arg == 7) {
        say "ok";
    } else {
        say "not ok #$arg != 7 $arg="~$arg;
    }
};
my sub f($nr) {
    is($nr,1);
};
sub h($nr) {
    is($nr,2);
};
our sub g($nr) {
    is($nr,3);
};
say "1..4";
gets_arg(7);
f(1);
h(2);
g(3);

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

