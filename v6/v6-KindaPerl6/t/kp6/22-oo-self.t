use v6-alpha;
class Foo {
    method ok1() {
        say "ok 1";
    };
    method ok2() {
        self.ok1;
        say "ok 2";
    }
}
module Main {
    say '1..2';
    my $foo = Foo.new();
    $foo.ok2();
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

