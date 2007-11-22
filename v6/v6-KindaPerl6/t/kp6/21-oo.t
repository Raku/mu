use v6-alpha;
class Foo {
    method ok1() {
        say "ok 1";
    };
    method ok2() {
        say "ok 2";
    };
    method ok() {
        say "ok "~@_[0];
    };
    method twice_ok($first,$second) {
        say "ok "~$first;
        say "ok "~$second;
    }
}
module Main {
    say '1..6';
    my $foo = Foo.new();
    $foo.ok1();
    $foo.ok2();
    $foo.ok(3);
    $foo.ok(4);
    $foo.twice_ok(5,6);
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

