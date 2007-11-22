use v6-alpha;
class Foo {
    has $.thing;

    method ok1() {
        say "ok ", self.thing;
    };
}
module Main {
    say '1..2';
    my $foo = Foo.new();
    $foo.thing = 1;
    $foo.ok1();
    $foo.thing = $foo.thing + 1;
    $foo.ok1();
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

