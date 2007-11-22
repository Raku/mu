use v6-alpha;
class Foo {
    has $.x;
}
module Main {
    say '1..4';

    subset StrOk of Str where { $_ eq 'ok' };

    # negative tests
    my $foo = Foo.new();
    if $foo.does( 'Foo' ) {
        say "ok 1"
    }
    else {
        say "not ok 1"
    };
    if $foo.does( 'StrOk' ) {
        say "not ok 2"
    }
    else {
        say "ok 2"
    };

    # Str negative tests
    my $foo = "Foo";
    if $foo.does( 'StrOk' ) {
        say "not ok 3"
    }
    else {
        say "ok 3"
    };

    # positive tests
    my $foo = "ok";
    if $foo.does( 'StrOk' ) {
        say "ok 4"
    }
    else {
        say "not ok 4"
    };
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

