use v6-alpha;
class Foo {
    method ok1() {
        1
    };
}
module Main {
    say '1..4';
    my $foo = Foo.new();
    if $foo.isa( 'Foo' ) {
        say "ok 1"
    }
    else {
        say "not ok 1"
    };

    $foo = "Foo";
    if $foo.isa( 'Foo' ) {
        say "not ok 2"
    }
    else {
        say "ok 2"
    };
    if $foo.isa( 'Str' ) {
        say "ok 3"
    }
    else {
        say "not ok 3"
    };
    if $foo.isa( 'Object' ) {
        say "ok 4"
    }
    else {
        say "not ok 4 - inheritance"
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

