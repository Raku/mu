grammar Foo {
    token foo {
        'f' 'o' 'o'
    }
    token foo2 {
        foo
    }
}
class Main {
say "1..4";
$_ = 'foo';
if (Foo.foo()) {
    say "ok 1";
} else {
    say "not ok 1";
}
$_ = 'bar';
if (Foo.foo()) {
    say "not ok 2";
} else {
    say "ok 2";
}
$_ = 'foo';
if (Foo.foo2()) {
    say "ok 3";
} else {
    say "not ok 3";
}
$_ = 'bar';
if (Foo.foo2()) {
    say "not ok 4";
} else {
    say "ok 4";
}
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

