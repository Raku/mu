grammar Test {
    token foobar {
        <foo>'bar'
    }
    token foo2 {
        <foo><foo>
    }
    token foofoo {
        <foo>foo
    }
    token foo :Perl5 {foo}
}
say "1..4";
$_ = "foo";
if (Test.foo()) {
    say "ok 1";
} else {
    say "not ok 1";
}
$_ = "bar";
if (Test.foo()) {
    say "not ok 2";
} else {
    say "ok 2";
}
$_ = "foobar";
if (Test.foobar()) {
    say "ok 3";
} else {
    say "not ok 3";
}
$_ = "barbaz";
if (Test.foobar()) {
    say "not ok 4";
} else {
    say "ok 4";
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

