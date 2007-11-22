class Parent {
    has $.foo;
    has $.bar;
};
class Child is Parent {
}
module Main {
    my $a = Child.new();
    $a.foo = 'not ok 1';
    $a.bar = 'not ok 2';

    $a.foo('ok 1');
    $a.bar('ok 2');

    say '1..2';
    say $a.foo;
    say $a.bar;
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

