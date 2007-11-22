grammar Foo {
token simple {
    'simple'
}
token foo {
    ['foo'|'ba']'baz'
}
token inner {
    inner
}
token outer {
    'outer.' <inner>
}
token code_block {
    foo{return 0}
}
}
say (Match.new()).isa('Capture');
say (Foo.simple("xsimple",1)).perl;
say (Foo.simple("complex",0)).perl;
say (Foo.outer("outer.inner",0)).perl;
say (Foo.code_block("foo",0)).perl;

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
