say "1..1";
say "not ok 1";

# deliberate death;
# this test case will not complete (infinite loop?).
# I am breaking this code so that the test case still fails.
# test cases
#      t/kp6/grammar/15-capture-from-rule-block.t
#      t/kp6/grammar/17-complex-return.t
#
# are both broken with what appears to be the same bug?
# dlocaus@irc.freenode.net #perl6
#
#
#
#grammar MyGrammar {
#    token t1 {
#        'a' { return ::Foo( 'a' => 6 ); }
#    };
#    token t2 {
#        <t1> 'b' { return ::Bar( 't1' => $$<t1>, 'b' => 'ok 4' ); }
#    };
#};
#class Foo {
#    has $.a;
#};
#class Bar {
#    has $.t1;
#    has $.b;
#};
#module Main {
#    say '1..6';
#    $_ = 'ab';
#    my $match = MyGrammar.t2();
#    if $match { say 'ok 1'; };
#    my $result = $$match;
#    if $result { say 'ok 2'; };
#    if $result.isa('Bar') { say 'ok 3'; };
#    say $result.b;
#    my $t1 = $result.t1;
#    if $t1 { say 'ok 5'; } else { say 'not ok 5' };
#    say 'ok ' ~ $t1.a;
#};

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

