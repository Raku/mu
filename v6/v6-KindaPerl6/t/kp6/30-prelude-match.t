use v6-alpha;

module Main {

    say '1..3';

    # my $m = ::Match(
    #    match_str => 'abcdef',
    #    from => 2,
    #    to   => 4,
    #    bool => 1,
    # );

    my $m = Match.new;
    $m.from = 1;
    say "ok ", $m.from, " - accessor";

    $m.from = 2;
    $m.to   = 4;
    $m.bool = 1;
    $m.match_str = 'abcdef';

    if $m.from == 2 {
        say "ok 2 - accessor";
    }
    else {
        say "not ok - got ", $m.from;
    };

    if $m.Str eq 'cd' {
        say "ok 3 - match stringify";
    }
    else {
        say "not ok - got ", $m.Str;
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

