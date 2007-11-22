class Main {
    use SDL::App:from<perl5>;
    use SDL::Rect:from<perl5>;
    use SDL::Color:from<perl5>;

    my $app = SDL::App.new("-width",640,"-height",480,"-depth",16);

    my $rect = SDL::Rect.new(
        "-height",100,
        "-width",100,
        "-x",270,
        "-y",390,
    );
    my $color = SDL::Color.new(
        "-r",0,
        "-g",0,
        "-b",255,
    );
    $app.fill($rect,$color);
    $app.update($rect);
    sleep(1);
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
