module Main {
    my %a = { a => 4, b => 2 };
    say '1..3';
    %a{'a'} = 1;
    say 'ok ' ~ %a{'a'};
    my $var = 2;
    %a{'a'} := $var;
    say 'ok ' ~ %a{'a'};
    $var = 3;
    if %a{'a'} == 3 {
        say 'ok ' ~ %a{'a'};
    } else {
        say 'not ok 3';
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

