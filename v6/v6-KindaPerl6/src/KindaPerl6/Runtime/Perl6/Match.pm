use v6-alpha;

class Match is Capture {
    has $.from;
    has $.to;
    has $.result;
    has $.bool;
    has $.match_str;

    method Str {
        if ($.result) {
            return $.result.Str;
        }
           $.bool
        ?? substr( $.match_str, $.from, $.to - $.from )
        !! undef;
    };

    method scalar {
        if ($.result) {
            return $.result;
        } else {
            return self.Str()
        }
    };

    method true {
        return ($.bool).true;
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
