# Junction.pm

# ChangeLog
#
# 2007-08-29
# * moved to kp6
# 2005-09-27
# * created this file (PIL-Run)
#

class Junction {
    has $.type;
    has $.things;

    method Str {
        my %sep = {
            "any" =>" | ",
            "none"=>" , ",
            "all" =>" & ",
            "one" =>" ^ ",
        };

        # if $.type is not set then prefix with 'none'
        #   or else prefix with nothing.
        ( ($.type eq '!') ?? 'none' !! '' )
            ~ "( "
            ~ ( @( $.things ) ).join( %sep{ $.type } )
            ~ " )"
    };

    method perl { self.Str };

    method true {
        my $thing;  # XXX
        if $.type eq 'any' {
            for @( $.things ) -> $thing {
                if $thing { return True; };
            };
            return False;
        };
        if $.type eq 'all' {
            for @( $.things ) -> $thing {
                if !$thing { return False; };
            };
            return True;
        };
        if $.type eq 'none' {
            for @( $.things ) -> $thing {
                if $thing { return False; };
            };
            return True;
        };
        if $.type eq 'one' {
            my $counter = 0;
            for @( $.things ) -> $thing {
                if $thing {
                    ++$counter;
                    if $counter > 1 {
                        return False;
                    };
                };
            };
            return $counter == 1;
        };
    };

};

# vim: sw=4 ts=4 expandtab syn=perl6

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
