
use Benchmark qw(:all) ;
use strict;

cmpthese(1000000, {
    'die' => sub {
        eval {
            die bless { answer => 42 }, 'Return';
        };
    },
    'return' => sub {
        return bless { answer => 42 }, 'Return';
    },
    'end' => sub {
        bless { answer => 42 }, 'Return';
    },
});

__END__
eval {
    my $r = bless { answer => 42 }, 'Return';
    die $r;
};
print $@;


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
