use v6-alpha;
class Hash is Container {
    method perl {
        my $s = '{ ';
        for self.pairs -> $pair {
            $s = $s ~ ($pair.key).perl ~ ' => ' ~ ($pair.value).perl ~ ', ';
        };
        return $s ~ ' }'
    };
    method Str {
        ( ( self.pairs ).map( -> $pair { $pair.key ~ "\t" ~ $pair.value}) ).join( "\n" );
    };
    method keys {
        my $pairs = self.pairs;
        $pairs.map( -> $pair {$pair.key});
    };
    method values {
        my $pairs = self.pairs;
        $pairs.map( -> $pair {$pair.value});
    };
    method true { self.elems != 0 };
    method Int  { self.elems };
    method hash { self };
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
