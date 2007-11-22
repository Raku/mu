
{
    my $CORO;
    my $i;
    # problem - inner blocks MUST be coros too, because of the way vars need to be declared
    sub mysub {
        goto $CORO if $CORO;
        {
            $i = 42;
            print "num: $i \n";
            # yield
            $CORO = 'HERE1';
            return ;
            HERE1: ;
        };
        {
            $i++;
            print "num: $i \n";
            # yield
            $CORO = 'HERE2';
            return ;
            HERE2: ;
        };
        print "num - end\n";
        # plain return
        $CORO = undef;
        return;
    }
}

{
    my $CORO2;
    sub myothersub {
        my $a;
        goto $CORO2 if $CORO2;
        {
            $a = 'x';
            print "str: $a \n";
            # yield
            $CORO2 = 'HEREA';
            return $END;
            HEREA: ;
        };
        print "str - end\n";
        # plain return
        $CORO2 = undef;
        return;
    }
}

for (1..10) {
    mysub();
    myothersub();
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
