#    coro mysub {
#        my $i;
#        $i = 42;
#        print "num: $i \n";
#        # yield
#        $i++;
#        print "num: $i \n";
#        # yield
#        print "num - end\n";
#        # return
#    }

our $Sub_mysub = \&mysub;
{
    my $i;
    # problem - inner blocks MUST be coros too, because of the way vars need to be declared
    sub mysub {
            $i = 42;
            print "num: $i \n";
            # yield
            $Sub_mysub = \&mysub_2;
        };
    sub mysub_2 {
            $i++;
            print "num: $i \n";
            # yield
            $Sub_mysub = \&mysub_3;
        };
    sub mysub_3 {
        print "num - end\n";
        # plain return
        $Sub_mysub = \&mysub;
    }
}

{
    my $CORO2;
    sub myothersub {
        goto $CORO2 if $CORO2;
        {
            print "--- \n";
            # yield
            $CORO2 = 'HEREA';
            return $END;
            HEREA: ;
        };
        print "+++ \n";
        # plain return
        $CORO2 = undef;
        return;
    }
}

for (1..10) {
    $Sub_mysub->();
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
