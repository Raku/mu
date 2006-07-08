use v6-alpha;

=head1 Writing a Unix-Style Filter Program

=cut


# XXX currently it seems you cannot eliminate the $_ from while expression

while $_ = =<> {
    print;
}

