role Net::IRC::JoinOnInvite;

submethod BUILD() {
  ./add_handler("INVITE", -> $event {
    my ($from, $chan) = $event<from rest>;
    ./debug("Got an invitation from \"$from\" to join channel \"$chan\".");
    ./join($chan);
  });
}

1;

=head1 NAME

Net::IRC::JoinOnInvite - Role which lets your bot join all channels it's
invite to

=head1 SYNOPSIS

  use Net::IRC;
  use Net::IRC::JoinOnInvite;

  my $bot = Net::IRC.new(...);
  $bot does Net::IRC::JoinOnInvite;
  # or
  class MyBot does Net::IRC::JoinOnInvite {...}
  my $bot = MyBot.new(...);

=head1 DESCRIPTION

C<Net::IRC::JoinOnInvite> is a role which installs a event handler listening
for C<INVITE>. When it is triggered, the bot joins the channel it was invited
to.

=head1 AUTHOR

Ingo Blechschmidt E<lt>iblech@web.deE<gt>

=head1 LICENSE

This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself. See L<perlgpl> and L<perlartistic> for details.

=cut
