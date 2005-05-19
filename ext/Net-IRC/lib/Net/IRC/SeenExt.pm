role Net::IRC::SeenExt[: Bool ?$public = 1];

has %:seen;

submethod BUILD() {
  ./add_handler("PRIVMSG", -> $event {
    my $sent_to_a_chan = $event<object> ~~ rx:Perl5/^[#+&]/;
    
    # Only record if the message was sent to a channel.
    %:seen{./normalize($event<from_nick>)} = {
      date => time,
      text => $event<rest>,
    } if $sent_to_a_chan;

    if $public and $event<rest> ~~ rx:Perl5/^\?seen\s+([^ ]+)/ {
      my $reply_to = $sent_to_a_chan ?? $event<object> :: $event<from_nick>;
      my $reply_msg = %:seen{$0}
	?? "$0 was last seen {time() - %:seen{./normalize($0)}<date>} seconds ago, saying: %seen{./normalize($0)}<text>"
	:: "Never seen $0.";
      ./notice(to => $reply_to, text => $reply_msg);
    }
  });
}

method seen(Str $nick) { %:seen{./normalize($nick)} }

1;

=head1 NAME

Net::IRC::SeenExt - Tracks the times people spoke lastly

=head1 SYNOPSIS

  use Net::IRC;
  use Net::IRC::SeenExt;

  my $bot = Net::IRC.new(...);
  $bot does Net::IRC::SeenExt[public => 0];
  # or
  class MyBot does Net::IRC::SeenExt[public => 0] {...}
  my $bot = MyBot.new(...);

  my (date => $date, text => $text) := $bot.seen("iblech");
  say "iblech was last seen {time - $date}s ago, saying: $text";

=head1 DESCRIPTION

C<Net::IRC::SeenExt> is a role which installs a event handler listening
for all C<PRIVMSG>s. Each time people say something (in public), the current
time and the text they said is saved.

If the (optional) role parameter C<public> is set to a true value,
C<Net::IRC::SeenExt> installs a public command handler matching
  ?seen nick

There's also the C<seen(Str $nick)> method, which can be used to query
C<Net::IRC::SeenExt> for the time C<$nick> was last seen, too.

=head1 BUGS

Beware: This role uses syntax which is not yet accepted, see thread "Syntax for
specifying role parameters" on p6l.

=head1 AUTHOR

Ingo Blechschmidt E<lt>iblech@web.deE<gt>

=head1 LICENSE

This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself. See L<perlgpl> and L<perlartistic> for details.

=cut
