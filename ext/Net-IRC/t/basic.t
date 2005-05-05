#!/usr/bin/pugs

use v6;
use Test;
use Net::IRC;

plan 2;

my $bot = new_bot(
  nick     => "blechbot",
  username => "blech",
  ircname  => "Ingo's Bot",
  host     => "localhost",
  port     => 6667,
  autoping => 90,
  live_timeout => 120,
  debug_raw => 0,
);

ok $bot,                     "instantiation of a bot 'object' worked";
is $bot<nick>(), "blechbot", "calling a method on a bot 'object' worked";
