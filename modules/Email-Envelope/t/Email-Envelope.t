#!/usr/bin/perl6

use Test;
BEGIN { plan 51 }
use Email::Simple;
use Email::Envelope;

my $mailnofold = ~Email::Simple.new(slurp "t/mail/josey-nofold");
my $mailfold   = ~Email::Simple.new(slurp "t/mail/josey-fold");

# Test setting values with hashref
my Email::Envelope $emailenv .= new(
  remote_host => '127.0.0.1',
  remote_port => 8888,
  local_host  => '127.0.0.1',
  local_port  => 9999,
  secure      => true,
  rcpt_to     => "Example User <user\@example.com>",
  mail_from   => "Another User <another\@example.com>",
  helo        => "HELO mx.example.com",
  data        => slurp("t/mail/josey-nofold"),
  mta_msg_id  => "foobar1234567890",
  recieved_timestamp => 1107115785,
);

ok $emailenv ~~ Email::Envelope;

is $emailenv.remote_host, '127.0.0.1', "remote_host set hashref";
is $emailenv.remote_port, 8888, "remote_port set hashref";

is $emailenv.local_host, '127.0.0.1', "local_host set hashref";
is $emailenv.local_port, 9999, "local_port set hashref";

is $emailenv.secure, true, "secure set hashref";

is $emailenv.rcpt_to, "Example User <user\@example.com>", "rcpt_to set hashref";
is $emailenv.mail_from, "Another User <another\@example.com>", "mail_from set hashref";
is $emailenv.helo, "HELO mx.example.com", "helo set hashref";
is $emailenv.data, $mailnofold, "data set hashref";

is $emailenv.mta_msg_id, "foobar1234567890", "mta_msg_id set hashref";
is $emailenv.recieved_timestamp, 1107115785, "recieved_timestamp set hashref";

# Test setting values with accessors
is $emailenv.remote_host = 'mx.example.org', 'mx.example.org', "remote_host set accessor";
is $emailenv.remote_host, 'mx.example.org', "remote_host get accessor";
is $emailenv.remote_port = 7777, 7777, "remote_port set accessor";
is $emailenv.remote_port, 7777, "remote_port get accessor";

is $emailenv.local_host = 'mx.example.net', 'mx.example.net', "local_host set accessor";
is $emailenv.local_host, 'mx.example.net', "local_host get accessor";
is $emailenv.local_port = 6666, 6666, "local_port set accessor";
is $emailenv.local_port, 6666, "local_port get accessor";

is $emailenv.secure = false, false, "secure set accessor";
is $emailenv.secure, false, "secure get accessor";

is $emailenv.rcpt_to = "Example Foo <foo\@example.com>", "Example Foo <foo\@example.com>", "rcpt_to set accessor";
is $emailenv.rcpt_to, "Example Foo <foo\@example.com>", "rcpt_to get accessor";
is $emailenv.mail_from = "Example Bar <bar\@example.com>", "Example Bar <bar\@example.com>", "mail_from set accessor";
is $emailenv.mail_from, "Example Bar <bar\@example.com>", "mail_from get accessor";
is $emailenv.helo = "HELO mx.example.net", "HELO mx.example.net", "helo set accessor";
is $emailenv.helo, "HELO mx.example.net", "helo get accessor";
is $emailenv.data = slurp('t/mail/josey-fold'), $mailfold, "data set accessor";
is $emailenv.data, $mailfold, "data get accessor";

is $emailenv.mta_msg_id = "foobar0987654321", "foobar0987654321", "mta_msg_id set accessor";
is $emailenv.mta_msg_id, "foobar0987654321", "mta_msg_id get accessor";
is $emailenv.recieved_timestamp = 1107115985, 1107115985, "recieved_timestamp set accessor";
is $emailenv.recieved_timestamp, 1107115985, "recieved_timestamp get accessor";

# Test for Email::Address and Email::Simple objects
ok $emailenv.simple       ~~ Email::Simple;
ok $emailenv.to_address   ~~ Email::Address;
ok $emailenv.from_address ~~ Email::Address;

# check for correct values
# -- use C<eval> so that Perl 6's new compile-time type checker doesn't give up :)
catch { $emailenv.remote_host = eval '2idks8u3kjd' }
is $emailenv.remote_host, "mx.example.org";

catch { $emailenv.remote_port = eval 'wjdfhsakjdh' }
is $emailenv.remote_port, 7777;

catch { $emailenv.remote_port = eval -25; }
is $emailenv.remote_port, 7777;

catch { $emailenv.remote_port = eval 9999999999 }
is $emailenv.remote_port, 7777;

catch { $emailenv.local_host = eval '2idks8u3kjd' }
is $emailenv.local_host, "mx.example.net";

catch { $emailenv.local_port = eval 'kasjdhskaj' }
is $emailenv.remote_port, 6666;
catch { $emailenv.local_port = eval -25 }
is $emailenv.remote_port, 6666;
catch { $emailenv.local_port = eval 9999999999 }
is $emailenv.remote_port, 6666;

catch { $emailenv.recieved_timestamp = eval 'jhdfkjhdkjsdbf' }
is $emailenv.received_timestamp, 1107115985;


# check for correct usage of Email::Address
$emailenv.mail_from = "Example Nut <foo\@example.com>, Another Nut <nuts\@example.com";
is $emailenv.mail_from, "Example Nut <foo\@example.com>", "only one address per MAIL_FROM";

$emailenv.rcpt_to = "Example Person <person\@example.com>, Perl Nut <perl\@example.com>, Baka <baka\@example.com>";
my @foobarbaz = $emailenv.to_address;
is +@foobarbaz, 3, "3 addresses";
for $emailenv.to_address -> { ok $_ ~~ Email::Address }
