module Email::Envelope-0.01;
class Email::Envelope;
use v6;

use Email::Simple;
use Email::Address;
use Regexp::Common <net number>; # XXX -- probably that will change in future

=head1 NAME

Email::Envelope - Email with SMTP time information

=head1 SYNOPSIS

 use Email::Envelope;
 my Email::Envelope $mailenv .= new();
 $mailenv.remote_host = "mx.example.com";
 ...

OR

 my Email::Envelope $mailenv -= new(
  remote_port => 29,
  secure      => 1,
  ...
 );

=head1 DESCRIPTION

This module has been designed as a simple container with a few handy
methods. Currently Email::Simple contains RFC2822 data, however when
dealing with filtering Email, sometimes the information available just
isn't enough. Many people may wish to block certain subnets or run
SBL/XBL checks. This module has been provided for this very reason.

=head1 METHODS

=head2 new

Currently the constructor supports adding data for the following data:

 remote_host
 remote_port
 local_host
 local_port
 secure
 rcpt_to
 mail_from
 helo
 data
 mta_msg_id
 received_timestamp

And can be used as so:

 my Email::Envelope $mailenv .= new(
   remote_host => '127.0.0.1',
   local_host  => 'mx.example.com',
   ...
 );

=cut

subtype Str::Host of Str where {
  m/<$RE<net><IPv4>>$|^<$RE<net><domain><-nospace>>/;
  # XXX -- will probably need fixing
}

subtype Int::Port of Int where { 1 <= $^port <= 65535 }

has Str           $:data;
has Email::Simple $.simple;
has Str::Host     ($.remote_host, $.local_host) is rw;
has Int::Port     ($.remote_port, $.local_port) is rw;
has Bool          $.secure      is rw;
has Str           $.mta_msg_id  is rw;
has Int           $.received_timestamp is rw;
has Str           ($.mail_from, $.rcpt_to) is rw;
has Str           $.helo        is rw;

submethod BUILD(
  $.remote_host, $.remote_port,
  $.local_host,  $.local_port,
  $.secure,      $.helo,
  $.rcpt_to,     $.mail_from,
  $data,
  $.mta_msg_id,
  $.received_timestamp,
) {
  .data = $data if defined $data;
}

=head2 remote_host

Simple accessor. Will only accept either an IP address or a Fully Qualified Domain Name.
Will die upon wrong value being set.

 $mailenv.remote_host = '127.0.0.1';
 say $mailenv.remote_host;

 $mailenv.remote_host = 'mx.example.com';
 say $mailenv.remote_host;

=head2 remote_port

Simple accessor. Will only accept a positive integer.
Will die upon wrong value being set.

 $mailenv.remote_port = 25;
 say $mailenv.remote_port;

=head2 local_host

Simple accessor. Will only accept either an IP address or a Fully Qualified
Domain Name.  Will die upon wrong value being set.

 $mailenv.local_host = '127.0.0.1';
 say $mailenv.local_host;

 $mailenv.local_host = 'mx.example.com';
 say $mailenv.local_host;

=head2 local_port 

Simple accessor. Will only accept a positive integer.
Will die upon wrong value being set.

 $mailenv.local_port = 25;
 say $mailenv.local_port;

=head2 secure

Simple accessor. Requires either a 'true' or 'false' value.

 $mailenv.secure = true;
 $mailenv.secure = false;
 say "Secured" if $mailenv.secure;

=head2 mta_msg_id

Simple accessor/mutator. Will take an arbitary string representing the message ID that the MTA has assigned.

 $mailenv.mta_msg_id = "Exim-2004/22927437493-189282";
 say "MTA reports this message as " . $mailenv.mta_msg_id;

=head2 recieved_timestamp

Simple accessor/mutator. Will take a unix epoch to represent the time that the
message arrived with the MTA.

 $mailenv.recieved_timestamp = 103838934;
 my $dt = Date::Time.new($mailenv.recieved_timestamp);

=head2 rcpt_to

Simple Accessor.

 $mailenv.rcpt_to = "Example User <user\@example.com>";
 print $mailenv.rcpt_to;

 $mailenv.rcpt_to = 
  "Example User <user\@example.com>, Another User <another\@example.com>";
 print $mailenv.rcpt_to;

=cut

=head2 mail_from

Simple Accessor.

 $mailenv.mail_from = "Example User <user\@example.com>";
 print $mailenv.mail_from;

=head2 helo

Simple Accessor.

 $mailenv.helo = "HELO mx.example.com";
 print $mailenv.helo;

=head2 data

Simple accessor. Uses an L<Email::Simple> object internally.

 $mailenv.data = $rfc2822;
 print $mailenv.data;

=cut

method data() is rw {
  return new Proxy:
    FETCH => { ~$.simple },
    STORE => {
      $:data   = $^new;
      $.simple = Email::Simple.new($:data) if defined $:data;
    };
}

=head2 simple

Simple getter. Will return an L<Email::Simple> object based on the DATA that the current object contains.

 my $simple = $mailenv.simple;

=head2 to_address

Simple getter. Will return an L<Email::Address> object based on the RCPT_TO
address that the current object contains.

 my @addresses = $mailenv.to_address;

=cut

method to_address() { Email::Address.parse($.rcpt_to) }

=head2 from_address

Simple getter. Will return an L<Email::Address> object based on the MAIL_FROM address that the current object contains.

 my $address = $mailenv.from_address;

NB: Since RFC 2821 states that there can only be one MAIL_FROM address per smtp
session, if you supply more than one MAIL_FROM format to mail_from() then you
will only recieve back the first address in the list.

=cut

method from_address() { Email::Address.parse($.mail_from) }

1;

=head1 COVERAGE

This module has been written using test-first development. Below are the 
Devel::Cover details.

 ---------------------------- ------ ------ ------ ------ ------ ------ ------
 File                           stmt branch   cond    sub    pod   time  total
 ---------------------------- ------ ------ ------ ------ ------ ------ ------
 blib/lib/Email/Envelope.pm    100.0   90.5  100.0  100.0  100.0  100.0   97.8
 Total                         100.0   90.5  100.0  100.0  100.0  100.0   97.8
 ---------------------------- ------ ------ ------ ------ ------ ------ ------

=head1 HISTORY

=over

=item 0.01

Initial release to CPAN.

=item 0.00_02

Fixes to how Email::Address is used. Added mta_msg_id and received_timestamp.

=item 0.00_01

Initial implementation.

=back

=head1 TODO

=over

=item IPv6 support

=back

=head1 SEE ALSO

L<Email::Simple> L<Email::Address>

=head1 AUTHOR

Scott McWhirter E<lt>kungfuftr@cpan.orgE<gt>

=head1 SUPPORT

This module is part of the Perl Email Project - http://pep.kwiki.org/

There is a mailing list at pep@perl.org (subscribe at pep-subscribe@perl.org) 
and an archive available at http://nntp.perl.org/group/pep.php

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2005 by Ingo Blechschmidt <iblech@web.de> (port to Perl 6)

Copyright (C) 2004 by Scott McWhirter

This library is released under a BSD licence, please see 
L<http://www.opensource.org/licenses/bsd-license.php> for more
information.

=cut
