#!/usr/bin/perl6

require Test;
require URI;

plan 28;

my URI $u .= new(uri => 'sip:phone@domain.ext');
is $u.user, "phone";
is $u.host, "domain.ext";
is $u.port, "5060";
is ~$u,     'sip:phone@domain.ext';

$u.host_port = "otherdomain.int:9999";
is $u.host, "otherdomain.int";
is $u.port, "9999";
is ~$u,     'sip:phone@otherdomain.int:9999';

$u.port = "5060";
$u .= canonical;
is $u.host, "otherdomain.int";
is $u.port, "5060";
is ~$u,     'sip:phone@otherdomain.int';

$u.user = "voicemail";
is $u.user, "voicemail";
is ~$u,     'sip:voicemail@otherdomain.int';

my URI $v .= new(uri => 'sip:phone@domain.ext?Subject=Meeting&Priority=Urgent');
is $v.host,  "domain.ext";
is $v.query, "Subject=Meeting&Priority=Urgent";

$v.query_form = (Subject => 'Lunch', Priority => 'Low');
my @q = $v.query_form;
is $u.host,  "domain.ext";
is $u.query, "Subject=Lunch&Priority=Low";
is +@q,      4;
is ~@q,      "Subject Lunch Priority Low";

my URI $w .= new(uri => 'sip:phone@domain.ext;maddr=127.0.0.1;ttl=16');
is $w.host,   "domain.ext";
is $w.params, "maddr=127.0.0.1;ttl=16";

my URI $x .= new(uri => 'sip:phone@domain.ext?Subject=Meeting&Priority=Urgent');
$x.params_form = (maddr => '127.0.0.1', ttl => '16');
my @p = $u.params_form;
is $u.host,   "domain.ext";
is $u.query,  "Subject=Meeting&Priority=Urgent";
is $u.params, "'maddr=127.0.0.1;ttl=16";
is +@p,       4;
is ~@p,       "maddr 127.0.0.1 ttl 16";

my URI $y .= new_abs(uri => 'sip:phone@domain.ext', base => 'sip:foo@domain2.ext');
is ~$y, 'sip:phone@domain.ext';

my URI $z .= new(uri => 'sip:phone@domain.ext');
is ~$z, $z.abs("http://www.cpan.org/");
is ~$z, $z.rel("http://www.cpan.org/");
