#!/usr/bin/perl6
use v6;

require Test;
require Email::Simple;

plan 11;

# Very basic functionality test
my $mail_text = slurp "t/test-mails/josey-nofold";
my Email::Simple $mail .= new(source => $mail_text);
isa_ok $mail, "Email::Simple";

ok $mail.head.{From}.[0] ~~ m/Andrew/, "Andrew's in the header";

my $old_from = $mail.header("From");
is $old_from, 'Andrew Josey <ajosey@rdg.opengroup.org>', "We can get a header";
my $sc = 'Simon Cozens <simon@cpan.org>';
is $mail->header("From") = $sc, $sc, "Setting returns new value";
is $mail->header("From"),       $sc, "Which is consistently returned");

# Put andrew back:
$mail->header("From") = $old_from;

my $body = my $old_body = $mail.body;
ok $body ~~ m/Austin Group Chair/, "Body has sane stuff in it";

my $hi = "Hi there!\n";
$mail.body = $hi;
is $mail.body, $hi, "Body can be set properly";

$mail.body = $old_body;
is $mail.as_string, $mail_text, "Good grief, it's round-trippable";
is Email::Simple.new(source => $mail.as_string).as_string, $mail_text, "Good grief, it's still round-trippable";

# With nasty newlines
my $nasty = "Subject: test\n\rTo: foo\n\r\n\rfoo\n\r";
$mail = Email::Simple.new(source => $nasty);
my $test = $mail.as_string;
is $test, $nasty, "Round trip that too";
is Email::Simple.new(source => $mail->as_string).as_string, $nasty, "... twice";
