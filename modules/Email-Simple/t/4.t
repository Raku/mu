#!/usr/bin/perl6
use v6;

require Test;
require Email::Simple;

plan 3;

my $mail_text = slurp "t/test-mails/long-msgid";

my Email::Simple $mail .= new(source => $mail_text);
isa_ok $mail, "Email::Simple";

ok $mail.as_string, "Doesn't hang";
