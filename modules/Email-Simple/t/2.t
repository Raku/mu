#!/usr/bin/perl6
use v6;

require Test;
require Email::Simple;

plan 3;

# This time, with folding!
sub read_file { local $/; local *FH; open FH, shift or die $!; return <FH> }

my $mail_text = slurp "t/test-mails/josey-fold";

my Email::Simple $mail .= new(source => $mail_text);
isa_ok $mail, "Email::Simple";

is $mail.header("References"), 
   q{<200211120937.JAA28130@xoneweb.opengroup.org>  <1021112125524.ZM7503@skye.rdg.opengroup.org>  <3DD221BB.13116D47@sun.com>},
    "References header checks out";
is $mail.header("reFerEnceS"),
   q{<200211120937.JAA28130@xoneweb.opengroup.org>  <1021112125524.ZM7503@skye.rdg.opengroup.org>  <3DD221BB.13116D47@sun.com>},
    "References header checks out with case folding";
