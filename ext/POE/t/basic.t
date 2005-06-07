#!/usr/bin/pugs

use v6;

use POE;
use Test;

plan 1;

class MySession is POE::Session {
  method dispatch(Str $event, *@args) {
    given $event {
      when "say_hello" {
        return "Hello, @args[0]!";
      }
    }
  }
}

my $session = MySession.new;
my $ret     = $POE::Kernel.post($session, "say_hello", "Ingo");
is $ret, "Hello, Ingo!", '$POE::Kernel.call works';
