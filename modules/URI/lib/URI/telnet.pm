use v6;

class URI::telnet isa URI::_login trusts URI {
  method default_port() { 23 }
}

1;
