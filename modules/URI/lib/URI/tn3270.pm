use v6;

class URI::tn3270 isa URI::_login trusts URI {
  method default_port() { 23 }
}

1;
