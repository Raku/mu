use v6;

class URI::rlogin isa URI::_login trusts URI {
  method default_port() { 513 }
}

1;
