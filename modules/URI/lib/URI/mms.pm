use v6;

class URI::mms isa URI::http trusts URI {
  method default_port() { 1755 }
}

1;
