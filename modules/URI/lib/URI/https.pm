use v6;

class URI::https isa URI::http trusts URI {
  method default_port() { 443 }
}

1;
