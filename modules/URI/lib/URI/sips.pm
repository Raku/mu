use v6;

class URI::sips isa URI::sip trusts URI {
  method default_port() { 5061 }
}

1;
