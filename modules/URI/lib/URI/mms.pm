use v6;

class URI::mms is URI::http {
  method default_port() { 1755 }
}

1;
