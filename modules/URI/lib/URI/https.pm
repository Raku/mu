use v6;

class URI::https is URI::http {
  method default_port() { 443 }
}

1;
