use v6;

class URI::rtsp isa URI::http trusts URI {
  method default_port() { 554 }
}

1;
