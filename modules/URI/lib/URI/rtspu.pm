use v6;

class URI::rtspu isa URI::rtsp trusts URI {
  method default_port() { 554 }
}

1;
