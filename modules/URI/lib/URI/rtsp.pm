use v6;

class URI::rtsp is URI::http {
  method default_port() { 554 }
}

1;
