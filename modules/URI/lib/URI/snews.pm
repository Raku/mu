use v6;

class URI::snews is URI::news {
  # draft-gilman-news-url-01
  method default_port() { 563 }
}

1;
