use v6;

class URI::snews isa URI::news trusts URI {
  # draft-gilman-news-url-01
  method default_port() { 563 }
}

1;
