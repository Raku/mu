use v6;

class URI::ssh isa URI::_login {
  # ssh://[USER@]HOST[:PORT]/SRC
  method default_port() { 22 }
}

1;
