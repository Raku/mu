use v6;

class URI::rsync isa URI::_server isa URI::_userpass trusts URI {
  # http://rsync.samba.org/

  # rsync://[USER@]HOST[:PORT]/SRC

  method default_port() { 873 }
}

1;
