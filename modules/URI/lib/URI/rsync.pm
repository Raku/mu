use v6;

class URI::rsync is URI::_server is URI::_userpass {
  # http://rsync.samba.org/

  # rsync://[USER@]HOST[:PORT]/SRC

  method default_port() { 873 }
}

1;
