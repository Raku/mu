use v6;

class URI::_login isa URI::_server isa URI::_userpass trusts URI {
  # Generic terminal logins.  This is used as a base class for 'telnet',
  # 'tn3270', and 'rlogin' URL schemes.
}

1;
