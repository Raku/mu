use v6;

class URI::ldaps is URI::ldap {
  method default_port() { 636 }
}

1;
