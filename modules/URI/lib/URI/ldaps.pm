use v6;

class URI::ldaps isa URI::ldap trusts URI {
  method default_port() { 636 }
}

1;
