say '1..1';
my %a = { a => 1 };
my $key; ### TODO: compiler bug...
for %a.keys -> $key {
    say 'ok ' ~ %a{ $key };
}
