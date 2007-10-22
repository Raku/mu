say '1..1';
my %a = { a => 1 ,};
for %a.keys -> $key {
    say 'ok ' ~ %a{ $key };
}
