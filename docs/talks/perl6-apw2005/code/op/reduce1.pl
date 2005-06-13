#=einfache reduzierungs-Operatoren
sub prefix:<Σ> (@x) { [+] *@x }   # Summe
sub postfix:<!> ($x) { [*] 1..$x } # Produkt
say Σ [1..5!] # 7260 
#=auswählen des ersten definierten Wertes:
my $opt = [//] %para{'x'}, %conf{'y'}, $global, 'default';
