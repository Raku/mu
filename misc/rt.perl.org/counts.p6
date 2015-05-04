my %counts;
for 'rt.txt'.IO.lines -> $line {
    next if $line ~~ /^'----'/;
    next if $line ~~ /^'#'/;
    next if $line ~~ /^\s*$/;
    if $line ~~ /'# OK'/ {
        %counts<OK>++;
    } elsif $line ~~ /'# MERGED'/ {
        %counts<MERGED>++;
    } elsif $line ~~ /'# REJECTED'/ {
        %counts<REJECTED>++;
    } elsif $line ~~ /'# RESOLVED'/ {
        %counts<RESOLVED>++;
    } else {
        %counts<NEEDREVIEW>++;
    }
}

for %counts.keys.sort -> $key {
    say sprintf "%-12s: %s", $key, %counts{$key};
}
