#=Variablen in Perl 6 im Vergleich zu Perl 5
# Skalar:
say "$test";      # print "$test";

# Array:
say "@test[1]";   # print "$test[1]";

# Hash:
say "%test{a}";   # print "$test{a}";

# Array Referenz:
say "$test[1]";   # print "$test->[1]";

# Hash Referenz:
say "$test{a}";   # print "$test->{a}";
