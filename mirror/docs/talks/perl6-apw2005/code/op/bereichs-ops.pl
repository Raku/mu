#=Bereichs-Operatoren
my @a = 1..5;  # '12345'
say 0..@a;     # '012345'

say 0..@a-1;   # '01234'
say 0..^@a;    # '01234'

say 0+1..@a;   # '12345'
say 0^..@a;    # '12345'

say 0^..^@a;   # '1234'

my @c = 1...;  # (1..Inf)
