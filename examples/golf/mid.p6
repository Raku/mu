my@l=@*ARGS[0].open.readline;
my$i;map{++$i}@l;
print@l[int(($i-1)/2)..int($i/2)]
