my$h=open@ARGS[0];my@l=$h.readline();
my$i;for(@l){++$i}
print@l[int(($i-1)/2)..int($i/2)]
