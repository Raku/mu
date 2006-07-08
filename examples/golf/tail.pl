my$h=open@*ARGS[0];my@l=$h.readline();
my$i;for(@l){++$i}
$i=$i-10;$i=0 if$i<0;
print@l[$i..Inf]
