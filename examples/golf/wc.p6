my$h=open@*ARGS[0];my@l=$h.readline();
my$i;for(@l){++$i}
say join"",(split"",int(7e10+$i))[1..Inf];
