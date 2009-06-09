my$h=open @*ARGS[0];my@l=$h.get;
my$i;++$i for @l;
say join "",(split "",int(7e10+$i))[1..Inf];
