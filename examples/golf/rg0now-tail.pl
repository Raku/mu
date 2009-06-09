my @l=lines;
print @l[(@l>9??@l-10!!0)..*]
