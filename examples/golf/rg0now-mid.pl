my @l= lines();
print @l[int((@l-1)/2)..int(@l/2)]
