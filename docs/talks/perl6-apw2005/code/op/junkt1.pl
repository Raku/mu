#=Alle alt genug für...?
my @alter = @anwesende».alter();

"Na zum Glück ist das hier jugendfrei ;-)".say if any(@alter) < 18;
