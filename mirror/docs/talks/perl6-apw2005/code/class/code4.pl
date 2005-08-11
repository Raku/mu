#=Erbsenzähler? Nee nur Kartoffeln!
for 5..7 -> $count {
	my $potato = "$count potato, ";
	NEXT {
		print $potato;
	}
	LAST {
		print $potato, "more.";
	}
}
