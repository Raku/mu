#=given..when - "Switch-Anweisung"
given $monster {
	when Monster::Schläft { lauf_weg();  }
	when 'hungrig'   { werfe('steak'); }
	when .füttert     { schleiche_vorbei();  }
	when /grrr+/      { ohren_zuhalten();  }
	
	print "Schritt für Schritt...";
	when 2            { lauf_dazwischen(); continue; }
	when (3..10)      { lauf_weg();    }
}
