#=Klassen-Variablen und Konstruktor
class Mensch {
	has $.geburtstag;
}

role Leben {
	...
}

class Person {
	is Mensch;
	does Leben;
	
	has $.name is rw;
}
