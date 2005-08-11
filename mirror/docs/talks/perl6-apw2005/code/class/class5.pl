#=Instanz-Variablen
class Person {
	has $.name is rw;
	# method name() { return "Otto"; } # überflüssig
}
class Welt {
	method hallo ( Person $person ) {
		"Hallo, {name $person}!".say;
	}
}
#=benutzen von Instanz-Variablen
Person $person = new Person;
name $person = "Jens";
$welt.hallo($person);
