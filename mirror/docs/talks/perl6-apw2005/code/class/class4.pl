#=polymorphe Welt
class Person {
	method name() { return "Otto"; }
}
class Welt {
	method hallo ( String $name ) { "Hallo, $name!".say; }
	method hallo ( Person $person ) {
		"Hallo, {name $person}!".say;
	}
}
#=aufrufen polymorpher Methoden
Person $person = new Person;
$welt.hallo($person);
$welt.hallo("Mike");
