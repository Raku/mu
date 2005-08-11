#=Methoden mit einem Parameter definieren
class Welt {
	method hallo ( $name ) { "Hallo, $name!".say; }
}
#=Methoden mit einem Argument aufrufen
$welt.hallo "Mike";
hallo $welt: "Mike";
hallo($welt: "Mike");
