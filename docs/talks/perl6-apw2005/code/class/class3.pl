#=Methoden mit mehreren Parametern definieren
class Welt {
	method hallo ( String $gruß, String $name ) { "$gruß, $name!".say; }
}
#=Aufruf
$welt.hallo("Hallo", "Mike");
$welt.hallo("Tach", "Otto");
