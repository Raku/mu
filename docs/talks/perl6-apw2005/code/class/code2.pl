#="Einpacken" von Code-Objekten
$id = &subname.wrap (sub (*@args) {
	# Argumente vorbearbeiten
	$result = call('neue', 'Argumente');
	# Nachbearbeitung
	return $result;
})
#="Auspacken"
$id.unwrap;
