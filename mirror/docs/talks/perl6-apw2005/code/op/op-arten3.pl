#=term Operatoren (nullär)
# der "und so weiter und so fort"-Operator:
$x = {...}	# "yada, yada, yada"
$x = {term:<...>()}

# PI könnte ein term Operator sein:
say PI;
say term:<PI>()

#=tertiärer Operator
$wahr ?? $a :: $b	# in Perl 5 war das $wahr ? $a : $b
$wahr.tertiary:<?? ::>($a, $b) # keine Ahnung ob richtig!
