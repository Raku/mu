my %mates = (
  'jane'   => 'joe',
  'sally'  => 'bob',
  'janice' => 'bill'
 );

for (%mates.kv) -> $girl, $guy { 
  "$girl is dating $guy".say
}
