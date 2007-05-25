use v6;

use Log::Entry;

for =<> -> my $line {
    my $e = Log::Entry.new(line => $line);
    $e.parse();
}
