use v6-alpha;

package t::packages::LoadCounter;

$Main::loaded++;

sub import {
    $Main::imported++;
}

sub unimport {
    $Main::imported--;
}
