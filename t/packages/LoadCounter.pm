package t::packages::LoadCounter;

$main::loaded++;

sub import {
    $main::imported++;
}

sub unimport {
    $main::imported--;
}
