.sub _main
    .local int spi, spc
    .local pmc args, match, add_rule
    .include "iglobals.pasm"
    load_bytecode "PGE-Hs.pbc"

    getinterp args
    set args, args[.IGLOBALS_ARGV_LIST]
    spi = 3
    spc = elements args
    add_rule = find_global "PGE::Hs", "add_rule"

  subrules:
    unless spi < spc goto do_match
    $S1 = args[spi]
    inc spi
    $S2 = args[spi]
    inc spi
    add_rule($S1, $S2)
    goto subrules
  do_match:
    match = find_global "PGE::Hs", "match"
    $S1 = args[1]
    $S2 = args[2]
    $S0 = match($S1, $S2)
    print $S0
.end
