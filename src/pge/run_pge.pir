.sub _main
    .local int spi, spc, utf8
    .local pmc args, match, add_rule
    .local string name, rule, input, result
    .include "iglobals.pasm"

    load_bytecode "PGE/Hs.pir"
    utf8        = charset "utf8"
    add_rule    = find_global "PGE::Hs", "add_rule"

    getinterp args
    set args, args[.IGLOBALS_ARGV_LIST]
    spi = 3
    spc = elements args

  subrules:
    unless spi < spc goto do_match
    name    = args[spi]
    inc spi
    rule    = args[spi]
    inc spi
    trans_charset name, utf8
    trans_charset rule, utf8
    add_rule(name, rule)
    goto subrules
  do_match:
    match   = find_global "PGE::Hs", "match"
    input   = args[1]
    rule    = args[2]
    trans_charset input, utf8
    trans_charset rule, utf8
    result  = match(input, rule)
    print result
.end
