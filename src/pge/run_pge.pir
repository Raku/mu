.sub _main
    .local int spi, spc, utf8, pos, size1, size2
    .local pmc args, match, add_rule
    .local string name, rule, input, result, cmd, sizeStr, arg1, arg2
    .local pmc stdin

    load_bytecode "PGE/Hs.pir"
    match = find_global "PGE::Hs", "match"

  loop:
    getstdin stdin
    readline input, stdin
    length pos, input 
    if pos < 1 goto end
    pos = index input, " "
    if pos == 0 goto loop
    cmd = substr input, 0, pos
    inc pos
    input = substr input, pos
    pos = index input, " "
    if pos == 0 goto loop
    sizeStr = substr input, 0, pos
    size1 = sizeStr
    inc pos
    sizeStr = substr input, pos
    size2 = sizeStr

    # Now read up arg1 and arg2
    inc size1
    arg1 = read stdin, size1
    chopn arg1, 1 # skip \n
    inc size2
    arg2 = read stdin, size2
    chopn arg2, 1 # skip \n

    if cmd == "add_rule" goto do_add_rule
    if cmd == "match" goto do_match
    goto loop
 
  do_add_rule:
    add_rule(arg1, arg2)
    goto loop

  do_match:
    result = match(arg1, arg2)
    length pos, result 
    print "OK "
    print pos
    print "\n"
    print result
    print "\n"
    goto loop

  end:
.end
