.sub _main
    .local int argc, pos, size1, size2
    .local pmc args, match, add_rule
    .local string input, result, cmd, sizeStr, arg1, arg2
    .local pmc stdin, stdout
    .include "iglobals.pasm"

    load_bytecode "PGE/Hs.pir"
    match = find_global "PGE::Hs", "match"
    add_rule = find_global "PGE::Hs", "add_rule"

    getinterp args
    set args, args[.IGLOBALS_ARGV_LIST]
    argc = elements args
    if argc == 3 goto do_args

    getstdin stdin
    getstdout stdout
    stdin."setbuf"(0)
    stdout."setbuf"(0)

  loop:
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

    arg1 = unescape(arg1)
    arg2 = unescape(arg2)

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

  do_args:
    arg1 = args[1]
    arg2 = args[2]
    arg1 = unescape(arg1)
    arg2 = unescape(arg2)
    result = match(arg1, arg2)
    print result

  end:
.end

.sub unescape
    .param string str
    .local string ret, tmp
    .local int i, j

    ret = ""
    j = length str
    if j == 0 goto END
    i = 0

LOOP:
    tmp = str[i]
    inc i
    if i >= j goto FIN

    eq tmp, "\\", ESC
    concat ret, tmp
    goto LOOP

ESC:
    tmp = str[i]
    inc i
    eq tmp, "n", LF
    concat ret, tmp
    goto UNESC
LF:
    concat ret, "\n"
UNESC:
    inc i
    if i >= j goto END
    goto LOOP

FIN:
    concat ret, tmp
END:
    .return(ret)
.end
