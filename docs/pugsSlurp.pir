.namespace [ "Pugs::Internals" ]

.sub pugsSlurp
    .local pmc str
    .local pmc words
    .local pmc splitf
    .local pmc splitd
    .local pmc space
    .local pmc part
    .local pmc fh
    .local pmc return
    .local string spart
    .local string cmd
    .local string buffer
    .local string temp
    .local int i
    .local int len
    get_params "(0)", str
    cmd = "pugs -Cparrot "

    space = new PerlString
    space = " "
    splitf = find_global "main", "&split"
    splitd = splitf(space, str)
    
    len = splitd
    i = 0
again:
    unless i < len goto endl
    part = splitd[i]
    spart = part
    spart = "-e " . spart
    cmd .= spart
    goto again
endl:
    
    fh = open cmd, "-|"
    $S0 = pop fh   # pop buf layer
lp:
    temp = read fh, 255
    buffer .= temp
    if fh goto lp

    return = new PerlString
    return = buffer
    set_returns "(0)", return
    returncc
.end

