.namespace ['Main']
.sub "&chomp"
    .local pmc s__str
    .lex "$str", s__str
    store_lex "$str", s__str
    get_params "(0)", s__str
    store_lex "$str", s__str
    .local pmc p1_block
    .const .Sub prelude_LABEL_0_block_C = "prelude_LABEL_0_block"
    p1_block = newclosure prelude_LABEL_0_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p1_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_0_block" :anon  :outer("&chomp")
    get_params "()"
    .local pmc p2_fun
    p2_fun = new .Undef
    p2_fun = find_name "&statement_control:cond"
    .local pmc p3_fun
    p3_fun = new .Undef
    p3_fun = find_name "&infix:eq"
    .local pmc p4_fun
    p4_fun = new .Undef
    p4_fun = find_name "&substr"
    .local pmc p5_lex
    p5_lex = new .Undef
    p5_lex = find_lex "$str"
    .local pmc p6_fun
    p6_fun = new .Undef
    p6_fun = find_name "&prefix:-"
    .local pmc p7_lit
    p7_lit = new .Undef
    p7_lit = assign 1
    .local pmc p8_app
    p8_app = new .Undef
    set_args '(64)', p7_lit
    get_results "(0)", p8_app
    invokecc p6_fun
    .local pmc p9_lit
    p9_lit = new .Undef
    p9_lit = assign 1
    .local pmc p10_app
    p10_app = new .Undef
    set_args '(64, 64, 64)', p5_lex, p8_app, p9_lit
    get_results "(0)", p10_app
    invokecc p4_fun
    .local pmc p11_lit
    p11_lit = new .Undef
    p11_lit = assign "\n"
    .local pmc p12_app
    p12_app = new .Undef
    set_args '(64, 64)', p10_app, p11_lit
    get_results "(0)", p12_app
    invokecc p3_fun
    .local pmc p13_thunk
    .const .Sub prelude_LABEL_1_thunk_C = "prelude_LABEL_1_thunk"
    p13_thunk = newclosure prelude_LABEL_1_thunk_C
    .local pmc p25_thunk
    .const .Sub prelude_LABEL_3_thunk_C = "prelude_LABEL_3_thunk"
    p25_thunk = newclosure prelude_LABEL_3_thunk_C
    .local pmc p28_app
    p28_app = new .Undef
    set_args '(64, 64, 64)', p12_app, p13_thunk, p25_thunk
    get_results "(0)", p28_app
    invokecc p2_fun
    set_returns "(64)", p28_app
    returncc
.end
.sub "prelude_LABEL_1_thunk" :anon  :outer("prelude_LABEL_0_block")
    .local pmc p14_block
    .const .Sub prelude_LABEL_2_block_C = "prelude_LABEL_2_block"
    p14_block = newclosure prelude_LABEL_2_block_C
    set_returns "(64)", p14_block
    returncc
.end
.sub "prelude_LABEL_2_block" :anon  :outer("prelude_LABEL_1_thunk")
    get_params "()"
    .local pmc p15_fun
    p15_fun = new .Undef
    p15_fun = find_name "&substr"
    .local pmc p16_lex
    p16_lex = new .Undef
    p16_lex = find_lex "$str"
    .local pmc p17_lit
    p17_lit = new .Undef
    p17_lit = assign 0
    .local pmc p18_fun
    p18_fun = new .Undef
    p18_fun = find_name "&infix:-"
    .local pmc p19_fun
    p19_fun = new .Undef
    p19_fun = find_name "&chars"
    .local pmc p20_lex
    p20_lex = new .Undef
    p20_lex = find_lex "$str"
    .local pmc p21_app
    p21_app = new .Undef
    set_args '(64)', p20_lex
    get_results "(0)", p21_app
    invokecc p19_fun
    .local pmc p22_lit
    p22_lit = new .Undef
    p22_lit = assign 1
    .local pmc p23_app
    p23_app = new .Undef
    set_args '(64, 64)', p21_app, p22_lit
    get_results "(0)", p23_app
    invokecc p18_fun
    .local pmc p24_app
    p24_app = new .Undef
    set_args '(64, 64, 64)', p16_lex, p17_lit, p23_app
    get_results "(0)", p24_app
    invokecc p15_fun
    set_returns "(64)", p24_app
    returncc
.end
.sub "prelude_LABEL_3_thunk" :anon  :outer("prelude_LABEL_0_block")
    .local pmc p26_block
    .const .Sub prelude_LABEL_4_block_C = "prelude_LABEL_4_block"
    p26_block = newclosure prelude_LABEL_4_block_C
    set_returns "(64)", p26_block
    returncc
.end
.sub "prelude_LABEL_4_block" :anon  :outer("prelude_LABEL_3_thunk")
    get_params "()"
    .local pmc p27_lex
    p27_lex = new .Undef
    p27_lex = find_lex "$str"
    set_returns "(64)", p27_lex
    returncc
.end
.sub "&chop"
    .local pmc s__str
    .lex "$str", s__str
    store_lex "$str", s__str
    get_params "(0)", s__str
    store_lex "$str", s__str
    .local pmc p29_block
    .const .Sub prelude_LABEL_5_block_C = "prelude_LABEL_5_block"
    p29_block = newclosure prelude_LABEL_5_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p29_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_5_block" :anon  :outer("&chop")
    get_params "()"
    .local pmc p30_fun
    p30_fun = new .Undef
    p30_fun = find_name "&statement_control:cond"
    .local pmc p31_fun
    p31_fun = new .Undef
    p31_fun = find_name "&infix:=="
    .local pmc p32_fun
    p32_fun = new .Undef
    p32_fun = find_name "&chars"
    .local pmc p33_lex
    p33_lex = new .Undef
    p33_lex = find_lex "$str"
    .local pmc p34_app
    p34_app = new .Undef
    set_args '(64)', p33_lex
    get_results "(0)", p34_app
    invokecc p32_fun
    .local pmc p35_lit
    p35_lit = new .Undef
    p35_lit = assign 0
    .local pmc p36_app
    p36_app = new .Undef
    set_args '(64, 64)', p34_app, p35_lit
    get_results "(0)", p36_app
    invokecc p31_fun
    .local pmc p37_thunk
    .const .Sub prelude_LABEL_6_thunk_C = "prelude_LABEL_6_thunk"
    p37_thunk = newclosure prelude_LABEL_6_thunk_C
    .local pmc p39_thunk
    .const .Sub prelude_LABEL_8_thunk_C = "prelude_LABEL_8_thunk"
    p39_thunk = newclosure prelude_LABEL_8_thunk_C
    .local pmc p61_app
    p61_app = new .Undef
    set_args '(64, 64, 64)', p36_app, p37_thunk, p39_thunk
    get_results "(0)", p61_app
    invokecc p30_fun
    set_returns "(64)", p61_app
    returncc
.end
.sub "prelude_LABEL_6_thunk" :anon  :outer("prelude_LABEL_5_block")
    .local pmc p38_block
    .const .Sub prelude_LABEL_7_block_C = "prelude_LABEL_7_block"
    p38_block = newclosure prelude_LABEL_7_block_C
    set_returns "(64)", p38_block
    returncc
.end
.sub "prelude_LABEL_7_block" :anon  :outer("prelude_LABEL_6_thunk")
    get_params "()"
    set_returns "(64)", p38_block
    returncc
.end
.sub "prelude_LABEL_8_thunk" :anon  :outer("prelude_LABEL_5_block")
    .local pmc p40_block
    .const .Sub prelude_LABEL_9_block_C = "prelude_LABEL_9_block"
    p40_block = newclosure prelude_LABEL_9_block_C
    set_returns "(64)", p40_block
    returncc
.end
.sub "prelude_LABEL_9_block" :anon  :outer("prelude_LABEL_8_thunk")
    get_params "()"
    print "IN CHOP\n"
    .local pmc p41_lex_decl
    p41_lex_decl = new .Undef
    .lex "$removed" ,  p41_lex_decl
    .local pmc p42_fun
    p42_fun = new .Undef
    p42_fun = find_name "&substr"
    .local pmc p43_lex
    p43_lex = new .Undef
    p43_lex = find_lex "$str"
    print "p43_lex: "
    print p43_lex
    print "\n"
    .local pmc p44_fun
    p44_fun = new .Undef
    p44_fun = find_name "&prefix:-"
    .local pmc p45_lit
    p45_lit = new .Undef
    p45_lit = assign 1
    .local pmc p46_app
    p46_app = new .Undef
    set_args '(64)', p45_lit
    get_results "(0)", p46_app
    invokecc p44_fun
    .local pmc p47_lit
    p47_lit = new .Undef
    p47_lit = assign 1
    .local pmc p48_app
    p48_app = new .Undef
    set_args '(64, 64, 64)', p43_lex, p46_app, p47_lit
    get_results "(0)", p48_app
    invokecc p42_fun
    store_lex "$removed", p48_app
    .local pmc p49_fun
    p49_fun = new .Undef
    p49_fun = find_name "&substr"
    .local pmc p50_lex
    p50_lex = new .Undef
    p50_lex = find_lex "$str"
    .local pmc p51_lit
    p51_lit = new .Undef
    p51_lit = assign 0
    .local pmc p52_fun
    p52_fun = new .Undef
    p52_fun = find_name "&infix:-"
    .local pmc p53_fun
    p53_fun = new .Undef
    p53_fun = find_name "&chars"
    .local pmc p54_lex
    p54_lex = new .Undef
    p54_lex = find_lex "$str"
    .local pmc p55_app
    p55_app = new .Undef
    set_args '(64)', p54_lex
    get_results "(0)", p55_app
    invokecc p53_fun
    .local pmc p56_lit
    p56_lit = new .Undef
    p56_lit = assign 1
    .local pmc p57_app
    p57_app = new .Undef
    set_args '(64, 64)', p55_app, p56_lit
    get_results "(0)", p57_app
    invokecc p52_fun
    .local pmc p58_app
    p58_app = new .Undef
    set_args '(64, 64, 64)', p50_lex, p51_lit, p57_app
    get_results "(0)", p58_app
    invokecc p49_fun
    store_lex "$str", p58_app
    print "p58_app: "
    print p58_app
    print "\n"
    .local pmc p59_lex
    p59_lex = new .Undef
    p59_lex = find_lex "$str"
    print "p59_lex: "
    print p59_lex
    print "\n"
    .local pmc p60_lex
    p60_lex = new .Undef
    p60_lex = find_lex "$removed"
    set_returns "(64)", p60_lex
    returncc
.end
.sub "&exit"
    .local pmc s__status
    .lex "$status", s__status
    store_lex "$status", s__status
    get_params "(128)", s__status
    unless_null s__status, prelude_LABEL_10_defaultDone
    .local pmc p62_lit
    p62_lit = new .Undef
    p62_lit = assign 0
    s__status = set p62_lit
  prelude_LABEL_10_defaultDone:
    store_lex "$status", s__status
    .local pmc p63_block
    .const .Sub prelude_LABEL_11_block_C = "prelude_LABEL_11_block"
    p63_block = newclosure prelude_LABEL_11_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p63_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_11_block" :anon  :outer("&exit")
    get_params "()"
    .local pmc p64_fun
    p64_fun = new .Undef
    p64_fun = find_name "&Perl6::Internals::exit"
    .local pmc p65_lex
    p65_lex = new .Undef
    p65_lex = find_lex "$status"
    .local pmc p66_app
    p66_app = new .Undef
    set_args '(64)', p65_lex
    get_results "(0)", p66_app
    invokecc p64_fun
    set_returns "(64)", p66_app
    returncc
.end
.sub "&lcfirst"
    .local pmc s__str
    .lex "$str", s__str
    store_lex "$str", s__str
    get_params "(0)", s__str
    store_lex "$str", s__str
    .local pmc p67_block
    .const .Sub prelude_LABEL_12_block_C = "prelude_LABEL_12_block"
    p67_block = newclosure prelude_LABEL_12_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p67_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_12_block" :anon  :outer("&lcfirst")
    get_params "()"
    .local pmc p68_fun
    p68_fun = new .Undef
    p68_fun = find_name "&infix:~"
    .local pmc p69_fun
    p69_fun = new .Undef
    p69_fun = find_name "&lc"
    .local pmc p70_fun
    p70_fun = new .Undef
    p70_fun = find_name "&substr"
    .local pmc p71_lex
    p71_lex = new .Undef
    p71_lex = find_lex "$str"
    .local pmc p72_lit
    p72_lit = new .Undef
    p72_lit = assign 0
    .local pmc p73_lit
    p73_lit = new .Undef
    p73_lit = assign 1
    .local pmc p74_app
    p74_app = new .Undef
    set_args '(64, 64, 64)', p71_lex, p72_lit, p73_lit
    get_results "(0)", p74_app
    invokecc p70_fun
    .local pmc p75_app
    p75_app = new .Undef
    set_args '(64)', p74_app
    get_results "(0)", p75_app
    invokecc p69_fun
    .local pmc p76_fun
    p76_fun = new .Undef
    p76_fun = find_name "&substr"
    .local pmc p77_lex
    p77_lex = new .Undef
    p77_lex = find_lex "$str"
    .local pmc p78_lit
    p78_lit = new .Undef
    p78_lit = assign 1
    .local pmc p79_fun
    p79_fun = new .Undef
    p79_fun = find_name "&infix:-"
    .local pmc p80_fun
    p80_fun = new .Undef
    p80_fun = find_name "&chars"
    .local pmc p81_lex
    p81_lex = new .Undef
    p81_lex = find_lex "$str"
    .local pmc p82_app
    p82_app = new .Undef
    set_args '(64)', p81_lex
    get_results "(0)", p82_app
    invokecc p80_fun
    .local pmc p83_lit
    p83_lit = new .Undef
    p83_lit = assign 1
    .local pmc p84_app
    p84_app = new .Undef
    set_args '(64, 64)', p82_app, p83_lit
    get_results "(0)", p84_app
    invokecc p79_fun
    .local pmc p85_app
    p85_app = new .Undef
    set_args '(64, 64, 64)', p77_lex, p78_lit, p84_app
    get_results "(0)", p85_app
    invokecc p76_fun
    .local pmc p86_app
    p86_app = new .Undef
    set_args '(64, 64)', p75_app, p85_app
    get_results "(0)", p86_app
    invokecc p68_fun
    set_returns "(64)", p86_app
    returncc
.end
.sub "&pi"
    get_params "()"
    .local pmc p87_block
    .const .Sub prelude_LABEL_13_block_C = "prelude_LABEL_13_block"
    p87_block = newclosure prelude_LABEL_13_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p87_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_13_block" :anon  :outer("&pi")
    get_params "()"
    .local pmc p88_lit
    p88_lit = new .Undef
    p88_lit = assign 3.141592653589793
    set_returns "(64)", p88_lit
    returncc
.end
.sub "&shift"
    .local pmc a__a
    .lex "@a", a__a
    store_lex "@a", a__a
    get_params "(0)", a__a
    store_lex "@a", a__a
    .local pmc p89_block
    .const .Sub prelude_LABEL_14_block_C = "prelude_LABEL_14_block"
    p89_block = newclosure prelude_LABEL_14_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p89_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_14_block" :anon  :outer("&shift")
    get_params "()"
    .local pmc p90_lex_decl
    p90_lex_decl = new .Undef
    .lex "$top" ,  p90_lex_decl
    .local pmc p91_fun
    p91_fun = new .Undef
    p91_fun = find_name "&infix:-"
    .local pmc p92_fun
    p92_fun = new .Undef
    p92_fun = find_name "&prefix:+"
    .local pmc p93_lex
    p93_lex = new .Undef
    p93_lex = find_lex "@a"
    .local pmc p94_app
    p94_app = new .Undef
    set_args '(64)', p93_lex
    get_results "(0)", p94_app
    invokecc p92_fun
    .local pmc p95_lit
    p95_lit = new .Undef
    p95_lit = assign 1
    .local pmc p96_app
    p96_app = new .Undef
    set_args '(64, 64)', p94_app, p95_lit
    get_results "(0)", p96_app
    invokecc p91_fun
    store_lex "$top", p96_app
    .local pmc p97_fun
    p97_fun = new .Undef
    p97_fun = find_name "&statement_control:cond"
    .local pmc p98_fun
    p98_fun = new .Undef
    p98_fun = find_name "&infix:<"
    .local pmc p99_lex
    p99_lex = new .Undef
    p99_lex = find_lex "$top"
    .local pmc p100_lit
    p100_lit = new .Undef
    p100_lit = assign 0
    .local pmc p101_app
    p101_app = new .Undef
    set_args '(64, 64)', p99_lex, p100_lit
    get_results "(0)", p101_app
    invokecc p98_fun
    .local pmc p102_thunk
    .const .Sub prelude_LABEL_15_thunk_C = "prelude_LABEL_15_thunk"
    p102_thunk = newclosure prelude_LABEL_15_thunk_C
    .local pmc p107_thunk
    .const .Sub prelude_LABEL_17_thunk_C = "prelude_LABEL_17_thunk"
    p107_thunk = newclosure prelude_LABEL_17_thunk_C
    .local pmc p109_app
    p109_app = new .Undef
    set_args '(64, 64, 64)', p101_app, p102_thunk, p107_thunk
    get_results "(0)", p109_app
    invokecc p97_fun
    .local pmc p110_lex_decl
    p110_lex_decl = new .Undef
    .lex "$e" ,  p110_lex_decl
    .local pmc p111_fun
    p111_fun = new .Undef
    p111_fun = find_name "&postcircumfix:[]"
    .local pmc p112_lex
    p112_lex = new .Undef
    p112_lex = find_lex "@a"
    .local pmc p113_lit
    p113_lit = new .Undef
    p113_lit = assign 0
    .local pmc p114_app
    p114_app = new .Undef
    set_args '(64, 64)', p112_lex, p113_lit
    get_results "(0)", p114_app
    invokecc p111_fun
    store_lex "$e", p114_app
    .local pmc p115_lex_decl
    p115_lex_decl = new .Undef
    .lex "$i" ,  p115_lex_decl
    .local pmc p116_lit
    p116_lit = new .Undef
    p116_lit = assign 0
    store_lex "$i", p116_lit
    .local pmc p117_fun
    p117_fun = new .Undef
    p117_fun = find_name "&statement_control:while"
    .local pmc p118_block
    .const .Sub prelude_LABEL_19_block_C = "prelude_LABEL_19_block"
    p118_block = newclosure prelude_LABEL_19_block_C
    .local pmc p123_block
    .const .Sub prelude_LABEL_20_block_C = "prelude_LABEL_20_block"
    p123_block = newclosure prelude_LABEL_20_block_C
    .local pmc p134_app
    p134_app = new .Undef
    set_args '(64, 64)', p118_block, p123_block
    get_results "(0)", p134_app
    invokecc p117_fun
    .local pmc p135_fun
    p135_fun = new .Undef
    p135_fun = find_name "&pop"
    .local pmc p136_lex
    p136_lex = new .Undef
    p136_lex = find_lex "@a"
    .local pmc p137_app
    p137_app = new .Undef
    set_args '(64)', p136_lex
    get_results "(0)", p137_app
    invokecc p135_fun
    .local pmc p138_fun
    p138_fun = new .Undef
    p138_fun = find_name "&return"
    .local pmc p139_lex
    p139_lex = new .Undef
    p139_lex = find_lex "$e"
    .local pmc p140_app
    p140_app = new .Undef
    set_args '(64)', p139_lex
    get_results "(0)", p140_app
    invokecc p138_fun
    set_returns "(64)", p140_app
    returncc
.end
.sub "prelude_LABEL_15_thunk" :anon  :outer("prelude_LABEL_14_block")
    .local pmc p103_block
    .const .Sub prelude_LABEL_16_block_C = "prelude_LABEL_16_block"
    p103_block = newclosure prelude_LABEL_16_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p103_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_16_block" :anon  :outer("prelude_LABEL_15_thunk")
    get_params "()"
    .local pmc p104_fun
    p104_fun = new .Undef
    p104_fun = find_name "&return"
    .local pmc p105_undef
    p105_undef = new .Undef
    .local pmc p106_app
    p106_app = new .Undef
    set_args '(64)', p105_undef
    get_results "(0)", p106_app
    invokecc p104_fun
    set_returns "(64)", p106_app
    returncc
.end
.sub "prelude_LABEL_17_thunk" :anon  :outer("prelude_LABEL_14_block")
    .local pmc p108_block
    .const .Sub prelude_LABEL_18_block_C = "prelude_LABEL_18_block"
    p108_block = newclosure prelude_LABEL_18_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p108_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_18_block" :anon  :outer("prelude_LABEL_17_thunk")
    get_params "()"
    set_returns "(64)", $P0
    returncc
.end
.sub "prelude_LABEL_19_block" :anon  :outer("prelude_LABEL_14_block")
    get_params "()"
    .local pmc p119_fun
    p119_fun = new .Undef
    p119_fun = find_name "&infix:<"
    .local pmc p120_lex
    p120_lex = new .Undef
    p120_lex = find_lex "$i"
    .local pmc p121_lex
    p121_lex = new .Undef
    p121_lex = find_lex "$top"
    .local pmc p122_app
    p122_app = new .Undef
    set_args '(64, 64)', p120_lex, p121_lex
    get_results "(0)", p122_app
    invokecc p119_fun
    set_returns "(64)", p122_app
    returncc
.end
.sub "prelude_LABEL_20_block" :anon  :outer("prelude_LABEL_14_block")
    get_params "()"
    .local pmc p124_fun
    p124_fun = new .Undef
    p124_fun = find_name "&postcircumfix:[]"
    .local pmc p125_lex
    p125_lex = new .Undef
    p125_lex = find_lex "@a"
    .local pmc p126_fun
    p126_fun = new .Undef
    p126_fun = find_name "&postfix:++"
    .local pmc p127_lex
    p127_lex = new .Undef
    p127_lex = find_lex "$i"
    .local pmc p128_app
    p128_app = new .Undef
    set_args '(64)', p127_lex
    get_results "(0)", p128_app
    invokecc p126_fun
    .local pmc p129_app
    p129_app = new .Undef
    set_args '(64, 64)', p125_lex, p128_app
    get_results "(0)", p129_app
    invokecc p124_fun
    .local pmc p130_fun
    p130_fun = new .Undef
    p130_fun = find_name "&postcircumfix:[]"
    .local pmc p131_lex
    p131_lex = new .Undef
    p131_lex = find_lex "@a"
    .local pmc p132_lex
    p132_lex = new .Undef
    p132_lex = find_lex "$i"
    .local pmc p133_app
    p133_app = new .Undef
    set_args '(64, 64)', p131_lex, p132_lex
    get_results "(0)", p133_app
    invokecc p130_fun
    p129_app = assign p133_app
    set_returns "(64)", p133_app
    returncc
.end
.sub "&sleep"
    .local pmc s__seconds
    .lex "$seconds", s__seconds
    store_lex "$seconds", s__seconds
    get_params "(0)", s__seconds
    store_lex "$seconds", s__seconds
    .local pmc p141_block
    .const .Sub prelude_LABEL_21_block_C = "prelude_LABEL_21_block"
    p141_block = newclosure prelude_LABEL_21_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p141_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_21_block" :anon  :outer("&sleep")
    get_params "()"
    .local pmc p142_lex_decl
    p142_lex_decl = new .Undef
    .lex "$time" ,  p142_lex_decl
    .local pmc p143_fun
    p143_fun = new .Undef
    p143_fun = find_name "&time"
    .local pmc p144_app
    p144_app = new .Undef
    set_args '()'
    get_results "(0)", p144_app
    invokecc p143_fun
    store_lex "$time", p144_app
    .local pmc p145_fun
    p145_fun = new .Undef
    p145_fun = find_name "&Perl6::Internals::sleep"
    .local pmc p146_lex
    p146_lex = new .Undef
    p146_lex = find_lex "$seconds"
    .local pmc p147_app
    p147_app = new .Undef
    set_args '(64)', p146_lex
    get_results "(0)", p147_app
    invokecc p145_fun
    .local pmc p148_lex_decl
    p148_lex_decl = new .Undef
    .lex "$seconds_slept" ,  p148_lex_decl
    .local pmc p149_fun
    p149_fun = new .Undef
    p149_fun = find_name "&infix:-"
    .local pmc p150_fun
    p150_fun = new .Undef
    p150_fun = find_name "&time"
    .local pmc p151_app
    p151_app = new .Undef
    set_args '()'
    get_results "(0)", p151_app
    invokecc p150_fun
    .local pmc p152_lex
    p152_lex = new .Undef
    p152_lex = find_lex "$time"
    .local pmc p153_app
    p153_app = new .Undef
    set_args '(64, 64)', p151_app, p152_lex
    get_results "(0)", p153_app
    invokecc p149_fun
    store_lex "$seconds_slept", p153_app
    .local pmc p154_lex
    p154_lex = new .Undef
    p154_lex = find_lex "$seconds_slept"
    set_returns "(64)", p154_lex
    returncc
.end
.sub "&splice"
    .local pmc a__a
    .lex "@a", a__a
    .local pmc s__offset
    .lex "$offset", s__offset
    .local pmc s__length
    .lex "$length", s__length
    .local pmc a__list
    .lex "@list", a__list
    store_lex "@a", a__a
    store_lex "$offset", s__offset
    store_lex "$length", s__length
    store_lex "@list", a__list
    get_params "(0, 128, 128, 32)", a__a, s__offset, s__length, a__list
    store_lex "@a", a__a
    unless_null s__offset, prelude_LABEL_22_defaultDone
    .local pmc p155_lit
    p155_lit = new .Undef
    p155_lit = assign 0
    s__offset = set p155_lit
  prelude_LABEL_22_defaultDone:
    store_lex "$offset", s__offset
    unless_null s__length, prelude_LABEL_23_defaultDone
    .local pmc p156_undef
    p156_undef = new .Undef
    s__length = set p156_undef
  prelude_LABEL_23_defaultDone:
    store_lex "$length", s__length
    store_lex "@list", a__list
    .local pmc p157_block
    .const .Sub prelude_LABEL_24_block_C = "prelude_LABEL_24_block"
    p157_block = newclosure prelude_LABEL_24_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p157_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_24_block" :anon  :outer("&splice")
    get_params "()"
    .local pmc p158_lex_decl
    p158_lex_decl = new .Undef
    .lex "$off" ,  p158_lex_decl
    .local pmc p159_fun
    p159_fun = new .Undef
    p159_fun = find_name "&prefix:+"
    .local pmc p160_lex
    p160_lex = new .Undef
    p160_lex = find_lex "$offset"
    .local pmc p161_app
    p161_app = new .Undef
    set_args '(64)', p160_lex
    get_results "(0)", p161_app
    invokecc p159_fun
    store_lex "$off", p161_app
    .local pmc p162_lex_decl
    p162_lex_decl = new .Undef
    .lex "$len" ,  p162_lex_decl
    .local pmc p163_lex_decl
    p163_lex_decl = new .Undef
    .lex "$length" ,  p163_lex_decl
    store_lex "$len", p163_lex_decl
    .local pmc p164_lex_decl
    p164_lex_decl = new .Undef
    .lex "$size" ,  p164_lex_decl
    .local pmc p165_fun
    p165_fun = new .Undef
    p165_fun = find_name "&prefix:+"
    .local pmc p166_lex
    p166_lex = new .Undef
    p166_lex = find_lex "@a"
    .local pmc p167_app
    p167_app = new .Undef
    set_args '(64)', p166_lex
    get_results "(0)", p167_app
    invokecc p165_fun
    store_lex "$size", p167_app
    .local pmc p168_fun
    p168_fun = new .Undef
    p168_fun = find_name "&statement_control:cond"
    .local pmc p169_fun
    p169_fun = new .Undef
    p169_fun = find_name "&infix:<"
    .local pmc p170_lex
    p170_lex = new .Undef
    p170_lex = find_lex "$off"
    .local pmc p171_lit
    p171_lit = new .Undef
    p171_lit = assign 0
    .local pmc p172_app
    p172_app = new .Undef
    set_args '(64, 64)', p170_lex, p171_lit
    get_results "(0)", p172_app
    invokecc p169_fun
    .local pmc p173_thunk
    .const .Sub prelude_LABEL_25_thunk_C = "prelude_LABEL_25_thunk"
    p173_thunk = newclosure prelude_LABEL_25_thunk_C
    .local pmc p180_thunk
    .const .Sub prelude_LABEL_27_thunk_C = "prelude_LABEL_27_thunk"
    p180_thunk = newclosure prelude_LABEL_27_thunk_C
    .local pmc p182_app
    p182_app = new .Undef
    set_args '(64, 64, 64)', p172_app, p173_thunk, p180_thunk
    get_results "(0)", p182_app
    invokecc p168_fun
    .local pmc p183_fun
    p183_fun = new .Undef
    p183_fun = find_name "&statement_control:cond"
    .local pmc p184_fun
    p184_fun = new .Undef
    p184_fun = find_name "&infix:>"
    .local pmc p185_lex
    p185_lex = new .Undef
    p185_lex = find_lex "$off"
    .local pmc p186_lex
    p186_lex = new .Undef
    p186_lex = find_lex "$size"
    .local pmc p187_app
    p187_app = new .Undef
    set_args '(64, 64)', p185_lex, p186_lex
    get_results "(0)", p187_app
    invokecc p184_fun
    .local pmc p188_thunk
    .const .Sub prelude_LABEL_29_thunk_C = "prelude_LABEL_29_thunk"
    p188_thunk = newclosure prelude_LABEL_29_thunk_C
    .local pmc p195_thunk
    .const .Sub prelude_LABEL_31_thunk_C = "prelude_LABEL_31_thunk"
    p195_thunk = newclosure prelude_LABEL_31_thunk_C
    .local pmc p197_app
    p197_app = new .Undef
    set_args '(64, 64, 64)', p187_app, p188_thunk, p195_thunk
    get_results "(0)", p197_app
    invokecc p183_fun
    .local pmc p198_fun
    p198_fun = new .Undef
    p198_fun = find_name "&statement_control:cond"
    .local pmc p199_fun
    p199_fun = new .Undef
    p199_fun = find_name "&defined"
    .local pmc p200_lex
    p200_lex = new .Undef
    p200_lex = find_lex "$len"
    .local pmc p201_app
    p201_app = new .Undef
    set_args '(64)', p200_lex
    get_results "(0)", p201_app
    invokecc p199_fun
    .local pmc p202_thunk
    .const .Sub prelude_LABEL_32_thunk_C = "prelude_LABEL_32_thunk"
    p202_thunk = newclosure prelude_LABEL_32_thunk_C
    .local pmc p208_thunk
    .const .Sub prelude_LABEL_34_thunk_C = "prelude_LABEL_34_thunk"
    p208_thunk = newclosure prelude_LABEL_34_thunk_C
    .local pmc p210_app
    p210_app = new .Undef
    set_args '(64, 64, 64)', p201_app, p202_thunk, p208_thunk
    get_results "(0)", p210_app
    invokecc p198_fun
    .local pmc p211_fun
    p211_fun = new .Undef
    p211_fun = find_name "&statement_control:cond"
    .local pmc p212_fun
    p212_fun = new .Undef
    p212_fun = find_name "&prefix:!"
    .local pmc p213_fun
    p213_fun = new .Undef
    p213_fun = find_name "&defined"
    .local pmc p214_lex
    p214_lex = new .Undef
    p214_lex = find_lex "$len"
    .local pmc p215_app
    p215_app = new .Undef
    set_args '(64)', p214_lex
    get_results "(0)", p215_app
    invokecc p213_fun
    .local pmc p216_app
    p216_app = new .Undef
    set_args '(64)', p215_app
    get_results "(0)", p216_app
    invokecc p212_fun
    .local pmc p217_thunk
    .const .Sub prelude_LABEL_36_thunk_C = "prelude_LABEL_36_thunk"
    p217_thunk = newclosure prelude_LABEL_36_thunk_C
    .local pmc p224_thunk
    .const .Sub prelude_LABEL_38_thunk_C = "prelude_LABEL_38_thunk"
    p224_thunk = newclosure prelude_LABEL_38_thunk_C
    .local pmc p226_app
    p226_app = new .Undef
    set_args '(64, 64, 64)', p216_app, p217_thunk, p224_thunk
    get_results "(0)", p226_app
    invokecc p211_fun
    .local pmc p227_fun
    p227_fun = new .Undef
    p227_fun = find_name "&statement_control:cond"
    .local pmc p228_fun
    p228_fun = new .Undef
    p228_fun = find_name "&infix:<"
    .local pmc p229_lex
    p229_lex = new .Undef
    p229_lex = find_lex "$len"
    .local pmc p230_lit
    p230_lit = new .Undef
    p230_lit = assign 0
    .local pmc p231_app
    p231_app = new .Undef
    set_args '(64, 64)', p229_lex, p230_lit
    get_results "(0)", p231_app
    invokecc p228_fun
    .local pmc p232_thunk
    .const .Sub prelude_LABEL_40_thunk_C = "prelude_LABEL_40_thunk"
    p232_thunk = newclosure prelude_LABEL_40_thunk_C
    .local pmc p242_thunk
    .const .Sub prelude_LABEL_42_thunk_C = "prelude_LABEL_42_thunk"
    p242_thunk = newclosure prelude_LABEL_42_thunk_C
    .local pmc p244_app
    p244_app = new .Undef
    set_args '(64, 64, 64)', p231_app, p232_thunk, p242_thunk
    get_results "(0)", p244_app
    invokecc p227_fun
    .local pmc p245_fun
    p245_fun = new .Undef
    p245_fun = find_name "&statement_control:cond"
    .local pmc p246_fun
    p246_fun = new .Undef
    p246_fun = find_name "&infix:<"
    .local pmc p247_lex
    p247_lex = new .Undef
    p247_lex = find_lex "$len"
    .local pmc p248_lit
    p248_lit = new .Undef
    p248_lit = assign 0
    .local pmc p249_app
    p249_app = new .Undef
    set_args '(64, 64)', p247_lex, p248_lit
    get_results "(0)", p249_app
    invokecc p246_fun
    .local pmc p250_thunk
    .const .Sub prelude_LABEL_44_thunk_C = "prelude_LABEL_44_thunk"
    p250_thunk = newclosure prelude_LABEL_44_thunk_C
    .local pmc p254_thunk
    .const .Sub prelude_LABEL_46_thunk_C = "prelude_LABEL_46_thunk"
    p254_thunk = newclosure prelude_LABEL_46_thunk_C
    .local pmc p256_app
    p256_app = new .Undef
    set_args '(64, 64, 64)', p249_app, p250_thunk, p254_thunk
    get_results "(0)", p256_app
    invokecc p245_fun
    .local pmc p257_lex_decl
    p257_lex_decl = new .Undef
    .lex "$listlen" ,  p257_lex_decl
    .local pmc p258_fun
    p258_fun = new .Undef
    p258_fun = find_name "&prefix:+"
    .local pmc p259_lex
    p259_lex = new .Undef
    p259_lex = find_lex "@list"
    .local pmc p260_app
    p260_app = new .Undef
    set_args '(64)', p259_lex
    get_results "(0)", p260_app
    invokecc p258_fun
    store_lex "$listlen", p260_app
    .local pmc p261_lex_decl
    p261_lex_decl = new .Undef
    .lex "$size_change" ,  p261_lex_decl
    .local pmc p262_fun
    p262_fun = new .Undef
    p262_fun = find_name "&infix:-"
    .local pmc p263_lex
    p263_lex = new .Undef
    p263_lex = find_lex "$listlen"
    .local pmc p264_lex
    p264_lex = new .Undef
    p264_lex = find_lex "$len"
    .local pmc p265_app
    p265_app = new .Undef
    set_args '(64, 64)', p263_lex, p264_lex
    get_results "(0)", p265_app
    invokecc p262_fun
    store_lex "$size_change", p265_app
    .local pmc p266_lex_decl
    p266_lex_decl = new .Undef
    .lex "@result" ,  p266_lex_decl
    .local pmc p267_fun
    p267_fun = new .Undef
    p267_fun = find_name "&statement_control:cond"
    .local pmc p268_lit
    p268_lit = new .Undef
    p268_lit = assign 1
    .local pmc p269_thunk
    .const .Sub prelude_LABEL_48_thunk_C = "prelude_LABEL_48_thunk"
    p269_thunk = newclosure prelude_LABEL_48_thunk_C
    .local pmc p296_thunk
    .const .Sub prelude_LABEL_52_thunk_C = "prelude_LABEL_52_thunk"
    p296_thunk = newclosure prelude_LABEL_52_thunk_C
    .local pmc p298_app
    p298_app = new .Undef
    set_args '(64, 64, 64)', p268_lit, p269_thunk, p296_thunk
    get_results "(0)", p298_app
    invokecc p267_fun
    .local pmc p299_fun
    p299_fun = new .Undef
    p299_fun = find_name "&statement_control:cond"
    .local pmc p300_fun
    p300_fun = new .Undef
    p300_fun = find_name "&infix:>"
    .local pmc p301_lex
    p301_lex = new .Undef
    p301_lex = find_lex "$size_change"
    .local pmc p302_lit
    p302_lit = new .Undef
    p302_lit = assign 0
    .local pmc p303_app
    p303_app = new .Undef
    set_args '(64, 64)', p301_lex, p302_lit
    get_results "(0)", p303_app
    invokecc p300_fun
    .local pmc p304_thunk
    .const .Sub prelude_LABEL_53_thunk_C = "prelude_LABEL_53_thunk"
    p304_thunk = newclosure prelude_LABEL_53_thunk_C
    .local pmc p341_thunk
    .const .Sub prelude_LABEL_57_thunk_C = "prelude_LABEL_57_thunk"
    p341_thunk = newclosure prelude_LABEL_57_thunk_C
    .local pmc p400_app
    p400_app = new .Undef
    set_args '(64, 64, 64)', p303_app, p304_thunk, p341_thunk
    get_results "(0)", p400_app
    invokecc p299_fun
    .local pmc p401_fun
    p401_fun = new .Undef
    p401_fun = find_name "&statement_control:cond"
    .local pmc p402_fun
    p402_fun = new .Undef
    p402_fun = find_name "&infix:>"
    .local pmc p403_lex
    p403_lex = new .Undef
    p403_lex = find_lex "$listlen"
    .local pmc p404_lit
    p404_lit = new .Undef
    p404_lit = assign 0
    .local pmc p405_app
    p405_app = new .Undef
    set_args '(64, 64)', p403_lex, p404_lit
    get_results "(0)", p405_app
    invokecc p402_fun
    .local pmc p406_thunk
    .const .Sub prelude_LABEL_66_thunk_C = "prelude_LABEL_66_thunk"
    p406_thunk = newclosure prelude_LABEL_66_thunk_C
    .local pmc p432_thunk
    .const .Sub prelude_LABEL_70_thunk_C = "prelude_LABEL_70_thunk"
    p432_thunk = newclosure prelude_LABEL_70_thunk_C
    .local pmc p434_app
    p434_app = new .Undef
    set_args '(64, 64, 64)', p405_app, p406_thunk, p432_thunk
    get_results "(0)", p434_app
    invokecc p401_fun
    .local pmc p435_lex
    p435_lex = new .Undef
    p435_lex = find_lex "@result"
    set_returns "(64)", p435_lex
    returncc
.end
.sub "prelude_LABEL_25_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p174_block
    .const .Sub prelude_LABEL_26_block_C = "prelude_LABEL_26_block"
    p174_block = newclosure prelude_LABEL_26_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p174_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_26_block" :anon  :outer("prelude_LABEL_25_thunk")
    get_params "()"
    .local pmc p175_fun
    p175_fun = new .Undef
    p175_fun = find_name "&infix:+"
    .local pmc p176_lex
    p176_lex = new .Undef
    p176_lex = find_lex "$off"
    .local pmc p177_lex
    p177_lex = new .Undef
    p177_lex = find_lex "$size"
    .local pmc p178_app
    p178_app = new .Undef
    set_args '(64, 64)', p176_lex, p177_lex
    get_results "(0)", p178_app
    invokecc p175_fun
    store_lex "$off", p178_app
    .local pmc p179_lex
    p179_lex = new .Undef
    p179_lex = find_lex "$off"
    set_returns "(64)", p179_lex
    returncc
.end
.sub "prelude_LABEL_27_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p181_block
    .const .Sub prelude_LABEL_28_block_C = "prelude_LABEL_28_block"
    p181_block = newclosure prelude_LABEL_28_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p181_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_28_block" :anon  :outer("prelude_LABEL_27_thunk")
    get_params "()"
    set_returns "(64)", $P0
    returncc
.end
.sub "prelude_LABEL_29_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p189_block
    .const .Sub prelude_LABEL_30_block_C = "prelude_LABEL_30_block"
    p189_block = newclosure prelude_LABEL_30_block_C
    set_returns "(64)", p189_block
    returncc
.end
.sub "prelude_LABEL_30_block" :anon  :outer("prelude_LABEL_29_thunk")
    get_params "()"
    .local pmc p190_fun
    p190_fun = new .Undef
    p190_fun = find_name "&warn"
    .local pmc p191_lit
    p191_lit = new .Undef
    p191_lit = assign "splice() offset past end of array\n"
    .local pmc p192_app
    p192_app = new .Undef
    set_args '(64)', p191_lit
    get_results "(0)", p192_app
    invokecc p190_fun
    .local pmc p193_lex
    p193_lex = new .Undef
    p193_lex = find_lex "$size"
    store_lex "$off", p193_lex
    .local pmc p194_lex
    p194_lex = new .Undef
    p194_lex = find_lex "$off"
    set_returns "(64)", p194_lex
    returncc
.end
.sub "prelude_LABEL_31_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p196_undef
    p196_undef = new .Undef
    set_returns "(64)", p196_undef
    returncc
.end
.sub "prelude_LABEL_32_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p203_block
    .const .Sub prelude_LABEL_33_block_C = "prelude_LABEL_33_block"
    p203_block = newclosure prelude_LABEL_33_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p203_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_33_block" :anon  :outer("prelude_LABEL_32_thunk")
    get_params "()"
    .local pmc p204_fun
    p204_fun = new .Undef
    p204_fun = find_name "&prefix:+"
    .local pmc p205_lex
    p205_lex = new .Undef
    p205_lex = find_lex "$len"
    .local pmc p206_app
    p206_app = new .Undef
    set_args '(64)', p205_lex
    get_results "(0)", p206_app
    invokecc p204_fun
    store_lex "$len", p206_app
    .local pmc p207_lex
    p207_lex = new .Undef
    p207_lex = find_lex "$len"
    set_returns "(64)", p207_lex
    returncc
.end
.sub "prelude_LABEL_34_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p209_block
    .const .Sub prelude_LABEL_35_block_C = "prelude_LABEL_35_block"
    p209_block = newclosure prelude_LABEL_35_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p209_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_35_block" :anon  :outer("prelude_LABEL_34_thunk")
    get_params "()"
    set_returns "(64)", $P0
    returncc
.end
.sub "prelude_LABEL_36_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p218_block
    .const .Sub prelude_LABEL_37_block_C = "prelude_LABEL_37_block"
    p218_block = newclosure prelude_LABEL_37_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p218_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_37_block" :anon  :outer("prelude_LABEL_36_thunk")
    get_params "()"
    .local pmc p219_fun
    p219_fun = new .Undef
    p219_fun = find_name "&infix:-"
    .local pmc p220_lex
    p220_lex = new .Undef
    p220_lex = find_lex "$size"
    .local pmc p221_lex
    p221_lex = new .Undef
    p221_lex = find_lex "$off"
    .local pmc p222_app
    p222_app = new .Undef
    set_args '(64, 64)', p220_lex, p221_lex
    get_results "(0)", p222_app
    invokecc p219_fun
    store_lex "$len", p222_app
    .local pmc p223_lex
    p223_lex = new .Undef
    p223_lex = find_lex "$len"
    set_returns "(64)", p223_lex
    returncc
.end
.sub "prelude_LABEL_38_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p225_block
    .const .Sub prelude_LABEL_39_block_C = "prelude_LABEL_39_block"
    p225_block = newclosure prelude_LABEL_39_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p225_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_39_block" :anon  :outer("prelude_LABEL_38_thunk")
    get_params "()"
    set_returns "(64)", $P0
    returncc
.end
.sub "prelude_LABEL_40_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p233_block
    .const .Sub prelude_LABEL_41_block_C = "prelude_LABEL_41_block"
    p233_block = newclosure prelude_LABEL_41_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p233_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_41_block" :anon  :outer("prelude_LABEL_40_thunk")
    get_params "()"
    .local pmc p234_fun
    p234_fun = new .Undef
    p234_fun = find_name "&infix:-"
    .local pmc p235_fun
    p235_fun = new .Undef
    p235_fun = find_name "&infix:+"
    .local pmc p236_lex
    p236_lex = new .Undef
    p236_lex = find_lex "$size"
    .local pmc p237_lex
    p237_lex = new .Undef
    p237_lex = find_lex "$len"
    .local pmc p238_app
    p238_app = new .Undef
    set_args '(64, 64)', p236_lex, p237_lex
    get_results "(0)", p238_app
    invokecc p235_fun
    .local pmc p239_lex
    p239_lex = new .Undef
    p239_lex = find_lex "$off"
    .local pmc p240_app
    p240_app = new .Undef
    set_args '(64, 64)', p238_app, p239_lex
    get_results "(0)", p240_app
    invokecc p234_fun
    store_lex "$len", p240_app
    .local pmc p241_lex
    p241_lex = new .Undef
    p241_lex = find_lex "$len"
    set_returns "(64)", p241_lex
    returncc
.end
.sub "prelude_LABEL_42_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p243_block
    .const .Sub prelude_LABEL_43_block_C = "prelude_LABEL_43_block"
    p243_block = newclosure prelude_LABEL_43_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p243_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_43_block" :anon  :outer("prelude_LABEL_42_thunk")
    get_params "()"
    set_returns "(64)", $P0
    returncc
.end
.sub "prelude_LABEL_44_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p251_block
    .const .Sub prelude_LABEL_45_block_C = "prelude_LABEL_45_block"
    p251_block = newclosure prelude_LABEL_45_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p251_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_45_block" :anon  :outer("prelude_LABEL_44_thunk")
    get_params "()"
    .local pmc p252_lit
    p252_lit = new .Undef
    p252_lit = assign 0
    store_lex "$len", p252_lit
    .local pmc p253_lex
    p253_lex = new .Undef
    p253_lex = find_lex "$len"
    set_returns "(64)", p253_lex
    returncc
.end
.sub "prelude_LABEL_46_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p255_block
    .const .Sub prelude_LABEL_47_block_C = "prelude_LABEL_47_block"
    p255_block = newclosure prelude_LABEL_47_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p255_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_47_block" :anon  :outer("prelude_LABEL_46_thunk")
    get_params "()"
    set_returns "(64)", $P0
    returncc
.end
.sub "prelude_LABEL_48_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p270_block
    .const .Sub prelude_LABEL_49_block_C = "prelude_LABEL_49_block"
    p270_block = newclosure prelude_LABEL_49_block_C
    set_returns "(64)", p270_block
    returncc
.end
.sub "prelude_LABEL_49_block" :anon  :outer("prelude_LABEL_48_thunk")
    get_params "()"
    .local pmc p271_lex_decl
    p271_lex_decl = new .Undef
    .lex "$i" ,  p271_lex_decl
    .local pmc p272_lex_decl
    p272_lex_decl = new .Undef
    .lex "$off" ,  p272_lex_decl
    store_lex "$i", p272_lex_decl
    .local pmc p273_lex_decl
    p273_lex_decl = new .Undef
    .lex "$stop" ,  p273_lex_decl
    .local pmc p274_fun
    p274_fun = new .Undef
    p274_fun = find_name "&infix:+"
    .local pmc p275_lex
    p275_lex = new .Undef
    p275_lex = find_lex "$off"
    .local pmc p276_lex
    p276_lex = new .Undef
    p276_lex = find_lex "$len"
    .local pmc p277_app
    p277_app = new .Undef
    set_args '(64, 64)', p275_lex, p276_lex
    get_results "(0)", p277_app
    invokecc p274_fun
    store_lex "$stop", p277_app
    .local pmc p278_fun
    p278_fun = new .Undef
    p278_fun = find_name "&statement_control:while"
    .local pmc p279_block
    .const .Sub prelude_LABEL_50_block_C = "prelude_LABEL_50_block"
    p279_block = newclosure prelude_LABEL_50_block_C
    .local pmc p284_block
    .const .Sub prelude_LABEL_51_block_C = "prelude_LABEL_51_block"
    p284_block = newclosure prelude_LABEL_51_block_C
    .local pmc p295_app
    p295_app = new .Undef
    set_args '(64, 64)', p279_block, p284_block
    get_results "(0)", p295_app
    invokecc p278_fun
    set_returns "(64)", p295_app
    returncc
.end
.sub "prelude_LABEL_50_block" :anon  :outer("prelude_LABEL_49_block")
    get_params "()"
    .local pmc p280_fun
    p280_fun = new .Undef
    p280_fun = find_name "&infix:<"
    .local pmc p281_lex
    p281_lex = new .Undef
    p281_lex = find_lex "$i"
    .local pmc p282_lex
    p282_lex = new .Undef
    p282_lex = find_lex "$stop"
    .local pmc p283_app
    p283_app = new .Undef
    set_args '(64, 64)', p281_lex, p282_lex
    get_results "(0)", p283_app
    invokecc p280_fun
    set_returns "(64)", p283_app
    returncc
.end
.sub "prelude_LABEL_51_block" :anon  :outer("prelude_LABEL_49_block")
    get_params "()"
    .local pmc p285_fun
    p285_fun = new .Undef
    p285_fun = find_name "&push"
    .local pmc p286_lex
    p286_lex = new .Undef
    p286_lex = find_lex "@result"
    .local pmc p287_fun
    p287_fun = new .Undef
    p287_fun = find_name "&postcircumfix:[]"
    .local pmc p288_lex
    p288_lex = new .Undef
    p288_lex = find_lex "@a"
    .local pmc p289_lex
    p289_lex = new .Undef
    p289_lex = find_lex "$i"
    .local pmc p290_app
    p290_app = new .Undef
    set_args '(64, 64)', p288_lex, p289_lex
    get_results "(0)", p290_app
    invokecc p287_fun
    .local pmc p291_app
    p291_app = new .Undef
    set_args '(64, 64)', p286_lex, p290_app
    get_results "(0)", p291_app
    invokecc p285_fun
    .local pmc p292_fun
    p292_fun = new .Undef
    p292_fun = find_name "&postfix:++"
    .local pmc p293_lex
    p293_lex = new .Undef
    p293_lex = find_lex "$i"
    .local pmc p294_app
    p294_app = new .Undef
    set_args '(64)', p293_lex
    get_results "(0)", p294_app
    invokecc p292_fun
    set_returns "(64)", p294_app
    returncc
.end
.sub "prelude_LABEL_52_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p297_undef
    p297_undef = new .Undef
    set_returns "(64)", p297_undef
    returncc
.end
.sub "prelude_LABEL_53_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p305_block
    .const .Sub prelude_LABEL_54_block_C = "prelude_LABEL_54_block"
    p305_block = newclosure prelude_LABEL_54_block_C
    set_returns "(64)", p305_block
    returncc
.end
.sub "prelude_LABEL_54_block" :anon  :outer("prelude_LABEL_53_thunk")
    get_params "()"
    .local pmc p306_lex_decl
    p306_lex_decl = new .Undef
    .lex "$i" ,  p306_lex_decl
    .local pmc p307_fun
    p307_fun = new .Undef
    p307_fun = find_name "&infix:-"
    .local pmc p308_fun
    p308_fun = new .Undef
    p308_fun = find_name "&infix:+"
    .local pmc p309_lex
    p309_lex = new .Undef
    p309_lex = find_lex "$size"
    .local pmc p310_lex
    p310_lex = new .Undef
    p310_lex = find_lex "$size_change"
    .local pmc p311_app
    p311_app = new .Undef
    set_args '(64, 64)', p309_lex, p310_lex
    get_results "(0)", p311_app
    invokecc p308_fun
    .local pmc p312_lit
    p312_lit = new .Undef
    p312_lit = assign 1
    .local pmc p313_app
    p313_app = new .Undef
    set_args '(64, 64)', p311_app, p312_lit
    get_results "(0)", p313_app
    invokecc p307_fun
    store_lex "$i", p313_app
    .local pmc p314_lex_decl
    p314_lex_decl = new .Undef
    .lex "$final" ,  p314_lex_decl
    .local pmc p315_fun
    p315_fun = new .Undef
    p315_fun = find_name "&infix:+"
    .local pmc p316_lex
    p316_lex = new .Undef
    p316_lex = find_lex "$off"
    .local pmc p317_lex
    p317_lex = new .Undef
    p317_lex = find_lex "$size_change"
    .local pmc p318_app
    p318_app = new .Undef
    set_args '(64, 64)', p316_lex, p317_lex
    get_results "(0)", p318_app
    invokecc p315_fun
    store_lex "$final", p318_app
    .local pmc p319_fun
    p319_fun = new .Undef
    p319_fun = find_name "&statement_control:while"
    .local pmc p320_block
    .const .Sub prelude_LABEL_55_block_C = "prelude_LABEL_55_block"
    p320_block = newclosure prelude_LABEL_55_block_C
    .local pmc p325_block
    .const .Sub prelude_LABEL_56_block_C = "prelude_LABEL_56_block"
    p325_block = newclosure prelude_LABEL_56_block_C
    .local pmc p340_app
    p340_app = new .Undef
    set_args '(64, 64)', p320_block, p325_block
    get_results "(0)", p340_app
    invokecc p319_fun
    set_returns "(64)", p340_app
    returncc
.end
.sub "prelude_LABEL_55_block" :anon  :outer("prelude_LABEL_54_block")
    get_params "()"
    .local pmc p321_fun
    p321_fun = new .Undef
    p321_fun = find_name "&infix:>="
    .local pmc p322_lex
    p322_lex = new .Undef
    p322_lex = find_lex "$i"
    .local pmc p323_lex
    p323_lex = new .Undef
    p323_lex = find_lex "$final"
    .local pmc p324_app
    p324_app = new .Undef
    set_args '(64, 64)', p322_lex, p323_lex
    get_results "(0)", p324_app
    invokecc p321_fun
    set_returns "(64)", p324_app
    returncc
.end
.sub "prelude_LABEL_56_block" :anon  :outer("prelude_LABEL_54_block")
    get_params "()"
    .local pmc p326_fun
    p326_fun = new .Undef
    p326_fun = find_name "&postcircumfix:[]"
    .local pmc p327_lex
    p327_lex = new .Undef
    p327_lex = find_lex "@a"
    .local pmc p328_lex
    p328_lex = new .Undef
    p328_lex = find_lex "$i"
    .local pmc p329_app
    p329_app = new .Undef
    set_args '(64, 64)', p327_lex, p328_lex
    get_results "(0)", p329_app
    invokecc p326_fun
    .local pmc p330_fun
    p330_fun = new .Undef
    p330_fun = find_name "&postcircumfix:[]"
    .local pmc p331_lex
    p331_lex = new .Undef
    p331_lex = find_lex "@a"
    .local pmc p332_fun
    p332_fun = new .Undef
    p332_fun = find_name "&infix:-"
    .local pmc p333_lex
    p333_lex = new .Undef
    p333_lex = find_lex "$i"
    .local pmc p334_lex
    p334_lex = new .Undef
    p334_lex = find_lex "$size_change"
    .local pmc p335_app
    p335_app = new .Undef
    set_args '(64, 64)', p333_lex, p334_lex
    get_results "(0)", p335_app
    invokecc p332_fun
    .local pmc p336_app
    p336_app = new .Undef
    set_args '(64, 64)', p331_lex, p335_app
    get_results "(0)", p336_app
    invokecc p330_fun
    p329_app = assign p336_app
    .local pmc p337_fun
    p337_fun = new .Undef
    p337_fun = find_name "&postfix:--"
    .local pmc p338_lex
    p338_lex = new .Undef
    p338_lex = find_lex "$i"
    .local pmc p339_app
    p339_app = new .Undef
    set_args '(64)', p338_lex
    get_results "(0)", p339_app
    invokecc p337_fun
    set_returns "(64)", p339_app
    returncc
.end
.sub "prelude_LABEL_57_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p342_block
    .const .Sub prelude_LABEL_58_block_C = "prelude_LABEL_58_block"
    p342_block = newclosure prelude_LABEL_58_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p342_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_58_block" :anon  :outer("prelude_LABEL_57_thunk")
    get_params "()"
    .local pmc p343_fun
    p343_fun = new .Undef
    p343_fun = find_name "&statement_control:cond"
    .local pmc p344_fun
    p344_fun = new .Undef
    p344_fun = find_name "&infix:<"
    .local pmc p345_lex
    p345_lex = new .Undef
    p345_lex = find_lex "$size_change"
    .local pmc p346_lit
    p346_lit = new .Undef
    p346_lit = assign 0
    .local pmc p347_app
    p347_app = new .Undef
    set_args '(64, 64)', p345_lex, p346_lit
    get_results "(0)", p347_app
    invokecc p344_fun
    .local pmc p348_thunk
    .const .Sub prelude_LABEL_59_thunk_C = "prelude_LABEL_59_thunk"
    p348_thunk = newclosure prelude_LABEL_59_thunk_C
    .local pmc p397_thunk
    .const .Sub prelude_LABEL_65_thunk_C = "prelude_LABEL_65_thunk"
    p397_thunk = newclosure prelude_LABEL_65_thunk_C
    .local pmc p399_app
    p399_app = new .Undef
    set_args '(64, 64, 64)', p347_app, p348_thunk, p397_thunk
    get_results "(0)", p399_app
    invokecc p343_fun
    set_returns "(64)", p399_app
    returncc
.end
.sub "prelude_LABEL_59_thunk" :anon  :outer("prelude_LABEL_58_block")
    .local pmc p349_block
    .const .Sub prelude_LABEL_60_block_C = "prelude_LABEL_60_block"
    p349_block = newclosure prelude_LABEL_60_block_C
    set_returns "(64)", p349_block
    returncc
.end
.sub "prelude_LABEL_60_block" :anon  :outer("prelude_LABEL_59_thunk")
    get_params "()"
    .local pmc p350_lex_decl
    p350_lex_decl = new .Undef
    .lex "$i" ,  p350_lex_decl
    .local pmc p351_lex_decl
    p351_lex_decl = new .Undef
    .lex "$off" ,  p351_lex_decl
    store_lex "$i", p351_lex_decl
    .local pmc p352_lex_decl
    p352_lex_decl = new .Undef
    .lex "$final" ,  p352_lex_decl
    .local pmc p353_fun
    p353_fun = new .Undef
    p353_fun = find_name "&infix:-"
    .local pmc p354_fun
    p354_fun = new .Undef
    p354_fun = find_name "&infix:+"
    .local pmc p355_lex
    p355_lex = new .Undef
    p355_lex = find_lex "$size"
    .local pmc p356_lex
    p356_lex = new .Undef
    p356_lex = find_lex "$size_change"
    .local pmc p357_app
    p357_app = new .Undef
    set_args '(64, 64)', p355_lex, p356_lex
    get_results "(0)", p357_app
    invokecc p354_fun
    .local pmc p358_lit
    p358_lit = new .Undef
    p358_lit = assign 1
    .local pmc p359_app
    p359_app = new .Undef
    set_args '(64, 64)', p357_app, p358_lit
    get_results "(0)", p359_app
    invokecc p353_fun
    store_lex "$final", p359_app
    .local pmc p360_fun
    p360_fun = new .Undef
    p360_fun = find_name "&statement_control:while"
    .local pmc p361_block
    .const .Sub prelude_LABEL_61_block_C = "prelude_LABEL_61_block"
    p361_block = newclosure prelude_LABEL_61_block_C
    .local pmc p366_block
    .const .Sub prelude_LABEL_62_block_C = "prelude_LABEL_62_block"
    p366_block = newclosure prelude_LABEL_62_block_C
    .local pmc p381_app
    p381_app = new .Undef
    set_args '(64, 64)', p361_block, p366_block
    get_results "(0)", p381_app
    invokecc p360_fun
    .local pmc p382_lex_decl
    p382_lex_decl = new .Undef
    .lex "$n" ,  p382_lex_decl
    .local pmc p383_lit
    p383_lit = new .Undef
    p383_lit = assign 0
    store_lex "$n", p383_lit
    .local pmc p384_fun
    p384_fun = new .Undef
    p384_fun = find_name "&statement_control:while"
    .local pmc p385_block
    .const .Sub prelude_LABEL_63_block_C = "prelude_LABEL_63_block"
    p385_block = newclosure prelude_LABEL_63_block_C
    .local pmc p392_block
    .const .Sub prelude_LABEL_64_block_C = "prelude_LABEL_64_block"
    p392_block = newclosure prelude_LABEL_64_block_C
    .local pmc p396_app
    p396_app = new .Undef
    set_args '(64, 64)', p385_block, p392_block
    get_results "(0)", p396_app
    invokecc p384_fun
    set_returns "(64)", p396_app
    returncc
.end
.sub "prelude_LABEL_61_block" :anon  :outer("prelude_LABEL_60_block")
    get_params "()"
    .local pmc p362_fun
    p362_fun = new .Undef
    p362_fun = find_name "&infix:<="
    .local pmc p363_lex
    p363_lex = new .Undef
    p363_lex = find_lex "$i"
    .local pmc p364_lex
    p364_lex = new .Undef
    p364_lex = find_lex "$final"
    .local pmc p365_app
    p365_app = new .Undef
    set_args '(64, 64)', p363_lex, p364_lex
    get_results "(0)", p365_app
    invokecc p362_fun
    set_returns "(64)", p365_app
    returncc
.end
.sub "prelude_LABEL_62_block" :anon  :outer("prelude_LABEL_60_block")
    get_params "()"
    .local pmc p367_fun
    p367_fun = new .Undef
    p367_fun = find_name "&postcircumfix:[]"
    .local pmc p368_lex
    p368_lex = new .Undef
    p368_lex = find_lex "@a"
    .local pmc p369_lex
    p369_lex = new .Undef
    p369_lex = find_lex "$i"
    .local pmc p370_app
    p370_app = new .Undef
    set_args '(64, 64)', p368_lex, p369_lex
    get_results "(0)", p370_app
    invokecc p367_fun
    .local pmc p371_fun
    p371_fun = new .Undef
    p371_fun = find_name "&postcircumfix:[]"
    .local pmc p372_lex
    p372_lex = new .Undef
    p372_lex = find_lex "@a"
    .local pmc p373_fun
    p373_fun = new .Undef
    p373_fun = find_name "&infix:-"
    .local pmc p374_lex
    p374_lex = new .Undef
    p374_lex = find_lex "$i"
    .local pmc p375_lex
    p375_lex = new .Undef
    p375_lex = find_lex "$size_change"
    .local pmc p376_app
    p376_app = new .Undef
    set_args '(64, 64)', p374_lex, p375_lex
    get_results "(0)", p376_app
    invokecc p373_fun
    .local pmc p377_app
    p377_app = new .Undef
    set_args '(64, 64)', p372_lex, p376_app
    get_results "(0)", p377_app
    invokecc p371_fun
    p370_app = assign p377_app
    .local pmc p378_fun
    p378_fun = new .Undef
    p378_fun = find_name "&postfix:++"
    .local pmc p379_lex
    p379_lex = new .Undef
    p379_lex = find_lex "$i"
    .local pmc p380_app
    p380_app = new .Undef
    set_args '(64)', p379_lex
    get_results "(0)", p380_app
    invokecc p378_fun
    set_returns "(64)", p380_app
    returncc
.end
.sub "prelude_LABEL_63_block" :anon  :outer("prelude_LABEL_60_block")
    get_params "()"
    .local pmc p386_fun
    p386_fun = new .Undef
    p386_fun = find_name "&infix:>"
    .local pmc p387_fun
    p387_fun = new .Undef
    p387_fun = find_name "&postfix:--"
    .local pmc p388_lex
    p388_lex = new .Undef
    p388_lex = find_lex "$n"
    .local pmc p389_app
    p389_app = new .Undef
    set_args '(64)', p388_lex
    get_results "(0)", p389_app
    invokecc p387_fun
    .local pmc p390_lex
    p390_lex = new .Undef
    p390_lex = find_lex "$size_change"
    .local pmc p391_app
    p391_app = new .Undef
    set_args '(64, 64)', p389_app, p390_lex
    get_results "(0)", p391_app
    invokecc p386_fun
    set_returns "(64)", p391_app
    returncc
.end
.sub "prelude_LABEL_64_block" :anon  :outer("prelude_LABEL_60_block")
    get_params "()"
    .local pmc p393_fun
    p393_fun = new .Undef
    p393_fun = find_name "&pop"
    .local pmc p394_lex
    p394_lex = new .Undef
    p394_lex = find_lex "@a"
    .local pmc p395_app
    p395_app = new .Undef
    set_args '(64)', p394_lex
    get_results "(0)", p395_app
    invokecc p393_fun
    set_returns "(64)", p395_app
    returncc
.end
.sub "prelude_LABEL_65_thunk" :anon  :outer("prelude_LABEL_58_block")
    .local pmc p398_undef
    p398_undef = new .Undef
    set_returns "(64)", p398_undef
    returncc
.end
.sub "prelude_LABEL_66_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p407_block
    .const .Sub prelude_LABEL_67_block_C = "prelude_LABEL_67_block"
    p407_block = newclosure prelude_LABEL_67_block_C
    set_returns "(64)", p407_block
    returncc
.end
.sub "prelude_LABEL_67_block" :anon  :outer("prelude_LABEL_66_thunk")
    get_params "()"
    .local pmc p408_lex_decl
    p408_lex_decl = new .Undef
    .lex "$i" ,  p408_lex_decl
    .local pmc p409_lit
    p409_lit = new .Undef
    p409_lit = assign 0
    store_lex "$i", p409_lit
    .local pmc p410_fun
    p410_fun = new .Undef
    p410_fun = find_name "&statement_control:while"
    .local pmc p411_block
    .const .Sub prelude_LABEL_68_block_C = "prelude_LABEL_68_block"
    p411_block = newclosure prelude_LABEL_68_block_C
    .local pmc p416_block
    .const .Sub prelude_LABEL_69_block_C = "prelude_LABEL_69_block"
    p416_block = newclosure prelude_LABEL_69_block_C
    .local pmc p431_app
    p431_app = new .Undef
    set_args '(64, 64)', p411_block, p416_block
    get_results "(0)", p431_app
    invokecc p410_fun
    set_returns "(64)", p431_app
    returncc
.end
.sub "prelude_LABEL_68_block" :anon  :outer("prelude_LABEL_67_block")
    get_params "()"
    .local pmc p412_fun
    p412_fun = new .Undef
    p412_fun = find_name "&infix:<"
    .local pmc p413_lex
    p413_lex = new .Undef
    p413_lex = find_lex "$i"
    .local pmc p414_lex
    p414_lex = new .Undef
    p414_lex = find_lex "$listlen"
    .local pmc p415_app
    p415_app = new .Undef
    set_args '(64, 64)', p413_lex, p414_lex
    get_results "(0)", p415_app
    invokecc p412_fun
    set_returns "(64)", p415_app
    returncc
.end
.sub "prelude_LABEL_69_block" :anon  :outer("prelude_LABEL_67_block")
    get_params "()"
    .local pmc p417_fun
    p417_fun = new .Undef
    p417_fun = find_name "&postcircumfix:[]"
    .local pmc p418_lex
    p418_lex = new .Undef
    p418_lex = find_lex "@a"
    .local pmc p419_fun
    p419_fun = new .Undef
    p419_fun = find_name "&infix:+"
    .local pmc p420_lex
    p420_lex = new .Undef
    p420_lex = find_lex "$off"
    .local pmc p421_lex
    p421_lex = new .Undef
    p421_lex = find_lex "$i"
    .local pmc p422_app
    p422_app = new .Undef
    set_args '(64, 64)', p420_lex, p421_lex
    get_results "(0)", p422_app
    invokecc p419_fun
    .local pmc p423_app
    p423_app = new .Undef
    set_args '(64, 64)', p418_lex, p422_app
    get_results "(0)", p423_app
    invokecc p417_fun
    .local pmc p424_fun
    p424_fun = new .Undef
    p424_fun = find_name "&postcircumfix:[]"
    .local pmc p425_lex
    p425_lex = new .Undef
    p425_lex = find_lex "@list"
    .local pmc p426_lex
    p426_lex = new .Undef
    p426_lex = find_lex "$i"
    .local pmc p427_app
    p427_app = new .Undef
    set_args '(64, 64)', p425_lex, p426_lex
    get_results "(0)", p427_app
    invokecc p424_fun
    p423_app = assign p427_app
    .local pmc p428_fun
    p428_fun = new .Undef
    p428_fun = find_name "&postfix:++"
    .local pmc p429_lex
    p429_lex = new .Undef
    p429_lex = find_lex "$i"
    .local pmc p430_app
    p430_app = new .Undef
    set_args '(64)', p429_lex
    get_results "(0)", p430_app
    invokecc p428_fun
    set_returns "(64)", p430_app
    returncc
.end
.sub "prelude_LABEL_70_thunk" :anon  :outer("prelude_LABEL_24_block")
    .local pmc p433_undef
    p433_undef = new .Undef
    set_returns "(64)", p433_undef
    returncc
.end
.sub "&ucfirst"
    .local pmc s__str
    .lex "$str", s__str
    store_lex "$str", s__str
    get_params "(0)", s__str
    store_lex "$str", s__str
    .local pmc p436_block
    .const .Sub prelude_LABEL_71_block_C = "prelude_LABEL_71_block"
    p436_block = newclosure prelude_LABEL_71_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p436_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_71_block" :anon  :outer("&ucfirst")
    get_params "()"
    .local pmc p437_fun
    p437_fun = new .Undef
    p437_fun = find_name "&infix:~"
    .local pmc p438_fun
    p438_fun = new .Undef
    p438_fun = find_name "&uc"
    .local pmc p439_fun
    p439_fun = new .Undef
    p439_fun = find_name "&substr"
    .local pmc p440_lex
    p440_lex = new .Undef
    p440_lex = find_lex "$str"
    .local pmc p441_lit
    p441_lit = new .Undef
    p441_lit = assign 0
    .local pmc p442_lit
    p442_lit = new .Undef
    p442_lit = assign 1
    .local pmc p443_app
    p443_app = new .Undef
    set_args '(64, 64, 64)', p440_lex, p441_lit, p442_lit
    get_results "(0)", p443_app
    invokecc p439_fun
    .local pmc p444_app
    p444_app = new .Undef
    set_args '(64)', p443_app
    get_results "(0)", p444_app
    invokecc p438_fun
    .local pmc p445_fun
    p445_fun = new .Undef
    p445_fun = find_name "&substr"
    .local pmc p446_lex
    p446_lex = new .Undef
    p446_lex = find_lex "$str"
    .local pmc p447_lit
    p447_lit = new .Undef
    p447_lit = assign 1
    .local pmc p448_fun
    p448_fun = new .Undef
    p448_fun = find_name "&infix:-"
    .local pmc p449_fun
    p449_fun = new .Undef
    p449_fun = find_name "&chars"
    .local pmc p450_lex
    p450_lex = new .Undef
    p450_lex = find_lex "$str"
    .local pmc p451_app
    p451_app = new .Undef
    set_args '(64)', p450_lex
    get_results "(0)", p451_app
    invokecc p449_fun
    .local pmc p452_lit
    p452_lit = new .Undef
    p452_lit = assign 1
    .local pmc p453_app
    p453_app = new .Undef
    set_args '(64, 64)', p451_app, p452_lit
    get_results "(0)", p453_app
    invokecc p448_fun
    .local pmc p454_app
    p454_app = new .Undef
    set_args '(64, 64, 64)', p446_lex, p447_lit, p453_app
    get_results "(0)", p454_app
    invokecc p445_fun
    .local pmc p455_app
    p455_app = new .Undef
    set_args '(64, 64)', p444_app, p454_app
    get_results "(0)", p455_app
    invokecc p437_fun
    set_returns "(64)", p455_app
    returncc
.end
.namespace ['Main']
.sub "&begin_prelude_dummy"
    .local pmc a___95
    .lex "@_", a___95
    store_lex "@_", a___95
    get_params "(32)", a___95
    push_eh prelude_LABEL_72_returnHandler
    store_lex "@_", a___95
    .local pmc p456_block
    .const .Sub prelude_LABEL_73_block_C = "prelude_LABEL_73_block"
    p456_block = newclosure prelude_LABEL_73_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p456_block
    set_returns "(64)", $P8
    returncc
  prelude_LABEL_72_returnHandler:
    get_results "(0, 0)", $P8, $S8
    $S8 = typeof $P8
    eq $S8, "Exception", prelude_LABEL_72_errHandler
    set_returns "(0)", $P8
    returncc
  prelude_LABEL_72_errHandler:
    throw $P8
.end
.sub "prelude_LABEL_73_block" :anon  :outer("&begin_prelude_dummy")
    get_params "()"
    .local pmc p457_lit
    p457_lit = new .Undef
    p457_lit = assign 1
    set_returns "(64)", p457_lit
    returncc
.end
.namespace ['Main']
.sub "&end_prelude_dummy"
    .local pmc a___95
    .lex "@_", a___95
    store_lex "@_", a___95
    get_params "(32)", a___95
    push_eh prelude_LABEL_74_returnHandler
    store_lex "@_", a___95
    .local pmc p458_block
    .const .Sub prelude_LABEL_75_block_C = "prelude_LABEL_75_block"
    p458_block = newclosure prelude_LABEL_75_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p458_block
    set_returns "(64)", $P8
    returncc
  prelude_LABEL_74_returnHandler:
    get_results "(0, 0)", $P8, $S8
    $S8 = typeof $P8
    eq $S8, "Exception", prelude_LABEL_74_errHandler
    set_returns "(0)", $P8
    returncc
  prelude_LABEL_74_errHandler:
    throw $P8
.end
.sub "prelude_LABEL_75_block" :anon  :outer("&end_prelude_dummy")
    get_params "()"
    .local pmc p459_lit
    p459_lit = new .Undef
    p459_lit = assign 1
    set_returns "(64)", p459_lit
    returncc
.end
.namespace ['Main']
.sub "&prefix:?"
    .local pmc s__var
    .lex "$var", s__var
    store_lex "$var", s__var
    get_params "(0)", s__var
    store_lex "$var", s__var
    .local pmc p460_block
    .const .Sub prelude_LABEL_76_block_C = "prelude_LABEL_76_block"
    p460_block = newclosure prelude_LABEL_76_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p460_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_76_block" :anon  :outer("&prefix:?")
    get_params "()"
    .local pmc p461_fun
    p461_fun = new .Undef
    p461_fun = find_name "&true"
    .local pmc p462_lex
    p462_lex = new .Undef
    p462_lex = find_lex "$var"
    .local pmc p463_app
    p463_app = new .Undef
    set_args '(64)', p462_lex
    get_results "(0)", p463_app
    invokecc p461_fun
    set_returns "(64)", p463_app
    returncc
.end
.namespace ['Perl6::Internals']
.sub "&eval_parrot"
    .local pmc s__code
    .lex "$code", s__code
    store_lex "$code", s__code
    get_params "(0)", s__code
    store_lex "$code", s__code
    .local pmc p464_block
    .const .Sub prelude_LABEL_77_block_C = "prelude_LABEL_77_block"
    p464_block = newclosure prelude_LABEL_77_block_C
    set_args '()'
    get_results "(0)", $P8
    invokecc p464_block
    set_returns "(64)", $P8
    returncc
.end
.sub "prelude_LABEL_77_block" :anon  :outer("&eval_parrot")
    get_params "()"
    .local pmc p465_lex_decl
    p465_lex_decl = new .Undef
    .lex "$sub" ,  p465_lex_decl
    .local pmc p466_fun
    p466_fun = new .Undef
    p466_fun = find_name "&statement_control:if"
    .local pmc p467_fun
    p467_fun = new .Undef
    p467_fun = find_name "&infix:eq"
    .local pmc p468_fun
    p468_fun = new .Undef
    p468_fun = find_name "&substr"
    .local pmc p469_lex
    p469_lex = new .Undef
    p469_lex = find_lex "$code"
    .local pmc p470_lit
    p470_lit = new .Undef
    p470_lit = assign 0
    .local pmc p471_lit
    p471_lit = new .Undef
    p471_lit = assign 1
    .local pmc p472_app
    p472_app = new .Undef
    set_args '(64, 64, 64)', p469_lex, p470_lit, p471_lit
    get_results "(0)", p472_app
    invokecc p468_fun
    .local pmc p473_lit
    p473_lit = new .Undef
    p473_lit = assign "."
    .local pmc p474_app
    p474_app = new .Undef
    set_args '(64, 64)', p472_app, p473_lit
    get_results "(0)", p474_app
    invokecc p467_fun
    .local pmc p475_thunk
    .const .Sub prelude_LABEL_78_thunk_C = "prelude_LABEL_78_thunk"
    p475_thunk = newclosure prelude_LABEL_78_thunk_C
    .local pmc p479_thunk
    .const .Sub prelude_LABEL_79_thunk_C = "prelude_LABEL_79_thunk"
    p479_thunk = newclosure prelude_LABEL_79_thunk_C
    .local pmc p489_app
    p489_app = new .Undef
    set_args '(64, 64, 64)', p474_app, p475_thunk, p479_thunk
    get_results "(0)", p489_app
    invokecc p466_fun
    store_lex "$sub", p489_app
    .local pmc p490_fun
    p490_fun = new .Undef
    p490_fun = find_name "$sub"
    .local pmc p491_app
    p491_app = new .Undef
    set_args '()'
    get_results "(0)", p491_app
    invokecc p490_fun
    set_returns "(64)", p491_app
    returncc
.end
.sub "prelude_LABEL_78_thunk" :anon  :outer("prelude_LABEL_77_block")
    .local pmc p476_fun
    p476_fun = new .Undef
    p476_fun = find_name "&Perl6::Internals::compile_pir"
    .local pmc p477_lex
    p477_lex = new .Undef
    p477_lex = find_lex "$code"
    .local pmc p478_app
    p478_app = new .Undef
    set_args '(64)', p477_lex
    get_results "(0)", p478_app
    invokecc p476_fun
    set_returns "(64)", p478_app
    returncc
.end
.sub "prelude_LABEL_79_thunk" :anon  :outer("prelude_LABEL_77_block")
    .local pmc p480_fun
    p480_fun = new .Undef
    p480_fun = find_name "&Perl6::Internals::compile_pir"
    .local pmc p481_fun
    p481_fun = new .Undef
    p481_fun = find_name "&infix:~"
    .local pmc p482_lit
    p482_lit = new .Undef
    p482_lit = assign ".sub pugs_eval_parrot\n"
    .local pmc p483_fun
    p483_fun = new .Undef
    p483_fun = find_name "&infix:~"
    .local pmc p484_lex
    p484_lex = new .Undef
    p484_lex = find_lex "$code"
    .local pmc p485_lit
    p485_lit = new .Undef
    p485_lit = assign "\n.end\n"
    .local pmc p486_app
    p486_app = new .Undef
    set_args '(64, 64)', p484_lex, p485_lit
    get_results "(0)", p486_app
    invokecc p483_fun
    .local pmc p487_app
    p487_app = new .Undef
    set_args '(64, 64)', p482_lit, p486_app
    get_results "(0)", p487_app
    invokecc p481_fun
    .local pmc p488_app
    p488_app = new .Undef
    set_args '(64)', p487_app
    get_results "(0)", p488_app
    invokecc p480_fun
    set_returns "(64)", p488_app
    returncc
.end
