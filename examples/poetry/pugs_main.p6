# From src/Main.hs:  
#
#   A ship then new they built for him
#   Of mithril and of elven-glass
#   With shining prow; no shaven oar
#   Nor sail she bore on silver mast;
#   The Silmaril as lantern light
#   And banner bright with living flame
#   To gleam thereon by Elbereth
#   Herself was set, who thither came...
#
for $*Larry {
    our Ship $pugs .= new(:of<mithril elven-glass>);
    given $pugs {
        $.prow does Shine;
        Silver $.mast but none(Oar::Shaven, Sail);
        Light  $.lantern := $*Silmaril;
        Bright $.banner  := Flame.bless;
        when $*Elbereth.gleam {
            .sail(...);
        }
    }
}
