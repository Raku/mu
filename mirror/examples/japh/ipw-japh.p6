use v6;
# this JAPH was written for the t-shirt of the Second Italian Perl Workshop
# author: dakkar (with help from autrijus)

say [~] (-> @c is copy {gather { while @c[0] { for @c -> {take(.shift)} } }
}(['Joec','utrk','shle','te6r',' r .','a h.','nPa.'].map:{[split "",$_]}));


