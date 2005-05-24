use v6;

say [~] (-> @c is copy {gather { while @c[0] { for @c -> {take(.shift)} } }
}(['Joec','utrk','shle','te6r',' r .','a h.','nPa.'].map:{[split "",$_]}));


