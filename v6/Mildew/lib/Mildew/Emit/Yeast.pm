package Mildew::Emit::Yeast;
sub assign {
    my ($target,$value) = @_;
     "if ($target) SMOP_RELEASE(interpreter,$target);\n"
      . "$target = " . $value . ";\n"
}
sub measure {
    my ($id,$code) = @_;
    if (defined $Mildew::profile_info) {
        ("smop_measure_start(".$id.");" . $code,"smop_measure_end(".$id.");");
    } else {
        ($code,'');
    }
}
1;
