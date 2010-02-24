package Emit::Yeast;
sub assign {
    my ($target,$value) = @_;
     "if ($target) SMOP_RELEASE(interpreter,$target);\n"
      . "$target = " . $value . ";\n"
}
1;
