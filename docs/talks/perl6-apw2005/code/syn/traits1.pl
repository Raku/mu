#="is rw" als Sub-Trait
sub storage is rw {...}
storage() = 5;
#="is constant"  ist der Standard
sub func( $arg is constant ) { $arg = 123; } # geht nicht!
#="is rw"  Parameter-Trait
sub swap (*@_ is rw) { @_[0,1] = @_[1,0] }
#="is copy"
sub func( $arg is copy ) { $arg = 123; }
#="is ref": eine Referenz wird erwartet
sub func( $arg is ref ) { ... }

