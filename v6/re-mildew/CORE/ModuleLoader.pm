my $loader = ::MildewSOLoader.new;
role ModuleLoader {
    has $.cache;
    method load($module) {
        my $filename = self.resolve_filename($module);
        if $.cache{$filename.FETCH} {
        } else {
            $.cache{$filename.FETCH} = $loader.load($filename.FETCH,$LexicalPrelude.FETCH);
        }
        $.cache{$filename.FETCH};
    }
    method resolve_filename($module) {
         'lib/' ~ $module ~ '.mildew.so'
    }
    method BUILDALL() {
        $.cache = ::Hash.new;
    }
}
$LexicalPrelude{'ModuleLoader'} = ::ModuleLoader;
