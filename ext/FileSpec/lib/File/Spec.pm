use v6;

module File::Spec-0.0.1;

# what follows is a really horrid hack, 
# please do not hold it against me :)

if ($?OS eq 'MSWin32') {
    eval 'require File::Spec::Win32';
}
else {
    eval 'require File::Spec::Unix';
}