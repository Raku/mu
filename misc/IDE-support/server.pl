

my $src = "";
while $_ = =<> {$src ~= $_;}

my $m = $src ~~ rx/oo/;
say "(mapcar 'delete-overlay (overlays-in (point-min) (point-max)))";
say "(overlay-put (make-overlay {$m.from +1} {$m.to +1} nil t t) 'face '(:foreground \"red\"))" if $m;
say '(message "did it")';
