
# runtime state
my $state = {
    'ck_num' => 0,
    
    'svnup_done' => 0,
    'rm_temp_dir_done' => 0,
    'copy_src_dir_done' => 0,
    'after_temp_copied_done' => 0,
    'cmd' => {
        'cmd_num' => 0,
        'before_done' => 0,
        'cmd_done' => 0,
        'after_done' => 0,
    },
};

return $state;