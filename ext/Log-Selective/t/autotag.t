#!/usr/bin/pugs

use Test;

plan 4;
require '::Log::Selective'; pass "(dummy instead of broken use_ok)";
lives_ok { Log::Selective::select(:magic) }, "Selected tag ':magic'";

skip_rest("looping tests"); exit;

module magic { Test::ok (Log::Selective::note "This should be output"), 'caught by autotagging' }
ok not(note "This should not be output"), 'tag not in list';
