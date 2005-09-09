#!/usr/bin/pugs

use Test;

plan 4;
use_ok ::Log::Selective;
lives_ok { Log::Selective::select(:magic) }, "Selected tag ':magic'";

module magic { Test::ok (Log::Selective::note "This should be output"), 'caught by autotagging' }
ok not(note "This should not be output"), 'tag not in list';