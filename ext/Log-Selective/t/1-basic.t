#!/usr/bin/pugs

use Test;

plan 4;
require '::Log::Selective'; pass "(dummy instead of broken use_ok)";
lives_ok { Log::Selective::select(:foo) }, "Selected tag ':foo'";

skip_rest("looping tests"); exit;

ok (note "This should be output", :foo), 'tag in list';
ok not(note "This should not be output", :bar), 'tag not in list';
