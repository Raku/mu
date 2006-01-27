#!./pugs

use v6;
use Test::Pil;

check_pil();

pil_is_eq("true`not()", "false", "True not test");
pil_is_eq("false`not()", "true");

pil_is_eq("true`and(true)", "true");
pil_is_eq("true`and(false)", "false");
pil_is_eq("false`and(true)", "false");
pil_is_eq("false`and(false)", "false");

pil_is_eq("true`or(true)", "true");
pil_is_eq("true`or(false)", "true");
pil_is_eq("false`or(true)", "true");
pil_is_eq("false`or(false)", "false");

