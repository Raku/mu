#!/usr/bin/pugs

use v6;
use Test::Pil;

$Test::Pil::Description = "Binding tests";
pil_is_eq( q/$a := 1; $a.add(2)/, "3");
pil_parsed_is_eq( q/$a := 1; $a.add(2)/, q/(-> $a {$a.add(2)}).(1)/);

pil_is_eq( q/$a := 1; $b := 2; $a.add(2)/, "3");
pil_parsed_is_eq( q/$b := 2; $a.add(2)/, q/(-> $b {$a.add(2)}).(2)/);

pil_is_eq( q/$b := 2; $a.add(2); $a := 1/, "");

$Test::Pil::Description = "Rebinding tests";
pil_is_eq( q/$a := 3; $b := 4; $a := 6; $a.multiply(10)/, "60");
pil_is_eq( q/$a := 3; $b := 4; $a := 6; $a.multiply(10); $a := 11/, "nil");
pil_is_eq( q/$a := 3; $b := 4; $a := 6; $a.multiply(10); $a := 11; (-> { $a }).();/, "11");

