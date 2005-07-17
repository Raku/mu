#!/usr/bin/pugs

use v6;
use Test;

plan 34;

{
  ok  ?1,     "? context forcer works (1)";
  ok !?0,     "? context forcer works (2)";
  ok  ?(?1),  "? context forcer works (3)";
  ok !?(?0),  "? context forcer works (4)";

  ok  ?"hi",     "? context forcer works (5)";
  ok !?"",       "? context forcer works (6)";
  ok  ?(?"hi"),  "? context forcer works (7)";
  ok !?(?""),    "? context forcer works (8)";

  ok  ?"3",     "? context forcer works (9)";
  ok !?"0",     "? context forcer works (10)";
  ok  ?(?"3"),  "? context forcer works (11)";
  ok !?(?"0"),  "? context forcer works (12)";
}

{
  is +1,           1, "+ context forcer works (1)";
  is +0,           0, "+ context forcer works (2)";
  is +"1",         1, "+ context forcer works (3)";
  is +"0",         0, "+ context forcer works (4)";
  is +"",          0, "+ context forcer works (5)";
  is +undef,       0, "+ context forcer works (6)";
  is +"Inf",     Inf, "+ context forcer works (7)";
  is +"-Inf",   -Inf, "+ context forcer works (8)";
  is +"NaN",     NaN, "+ context forcer works (9)";
  is +"3e5",  300000, "+ context forcer works (10)";
  is +(?0),        0, "+ context forcer works (11)";
  is +(?3),        1, "+ context forcer works (11)";
}

{
  is ~1,         "1", "? context forcer works (1)";
  is ~0,         "0", "? context forcer works (2)";
  is ~"1",       "1", "? context forcer works (3)";
  is ~"0",       "0", "? context forcer works (4)";
  is ~"",         "", "? context forcer works (5)";
  is ~undef,      "", "? context forcer works (6)";
  is ~"Inf",   "Inf", "? context forcer works (7)";
  is ~"-Inf", "-Inf", "? context forcer works (8)";
  is ~"NaN",   "NaN", "? context forcer works (9)";
  is ~"3e5",   "3e5", "? context forcer works (10)";
}
