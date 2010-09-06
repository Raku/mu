#!/usr/bin/perl -w

# This program determines the revision (number) that has been checked out.

use strict;
use warnings;

exec 'git log --pretty=%h -1'
