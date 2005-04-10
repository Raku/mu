s0 1 s0 1 R  # Seek right to the end of the numeral
s0 0 s0 0 R
s0 _ s1 _ L

s1 1 s1 0 L  # Scan left, changing 1s to 0's
s1 0 s2 1 L  # Until you find the rightmost 0
s1 _ s2 1 L  # or fall off the left end of the numeral

s2 1 s2 1 L  # Seek left to the left end of the numeral
s2 0 s2 0 L
s2 _ s3 _ R  # ... and then stop
