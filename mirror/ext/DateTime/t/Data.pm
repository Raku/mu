use v6;

@*MonthLengths =
    ( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );

@*LeapYearMonthLengths = @*MonthLengths;
@*LeapYearMonthLengths[1]++;
