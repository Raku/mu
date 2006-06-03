#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <Judy.h>

//  Compiled:
//  cc -O PrintDupLines.c -lJudy -o PrintDupLines

/* From Judy_HS manual, includes HSIter code */

#define MAXLINE 1000000                 /* max fgets length of line */
uint8_t   Index[MAXLINE];               // string to check

int     // Usage:  PrintDupLines < file
main() {
	Pvoid_t   PJArray = (PWord_t)NULL;  // Judy array.
	PWord_t   PValue;                   // Judy array element pointer.
	Word_t    Bytes;                    // size of JudyHS array.
	Word_t    LineNumb = 0;             // current line number
	Word_t    Dups = 0;                 // number of duplicate lines

	while (fgets(Index, MAXLINE, stdin) != (char *)NULL)
	{
		LineNumb++;                     // line number

		// store string into array
		JHSI(PValue, PJArray, Index, strlen(Index)); 
		if (PValue == PJERR)            // See ERRORS section
		{
			fprintf(stderr, "Out of memory -- exit\n");
			exit(1);
		}
		if (*PValue != 0)               // check if duplicate
		{
			Dups++;
			printf("Duplicate lines %lu:%lu:%s", *PValue, LineNumb, Index);
		}
		else
		{
			*PValue = LineNumb;         // store Line number
		}
	}
	printf("%lu Duplicates, JudyHS array of %lu Lines\n", 
			Dups, LineNumb - Dups);

	// dump lines in hash order
	printf("Lines in hash order:\n");
	{
		Pvoid_t PJHSIter = (Pvoid_t) NULL;  // JudyHS iterator
		uint8_t *Index2 = (uint8_t) NULL;   // JudyHS key: line
		Word_t Length2 = 0;                 // length of key
		PWord_t PValue2;                    // pointer to value
		Word_t IterBytes;                   // size of iterator

		JHSIF(PValue2, PJArray, PJHSIter, Index2, Length2);
		while (PValue2) {
			printf(" line %lu: %*.*s", *PValue2, Index2, Length2, Length2);
			JHSIN(PValue2, PJArray, PJHSIter, Index2, Length2);
		}
		JHSFI(IterBytes, PJHSIter);
		printf("JudyHSFreeIter() freed %lu bytes of memory\n", IterBytes);
	}

	JHSFA(Bytes, PJArray);              // free JudyHS array
	printf("JudyHSFreeArray() freed %lu bytes of memory\n", Bytes);
	return (0);
}
