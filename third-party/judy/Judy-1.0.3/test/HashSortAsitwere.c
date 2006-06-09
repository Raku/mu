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
	PPvoid_t  PPJ1Array;                // pointer to Judy1 array
	Word_t    J1Value;                  // same line and line number seen before?
	
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
		PPJ1Array = PValue;         // in the value cell we have a Judy1 bitset
		if (*PPJ1Array != NULL)               // check if duplicate
		{
			Dups++;
			printf("Duplicate line: %lu:%s", LineNumb, Index);
		}
		J1S(J1Value, *PPJ1Array, LineNumb);         // store Line number
		if (J1Value == 0)
		{
			printf("Same line seen twice?!: %lu:%s", LineNumb, Index);
		}
		
	}
	printf("%lu Duplicates, JudyHS array of %lu Lines\n", 
			Dups, LineNumb - Dups);

	// dump lines in hash order
	printf("Lines in hash order:\n");
	{
		Pvoid_t PJHSIter = (Pvoid_t) NULL;  // JudyHS iterator
		uint8_t *Index2 = (uint8_t) NULL;   // JudyHS key: line
		Word_t Length2 = 0;                 // length of key: start at 0
		PWord_t PValue2;                    // pointer to value
		Word_t IterBytes;                   // size of iterator

		JHSIF(PValue2, PJArray, PJHSIter, Index2, Length2);
		while (PValue2)
		{
			PPvoid_t PPJ1Array2 = PValue2;  // points to bitset of line numbers
			Word_t LineNumb2 = 0;
			Word_t J1Value2;
			
			J1F(J1Value2, *PPJ1Array2, LineNumb2);  // walk bitset
			while (J1Value2 == 1)
			{
				printf(" line %lu: %*.*s", LineNumb2, Length2, Length2, Index2);
				J1N(J1Value2, *PPJ1Array2, LineNumb2);
			}
			JHSIN(PValue2, PJArray, PJHSIter, Index2, Length2);
		}
		JHSFI(IterBytes, PJHSIter);
		printf("JudyHSFreeIter() freed %lu bytes of memory\n", IterBytes);
	}

	// dump lines in reverse hash order
	printf("Lines in reverse hash order:\n");
	{
		Pvoid_t PJHSIter = (Pvoid_t) NULL;  // JudyHS iterator
		uint8_t *Index2 = (uint8_t) NULL;   // JudyHS key: line
		Word_t Length2 = -1;                // length of key: start at maxuint
		PWord_t PValue2;                    // pointer to value
		Word_t IterBytes;                   // size of iterator

		JHSIL(PValue2, PJArray, PJHSIter, Index2, Length2);
		while (PValue2)
		{
			PPvoid_t PPJ1Array2 = PValue2;  // points to bitset of line numbers
			Word_t LineNumb2 = -1;
			Word_t J1Value2;
			
			J1L(J1Value2, *PPJ1Array2, LineNumb2);  // walk bitset
			while (J1Value2 == 1)
			{
				printf(" line %lu: %*.*s", LineNumb2, Length2, Length2, Index2);
				J1P(J1Value2, *PPJ1Array2, LineNumb2);
			}
			JHSIP(PValue2, PJArray, PJHSIter, Index2, Length2);
		}
		JHSFI(IterBytes, PJHSIter);
		printf("JudyHSFreeIter() freed %lu bytes of memory\n", IterBytes);
	}

	// free everything
	{       // free each line number bitset
		Pvoid_t PJHSIter = (Pvoid_t) NULL;  // JudyHS iterator
		uint8_t *Index2 = (uint8_t) NULL;   // JudyHS key
		Word_t Length2 = 0;                 // and length: walk forward
		PWord_t PValue2;                    // pointer to value...
		Word_t J1TotalBytes = 0;            // size of all bitsets
		Word_t IterBytes;                   // size of iterator

		JHSIF(PValue2, PJArray, PJHSIter, Index2, Length2);
		while (PValue2)
		{
			PPvoid_t PPJ1Array2 = PValue2;  // value points to bitset
			Word_t J1Bytes;             // size of this bitset
			
			J1FA(J1Bytes, *PPJ1Array2);
			J1TotalBytes += J1Bytes;
			
			JHSIN(PValue2, PJArray, PJHSIter, Index2, Length2);
		}
		printf("Freeing Judy1 bitsets freed %lu bytes of memory\n", J1TotalBytes);
		JHSFI(IterBytes, PJHSIter);
		printf("JudyHSFreeIter() freed %lu bytes of memory\n", IterBytes);
	}
	JHSFA(Bytes, PJArray);              // free JudyHS array
	printf("JudyHSFreeArray() freed %lu bytes of memory\n", Bytes);
	return (0);
}
