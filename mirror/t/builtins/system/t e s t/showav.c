#include <stdio.h>
int
main(int ac, char **av)
{
    int i;
    for (i = 0; i < ac; i++)
	printf("[%s]", av[i]);
    printf("\n");
    return 0;
}
