#include <time.h>
#include <stdio.h>
static FILE* profile_info;
void smop_measure_init(char* file,char* ast_uid) {
  profile_info = fopen(file,"w");
  fprintf(profile_info,"%s\n",ast_uid);
}
void smop_measure_start(int id) {
  struct timespec tp;
  clock_gettime(CLOCK_MONOTONIC,&tp);
  fprintf(profile_info,"s%d:%d:%ld\n",id,(int)tp.tv_sec,tp.tv_nsec);
}
void smop_measure_end(int id) {
  struct timespec tp;
  clock_gettime(CLOCK_MONOTONIC,&tp);
  fprintf(profile_info,"e%d:%d:%ld\n",id,(int)tp.tv_sec,tp.tv_nsec);
}
