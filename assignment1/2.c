#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* 2 - a command to connect two programs by a bidirectional pipe */
/* Author: Thomas Hallgren, hallgren@cs.chalmers.se */

/* Compile with gcc -o 2 2.c */

void startcommand(char *cmd,int left[2],int right[2])
{
  close(0);
  dup(left[0]);
  close(1);
  dup(right[1]);
  close(left[0]);
  close(left[1]);
  close(right[0]);
  close(right[1]);
  fprintf(stderr,"Starting %s\n",cmd);
  execl("/bin/sh","sh","-c",cmd,NULL);
  fprintf(stderr,"Failed starting %s\n",cmd);
  exit(1);
}

void main(int argc,char *argv[])
{
  int left[2],right[2];
  int pid;

  if(argc!=3) {
    fprintf(stderr,"Usage: %s cmd1 cmd2\n",argv[0]);
    exit(1);
  }

  pipe(left);
  pipe(right);
  switch(fork()) {
    case -1: perror("fork"); exit(1); break;
    case 0: startcommand(argv[1],left,right); break;
    default: startcommand(argv[2],right,left); break;
  }
}
