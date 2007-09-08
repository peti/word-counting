#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

enum { buffer_size = 4096 };

int main(int argc, char** argv)
{
  void *        p;
  int           rc;

  for (;;)
  {
    p = malloc(buffer_size);
    if (!p)
    {
      perror("malloc() failed");
      return 1;
    }

    rc = read(STDIN_FILENO, p, buffer_size);
    if (rc > 0)
    {
      rc -= write(STDOUT_FILENO, p, rc);
      if (rc != 0)
      {
        perror("write() failed");
        return 1;
      }
    }
    else if (rc < 0)
    {
      perror("read() failed");
      return 1;
    }
    else
      break;

    //free(p);
  }

  return 0;
}
