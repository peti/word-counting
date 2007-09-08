#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

enum { number_of_buffers = 1024 * 20, buffer_size = 4096 };

int main(int argc, char** argv)
{
  void *        buffers[ number_of_buffers ];
  size_t        i;
  int           rc;

  srandom(time(NULL));

  for (i = 0u; i != number_of_buffers; ++i)
  {
    if (!(buffers[i] = malloc(buffer_size)))
    {
      perror("malloc() failed");
      return 1;
    }
  }

  for (;;)
  {
    i = random() % number_of_buffers;
    rc = read(STDIN_FILENO, buffers[i], buffer_size);
    if (rc > 0)
    {
      rc -= write(STDOUT_FILENO, buffers[i], rc);
      if (rc != 0)
      {
        perror("write() failed");
        break;
      }
    }
    else
    {
      if (rc < 0) perror("read() failed");
      break;
    }
  }

  return 0;
}
