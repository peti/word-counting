Simple Haskell I/O Benchmark
============================

This benchmark compares the performance of different Haskell
implementations of the POSIX `cat` utility against the version shipped
by GNU coreutils.

Running `make test` in this repository gives the following results on my
Linux box:

                       +-----------------------------------------|
                       |             i/o buffer size             |
      |----------------+-------------+--------------+------------|
      |                | 32KB buffer | 128KB buffer | 1GB buffer |
      |----------------+-------------+--------------+------------|
      | cat            |        1.65 |          n/a |        n/a |
      |----------------+-------------+--------------+------------|
      | cat-hgetbuf    |        2.42 |         1.87 |       1.75 |
      | cat-bytestring |        2.39 |         1.87 |       1.75 |
      |----------------+-------------+--------------+------------|

These results suggest that reading/writing data through the Haskell API
is approximately *3 times slower* than doing it through the native API.
