#!/bin/sh
exec scala $0 $@
!#
for (i <- 1 until 101)
  if ((i % 3 == 0) && (i % 5 == 0)) {
    println("Fizzbuzz");
  } else if (i % 3 == 0) {
    println("Fizz");
  } else if (i % 5 == 0) {
    println("Buzz");
  } else {
    println(i);
  }
