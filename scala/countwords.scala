#!/bin/sh
exec scala $0 $@
!#
import scala.collection.mutable.HashMap

val wordcount = new HashMap[String, Int]() {
    override def default(key: String) = 0
}

val whitespace = """\r|\n|\t|\s"""
val input = scala.io.Source.fromFile("input.txt").getLines

for (chunk <- input)
    for (word <- chunk.split(whitespace))
        wordcount(word) += 1;

for ((word, count) <- wordcount.toSeq.sortBy(entry => -entry._2))
    println(word + ", " + count)
