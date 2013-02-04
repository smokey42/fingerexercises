#!/usr/bin/env ruby

File.open("input.txt").read.split.inject({}) { |hash, word|
  hash[word] ||= 0
  hash[word] += 1
  hash
}.sort_by { |word, count|
  -count
}.each { |word, count|
  print "%s, %s\n" % [word, count]
}
