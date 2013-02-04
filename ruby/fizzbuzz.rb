#!/usr/bin/env ruby

fizzbuzz = Proc.new do |i|
  if i % 3 == 0 and i % 5 == 0
    "Fizzbuzz"
  elsif i % 3 == 0
    "Fizz"
  elsif i % 5 == 0
    "Buzz"
  else
    i.to_s
  end
end

(1..100).map(&fizzbuzz).each { |i| print i + "\n" }
