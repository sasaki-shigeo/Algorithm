#! /usr/local/opt/ruby/bin/ruby

def powmod(x, n, m)
    if n == 0 then
	1
    elsif n % 2 == 0 then
	powmod(x * x % m, n / 2, m)
    else
	powmod(x * x % m, n / 2, m) * x % m
    end
end

def powmod(x, n, m)
    if n == 0 then
	1
    else
	powmod(x, n - 1, m) * x % m
    end
end

puts powmod(2, 0, 1000)
puts powmod(2, 1, 1000)
puts powmod(2, 2, 1000)
puts powmod(2, 3, 1000)
puts powmod(2, 4, 1000)
puts powmod(2, 5, 1000)
puts powmod(2, 6, 1000)
puts powmod(2, 7, 1000)
puts powmod(2, 8, 1000)
puts powmod(2, 9, 1000)
