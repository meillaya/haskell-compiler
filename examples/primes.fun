-- The lazy prime sieve from the Write You a Haskell example set.
sieve [] = []
sieve (p:ps) = p : sieve (filter (nonMultiple p) ps)

nonMultiple p n = ((n / p) * p) /= n

primes = sieve [2..]

main = take 10 primes
