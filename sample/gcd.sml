fun gcd m n =
  if m = 0 then n
  else if m <= n then gcd m (n - m)
  else gcd n (m - n)

val x = gcd 100 125
