def numer(x):
  return x[0]

def denom(x):
  return x[1]

def gcd(a, b):
  rest = a%b
  if rest == 0:
    return b
  else:
    return gcd(b, rest)

def make_rat(n, d):
  g = gcd(n, d)
  n = n/g
  d = d/g
  return (n, d)