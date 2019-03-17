def quicksort(xs):
  def swap_at_index(index, p):
    if xs[p] < xs[index]:
      tmp = xs[index]

      if p - index > 1:
        xs[index] = xs[p - 1]
        xs[p - 1] = xs[p]
        xs[p] = tmp
        # It is possible that the value we swapped with
        # is also larger, therefore we recursively iterate
        # to reapply the swap if necessary.
        return swap_at_index(index, p - 1)
    
      xs[index] = xs[p]
      xs[p] = tmp
      # We are at the end of the possibilities to swap
      # therefore we can directly return.
      return p -1
    
    # Else no swap is needed.
    return p

  if not xs or len(xs) == 1:
    return xs

  i = 0
  pi = len(xs) - 1
  while i < pi:
    pi = swap_at_index(i, pi)
    i += 1

  return [*quicksort(xs[:pi]), xs[pi], *quicksort(xs[(pi + 1):])]