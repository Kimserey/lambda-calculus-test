class MyStack:
  """
    Stack implementation with:
    - Push
    - Pop
  """ 
  def __init__(self, initial):
    self.stack = [initial]

  def push(self, value):
    self.stack = [value, *self.stack]
  
  def pop(self):
    if not self.stack:
      return False
    
    val = self.stack[0]
    self.stack = self.stack[1:]
    return val


class MyQueue:
  """
    Queue implementation with:
    - Enqueue
    - Dequeue
  """ 
  def __init__(self):
    self.queue = []

  def __append(self, x, ls):
    """Append item at the end of a list."""
    if not ls:
      return [x]
    
    return [ls[0], *(self.__append(x, ls[1:]))]

  def enqueue(self, x):
    self.queue = self.__append(x, self.queue)
  
  def dequeue(self):
    if not self.queue:
      return False

    val = self.queue[0]
    self.queue = self.queue[1:]
    return val


class OrderedSet:
  """
    Ordered Set implementation with:
    - Adjoin, adjoin an element to the list in order
  """ 
  def __init__(self):
    self.content = []

  def __adjoin(self, x, xs):
    """Adjoin an element x to a list xs."""
    if not xs:
      return [x]
    
    if xs[0] == x:
      return xs
    
    if x < xs[0]:
      return [x, *xs]
    
    return [xs[0], *(self.__adjoin(x, xs[1:]))] 

  def adjoin(self, x):
    self.content = self.__adjoin(x, self.content)


class Map:
  """
    Map implementation with:
    - Add
    - Lookup
  """
  def __init__(self):
    self.content = []

  def __lookup(self, key, ls):
    """
      Looks up for an element with 
      the corresponding key in ls. 
    """
    if not ls:
      return False

    if key == ls[0][0]:
      return ls[0][1]

    return self.__lookup(key, ls[1:])

  def lookup(self, key):
    return self.__lookup(key, self.content)
  
  def add(self, key, value):
    if self.lookup(key):
      return False
    
    self.content = [(key, value), *self.content]

class BinaryTree:
  """
    Binary Tree implementation with:
    - Insert, insert an element by comparing with node value
    - Contain, contain a value
  """
  def __init__(self):
    self.content = []

  def __insert(self, x, tree):
    if not tree:
      return (x, None, None)
    
    if x < tree[0]:
      if not tree[1]:
        return (tree[0], (x, None, None), tree[2])
      return (tree[0], self.__insert(x, tree[1]), tree[2])
    
    if tree[0] < x:
      if not tree[2]:
        return (tree[0], tree[1], (x, None, None))
      return (tree[0], tree[1], self.__insert(x, tree[2]))
    
    return tree
  
  def insert(self, x):
    self.content = self.__insert(x, self.content)

  def __contain(self, x, tree):
    if not tree:
      return False

    if x < tree[0]:
      if not tree[1]:
        return False
      return self.__contain(x, tree[1])

    if tree[0] < x:
      if not tree[2]:
        return False
      return self.__contain(x, tree[2])
    
    return True

  def contain(self, x):
    return self.__contain(x, self.content)
