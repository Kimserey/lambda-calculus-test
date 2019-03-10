import sys

def contains(x, xs):
  if not xs: return False
  elif x == xs[0]: return True
  else: return contains(x, xs[1:])

def filter(predicate, xs):
  def inner_filter(xs, res):
    if not xs: 
      return res
    elif predicate(xs[0]):
      return inner_filter(xs[1:], [*res, xs[0]])
    else:
      return inner_filter(xs[1:], res)
  return inner_filter(xs, [])

def any_level(predicate, xs):
  if not xs: return False
  elif predicate(xs[0]): return True
  else: return any_level(predicate, xs[1:])

def is_up(direction):
  return direction == 'up'
def is_down(direction):
  return direction == 'down'

def opposite(direction):
  if is_up(direction): return 'down'
  else: return 'up'

def update_level(direction, level):
  if is_up(direction):
    return level + 1
  else:
    return level - 1

def open_door(level, requests):
  return contains(level, requests)

def continue_same_direction(direction, level, requests):
  if is_up(direction):
    return any_level(lambda l: level < l, requests)
  else:
    return any_level(lambda l: l < level, requests)

requests = [3, 5, 6, 9]

def remove_request(level):
  global requests
  requests = filter(lambda l: l != level, requests)
def get_requests():
  return requests
def simulate_new_request():
  global requests
  new_req = sys.stdin.readline().rstrip("\n")
  if new_req:
    requests.append(int(new_req))

def move_elevator(direction, level):
  current_level = update_level(direction, level)
  requests = get_requests()

  print ('+Direction {}'.format(direction))

  if open_door(current_level, requests):
    print ('+Open {}'.format(current_level))
  else:
    print ('+Skip {}'.format(current_level))

  remove_request(current_level)
  updated_requests = get_requests()

  print('++Requests {}'.format(updated_requests))
  simulate_new_request()
  if not updated_requests:
    return 'IDLE'
  elif continue_same_direction(direction, current_level, updated_requests):
    return move_elevator(direction, current_level)
  else:
    return move_elevator(opposite(direction), current_level)

move_elevator('up', 1)
