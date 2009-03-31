
def get_binding
  binding
end
b = get_binding { 25 }
ra = eval(' yield ', b)
unless ra == 25 ; raise 'error'; end

rb = eval(' block_given? ', b)
unless rb == true ; raise 'error'; end

c = get_binding

rc = eval(' block_given? ', c)
unless rc == false ; raise 'error'; end
true

