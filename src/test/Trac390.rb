require 'erb'

template = ERB.new "<%= yield %>"
def get_binding
  binding
end
puts template.result(get_binding { 25 })


