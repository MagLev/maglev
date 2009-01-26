#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'ramaze/store/default'

TodoList = Ramaze::Store::Default.new('todolist.db')

{
  'Laundry'     => {:done => false},
  'Wash dishes' => {:done => false},

}.each do |title, value|
  TodoList[title] = value
end
