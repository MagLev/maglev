require "m023.rb"

class T023
  redo_include M023_1
  redo_extend M023_2
end

T023.maglev_persistable(true) do |mod| false end
Maglev.commit_transaction