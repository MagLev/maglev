module M016
end

class T016
  include M016
end

T016.maglev_persistable(true)
M016.maglev_persistable(true)
Maglev.commit_transaction