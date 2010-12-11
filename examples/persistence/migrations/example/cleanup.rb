# Remove all traces of our blog post from the repository

Maglev::PERSISTENT_ROOT[:BlogPost] = nil

if defined? BlogPost
  Maglev.persistent do
    Object.remove_const(:BlogPost)
  end
end

Maglev.commit_transaction


