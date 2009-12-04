# directory access
# list all files but .*/*~/*.o

[10000].map do |n|
  n.times do
    dirp = Dir.open(".")
    for f in dirp
      case f
      when /^\./, /~$/, /\.o/
        # do not print
      else
        # don't print since we don't care about print speed
        #print f, "\n"
      end
    end
    dirp.close
  end
end
