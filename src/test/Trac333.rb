module RbYAML
  @@cvone = 98
  @@tagged_classes = { }

  def self.tag_class(tag, cls)
    unless @@cvone == 98 ; raise 'error' ; end 
    if @@tagged_classes.has_key? tag
      puts "Already tagged"
    else
      @@tagged_classes[tag] = cls
    end
  end
end

class Module
  RbYAML::tag_class "foo", self
end
