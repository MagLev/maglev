# This file defines the blog classes.  To commit the code, load this
# file from within a Maglev.persistent block and then commit it.

require 'maglev_model'

class SimplePost
  include Maglev::Model

  attr_reader :text, :title, :timestamp, :tags
  def initialize(params)
    @title = params[:title]
    @text =  params[:text]
    @timestamp = Time.now
    @tags = []
  end

  # Tag the post: (a) adds reciever to the tag and (b) adds
  # each tag to recevier's @tags
  def tag(*tags)
    p tags
    tags.each do |tag|
      tag << self
      @tags << tag
    end
  end
end

class SimpleTag < Array
  include Maglev::Model

  attr_reader :name

  def initialize(name)
    @name = name.to_s
  end

  def to_s
    @name
  end

  def self.find_by_name(name)
    SimpleTag.detect { |t| t.name == name }
  end
end
