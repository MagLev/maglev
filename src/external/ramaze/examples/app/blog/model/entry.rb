require 'sequel'

DB = Sequel.connect("sqlite:///#{__DIR__}/../blog.db")

class Entry < Sequel::Model(:entry)
  set_schema do
    primary_key :id

    time :created
    time :updated
    text :title
    text :content
  end

  def self.add(title, content)
    create :title => title, :content => content,
      :created => Time.now, :updated => Time.now
  end

  def update(title = title, content = content)
    self.title, self.content, self.updated = title, content, Time.now
    save
  end
end

Entry.create_table! unless Entry.table_exists?

if Entry.empty?
  Entry.add 'Blog created', 'Exciting news today, this blog was created'
end
