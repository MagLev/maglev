#          Copyright (c) 2006 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

# Default url mappings are:
#  a controller called Main is mapped on the root of the site: /
#  a controller called Something is mapped on: /something
# If you want to override this, add a line like this inside the class
#  map '/otherurl'
# this will force the controller to be mounted on: /otherurl

class MainController < Ramaze::Controller

  def index
    @tasks = []
    TodoList.original.each do |title, value|
      if value[:done]
        status = 'done'
        toggle = A('Open Task', :href => Rs(:open, title))
      else
        status = 'not done'
        toggle = A('Close Task', :href => Rs(:close, title))
      end
      delete = A('Delete', :href => Rs(:delete, title))
      @tasks << [title, status, toggle, delete]
    end
    @tasks.sort!
  end

  def create
    if title = request['title']
      title.strip!
      if title.empty?
        failed("Please enter a title")
        redirect '/new'
      end
      TodoList[title] = {:done => false}
    end
  end

  def open title
    task_status title, false
  end

  def close title
    task_status title, true
  end

  def delete title
    TodoList.delete title
  end

  helper :aspect
  after(:create, :open, :close, :delete){ redirect(Rs()) unless redirected? }

  private

  def failed(message)
    flash[:error] = message
  end

  def task_status title, status
    unless task = TodoList[title]
      failed "No such Task: `#{title}'"
      redirect_referer
    end

    task[:done] = status
    TodoList[title] = task
  end
end
