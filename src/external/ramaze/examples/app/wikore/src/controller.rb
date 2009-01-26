class Ramaze::Controller
  Page = Model::Page
  OldPage = Model::OldPage
end

class MainController < Ramaze::Controller
  def index(title = 'Main', version = nil)
    query = {:title => title}
    query[:version] = version.to_i if version
    model = version ? OldPage : Page

    if page = model[query]
      @text, @version = page.text, page.version
    else
      @text = "No Page known as '#{title}'"
      @version = false
    end
  end
end

class PageController < Ramaze::Controller
  map '/page'

  helper :aspect

  def create
    change "Created Page '%s'" do |title, text|
      Page.create(:title => title, :text => text, :version => 1)
      redirect R(MainController, title)
    end
  end

  def save
    change "Updated Page '%s'" do |title, text|
      page = Page[:title => title]
      page.backup
      page.text = text
      page.version += 1
      page.save
      redirect R(MainController, title)
    end
  end

  def delete(title)
    page = Page[:title => title]
    page.backup
    page.delete
  end

  def rename(title, to)
    change("Renamed #{title} to '%s'", to) do |title, text|
      page = Page[:title => title]
      page.backup
      page.title = to
      page.version += 1
      page.save
    end
  end

  def revert(title)
    page = Page[:title => title]
    page.revert
    redirect R(MainController, title)
  end

  before :create, :save do
    redirect_referer unless request.post?
  end

  private

  def change(message, redirect_to = '/')
    if title = request['title'] and text = request['text']
      yield(title, text)
      message % title
    end

    redirect(R(MainController, redirect_to))
  end
end
