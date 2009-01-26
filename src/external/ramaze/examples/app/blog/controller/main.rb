class MainController < Ramaze::Controller
  layout '/layout'

  def index
    @entries = Entry.order(:created.desc).all
  end

  def delete id
    entry = Entry[id]
    entry.delete
    redirect :/
  end

  def edit id
    @entry = Entry[id]
    redirect_referrer unless @entry
  end

  def create
    Entry.add(*request[:title, :content])
    redirect :/
  end

  def save
    redirect_referer unless  entry = Entry[request[:id]]
    entry.update(*request[:title, :content])
    redirect :/
  end
end
