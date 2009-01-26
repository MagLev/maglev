Ramaze::Route[%r!^/(\d+)\.(?:te?xt|plain|rb|css|js)$!] = '/plain/%d'
Ramaze::Route[%r!^/(\d+)\.(?:html)$!] = '/html/%d'
Ramaze::Route[%r!^/(?:te?xt|plain|rb|css|js)/(\d+)$!] = '/plain/%d'
Ramaze::Route[%r!^/(\d+)\.(\w+)$!] = '/view/%d/%s'
Ramaze::Route[%r!^/(\d+)$!] = '/view/%d/html'
Ramaze::Route[%r!^/list/page/(\d+)$!] = '/list/%d'
# Ramaze::Route[%r!^/list/?(.*)!] = '/%s'

class PasteController < Ramaze::Controller
  map :/
  engine :Ezamar
  helper :formatting, :sequel, :aspect
  layout :layout
  deny_layout :plain, :save_theme

  def list(start = 1)
    ordered = Paste.order(:created.desc)
    @paginated = ordered.paginate(start.to_i, 10)
    @pager = paginator(@paginated, '/list')
    @pastes = @paginated
    @style = style
  end

  def search
    if needle = request['substring'] and not needle.empty?
      limit = 50
      @pastes = Paste.where( "text LIKE '%' || ? || '%'", request[ 'substring' ] ).limit( limit ).order( :created.desc ).all
      @hit_limit = ( @pastes.size == limit )
      @style = session[ :theme ] || STYLE
    end
  end

  def save
    syntax, text = request[:syntax, :text]

    if request.post? and text and Paste::SYNTAX_LIST[syntax]
      paste = Paste.create :syntax => syntax,
        :text => text,
        :created => Time.now
      redirect R(:/, paste.id)
    end

    redirect_referrer
  end

  def copy(id)
    @paste = paste_for(id)
  end

  def view(id, format)
    @paste, @format = paste_for(id), format
    @syntax = @paste.syntax_name
    @formatted = @paste.view(format, style)

    ordered = Paste.order(:created.desc)
    @paginated = ordered.paginate(id.to_i, 1)
    @pager = paginator(@paginated, '/')
  end

  # Do not run through templating

  def plain(id)
    paste = paste_for(id)
    response['Content-Type'] = 'text/plain'
    respond paste.text
  end

  def html(id)
    paste = paste_for(id)
    response['Content-Type'] = 'text/html'
    respond paste.text
  end

  def save_theme( theme_name )
    session[ :theme ] = theme_name
  end

  def diff(from, to)
    paste1, paste2 = Paste[from], Paste[to]
    cs1 = Digest::MD5.hexdigest(paste1.text)
    cs2 = Digest::MD5.hexdigest(paste2.text)
    File.open(f1 = Dir.tmpdir/cs1, 'w+'){|io| io.puts(paste1.text) }
    File.open(f2 = Dir.tmpdir/cs2, 'w+'){|io| io.puts(paste2.text) }
    diff = `diff -up #{f1} #{f2}`.strip
    FileUtils.rm(f1)
    FileUtils.rm(f2)

    Uv.parse(diff, 'xhtml', 'diff', true, style)
  end

  private

  def paste_for(id)
    redirect Rs() unless paste = Paste[:id => id.to_i]
    paste
  end

  def style
    @style ||= session[ :theme ] || STYLE
  end
end
