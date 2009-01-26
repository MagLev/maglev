require 'spec/helper'

spec_require 'hpricot'

describe 'Dispatcher::Directory' do
  @hierarchy = %w[
  /test/deep/hierarchy/one.txt
  /test/deep/hierarchy/two.txt
  /test/deep/three.txt
  /test/deep/four.txt
  /test/five.txt
  /test/six.txt ]

  @hierarchy.each do |path|
    FileUtils.mkdir_p(__DIR__(:public, File.dirname(path)))
    FileUtils.touch(__DIR__(:public, path))
  end

  def build_listing(path)
    Ramaze::Dispatcher::Directory.build_listing(path)
  end

  def check(url, title, list)
    body, status, header = build_listing(url)
    status.should == 200
    header['Content-Type'].should == 'text/html'

    doc = Hpricot(body)
    doc.at(:title).inner_text.should == title
    doc.search("//td[@class='n']").map{|td|
      a = td.at(:a)
      [ a[:href], a.inner_text ]
    }.should == list
  end

  it 'should dry serve root directory' do
   files = [
     ["/../", "Parent Directory"], ["/test", "test/"],
     ["/favicon.ico", "favicon.ico"], ["/file name.txt", "file name.txt"],
     ["/test_download.css", "test_download.css"]
   ]

    check '/', 'Directory listing of /', files
  end

  it 'should serve hierarchies' do
    files = [
      ["/test/../", "Parent Directory"], ["/test/deep", "deep/"],
      ["/test/five.txt", "five.txt"], ["/test/six.txt", "six.txt"]
    ]
    check '/test', 'Directory listing of /test', files
  end

  FileUtils.rm_rf(__DIR__('public/test'))
end
