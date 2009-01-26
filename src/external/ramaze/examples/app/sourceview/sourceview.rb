require 'rubygems'
require 'remarkably/engines/html'
require 'coderay'
require 'ramaze'

# where is the source
RAMAZE_SRC = File.expand_path(Ramaze::BASEDIR/'../') unless defined? RAMAZE_SRC

# delete cached filetree when source changes
module Ramaze::SourceReloadHooks
  module_function
  def after_safe_load file
    Ramaze::Cache.actions.clear
  end
end

class MainController < Ramaze::Controller

  include Remarkably::Common
  helper :cache, :aspect
  engine :None

  def index *args
    redirect "/#/#{args.join('/')}" if args.size > 0
  end

  def source *args
    file = args.join('/')
    return if file.empty? or file =~ /\.{2}/

    file[0,0] = RAMAZE_SRC + '/'
    CodeRay.scan_file(file).html(:line_numbers => :table) if FileTest.file? file
  end
  before(:source){ %(<link href='/coderay.css' rel='stylesheet' type='text/css' />) unless request.xhr? }

  def filetree
    ul :class => 'filetree treeview' do
      Dir.chdir(RAMAZE_SRC) do
        Dir['{benchmarks,doc,examples,lib,spec}'].collect do |d|
          dir_listing d
        end
      end
    end.to_s
  end
  cache :filetree

  private

  def dir_listing dir
    li do
      span dir, :class => 'folder'
      Dir.chdir(dir) do
        ul :style => 'display: none;' do
          a '', :href => "##{File.expand_path('.').sub(RAMAZE_SRC,'')}"
          Dir['*'].sort.each do |d|
            if FileTest.directory? d
              dir_listing d
            else
              file = File.expand_path(d).sub(RAMAZE_SRC,'')
              li do
                span :class => 'file' do
                  a d, :href => "##{file}"
                end
              end
            end
          end
        end if Dir['*'].any?
      end
    end
  end

end

Ramaze.start :adapter      => :mongrel,
             :load_engines => :Haml,
             :boring       => /(js|gif|css)$/,
             :port         => 9950
