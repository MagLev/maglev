#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  class Dispatcher

    # Generates a directory listing, see Ramaze::Controller::Directory for more
    # information and how to create your own directory listing page
    class Directory
      class << self

        # Entry point from Dispatcher::filter.
        # Just a forwarder to build_listing, automatticly exiting if there is
        # an error (defined by returning false in build_listing)
        def call(path)
          if Global.list_directories
            response = build_listing(path)
            Response.current.build(*response) if response
          end
        end

        # Makes a request for http://yourserver/dirlist/path and returns the
        # result. Due to this method, you can overwrite the action and create your
        # own page. See Ramaze::Controller::Directory for more.
        def build_listing(path)
          dir = ::File.expand_path(Global.public_root/::File.expand_path(path, '/'))

          if ::File.directory?(dir)
            Log.debug("Serving directory listing: #{dir}")
            Session.current.drop! if Session.current

            status = Ramaze::STATUS_CODE['OK']
            header = {'Content-Type' => "text/html"}
            body = list_for(dir)
            [body, status, header]
          end
        end

        # Nicely formatted listing for given path, template is "stolen" from
        # lighttpd
        def list_for(path)
          root = ::File.expand_path(Global.public_root)
          display = path.gsub(/^#{Regexp.escape(root)}\/?/, '/')
          wrapper =
%(<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN"
"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
  <head>
    <title>Directory listing of #{display}</title>

    <style type="text/css"> /*<![CDATA[*/ a, a:active {text-decoration: none; color: blue;} a:visited {color: #48468F;} a:hover, a:focus {text-decoration: underline; color: red;} body {background-color: #F5F5F5;} h2 {margin-bottom: 12px;} table {margin-left: 12px;} th, td { font-family: "Courier New", Courier, monospace; font-size: 10pt; text-align: left;} th { font-weight: bold; padding-right: 14px; padding-bottom: 3px;} td {padding-right: 14px;} td.s, th.s {text-align: right;} div.list { background-color: white; border-top: 1px solid #646464; border-bottom: 1px solid #646464; padding-top: 10px; padding-bottom: 14px;} div.foot { font-family: "Courier New", Courier, monospace; font-size: 10pt; color: #787878; padding-top: 4px;} /*]]>*/ </style>
  </head>
  <body>
    <h2>Directory listing of #{display}</h2>
    <div class="list">
      <table summary="Directory Listing" cellpadding="0" cellspacing="0">
        <thead>
          <tr>
            <th class="n">Name</th>
            <th class="m">Last Modified</th>
            <th class="s">Size</th>
            <th class="t">Type</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td class="n"><a href="#{display/'..'}/">Parent Directory</a>/</td>
            <td class="m">&nbsp;</td>
            <td class="s">- &nbsp;</td>
            <td class="t">Directory</td>
          </tr>
          %s
          %s
        </tbody>
      </table>
    </div>
    <div class="foot">%s</div>
  </body>
</html>
)

          dirs, files = Dir[path/'*'].partition{|file| ::File.directory?(file) }
          dir_body, file_body = [], []

          dirs.sort.each do |dir|
            basename = ::File.basename(dir)
            dir_body << %[<tr>
            <td class="n"><a href="#{display/basename}">#{basename}/</a></td>
            <td class="m">&nbsp;</td>
            <td class="s">- &nbsp;</td>
            <td class="t">Directory</td>
          </tr>]
          end

          time_format = "%Y-%b-%d %H:%M:%S"
          files.sort.each do |file|
            basename = ::File.basename(file)
            time = ::File.mtime(file).strftime(time_format)
            size = ::File.size(file).filesize_format
            mime = Tool::MIME.type_for(file)
            file_body << %[<tr>
            <td class="n"><a href="#{display/basename}">#{basename}</a></td>
            <td class="m">#{time}</td>
            <td class="s">#{size}</td>
            <td class="t">#{mime}</td>
          </tr>]
          end

          version = "ramaze/#{Ramaze::VERSION}"
          wrapper % [
            dir_body. join("\n          "),
            file_body.join("\n          "),
            version
          ]
        end
      end
    end
  end
end
