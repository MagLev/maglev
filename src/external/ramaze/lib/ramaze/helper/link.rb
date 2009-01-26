#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  # Helper::Link is included into the Controller by default
  #
  # Usage is shown in spec/ramaze/helper/link.rb and the rdocs below.

  module Helper::Link
    # Builds a basic <a> tag.
    #
    # `text` is mandatory, the second hash of options will be transformed into
    # arguments of the tag, :href is a special case and its segments will be
    # escaped.
    #
    # If you pass no :href, the text will be run through Rs and its result is
    # used instead. If you really want an empty href, use :href => ''
    #
    # Usage:
    #   A('text')                       #> <a href="/text">text</a>
    #   A('foo/bar')                    #> <a href="/foo/bar">foo/bar</a>
    #   A('/foo?x=y')                   #> <a href="/foo?x=y">/foo?x=y</a>
    #   A('text', :href => '/foo?x=y')  #> <a href="/foo?x=y">text</a>
    #   A('Home', :href => Rs(:/))      #> <a href="/foo/bar">Home</a>

    def A(*args)
      hash = args.last.respond_to?(:to_hash) ? args.pop : {}

      hash[:href] ||= Rs(*args)
      text = hash.delete(:text) || args.last || hash[:title] || hash[:href]
      hash[:href] = hash[:href].to_s.gsub(/[^\/?;=]+/){|m| Rack::Utils.escape(m) }

      args = ['']
      hash.each {|k,v| args << %(#{k}="#{v}") if k and v }

      %(<a#{args.join(' ')}>#{text}</a>)
    end

    # Builds links out of segments.
    #
    # Pass it strings, symbols, controllers and it will produce a link out of
    # it. Paths to Controllers are obtained from Global.mapping.
    #
    # For brevity, the mapping for the example below is following:
    #   { MC => '/', OC => '/o', ODC => '/od' }
    #
    # Usage:
    #   R(MC) #=> '/'
    #   R(OC) #=> '/o'
    #   R(ODC) #=> '/od'
    #   R(MC, :foo) #=> '/foo'
    #   R(OC, :foo) #=> '/o/foo'
    #   R(ODC, :foo) #=> '/od/foo'
    #   R(MC, :foo, :bar => :x) #=> '/foo?bar=x'

    def R(*atoms)
      args, atoms = atoms.flatten.partition{|a| a.is_a?(Hash) }
      args = args.flatten.inject{|s,v| s.merge!(v) }

      map = Global.mapping.invert

      atoms.map! do |atom|
        if atom.is_a?(Ramaze::Controller)
          map[atom.class]
        else
          map[atom]
        end or atom.to_s
      end

      front = [Global.prefix, *atoms].join('/').squeeze('/')

      if args
        rear = args.inject('?'){|s,(k,v)| s << "#{Rack::Utils.escape(k)}=#{Rack::Utils.escape(v)};"}[0..-2]
        front + rear
      else
        front
      end
    end

    # Uses R with Controller.current as first element.

    def Rs(*atoms)
      atoms.unshift Controller.current unless atoms.first.is_a?(Controller)
      R(*atoms)
    end

    # Give it a path with character to split at and one to join the crumbs with.
    # It will generate a list of links that act as pointers to previous pages on
    # this path.
    #
    # Example:
    #   breadcrumbs('/path/to/somewhere')
    #
    #   # results in this, newlines added for readability:
    #
    #   <a href="/path">path</a>/
    #   <a href="/path/to">to</a>/
    #   <a href="/path/to/somewhere">somewhere</a>
    #
    # Optionally a href prefix can be specified which generate link
    # names a above, but with the prefix prepended to the href path.
    #
    # Example:
    #   breadcrumbs('/path/to/somewhere', '/', '/', '/mycontroller/action')
    #
    #   # results in this, newlines added for readability:
    #
    #   <a href="/mycontroller/action/path">path</a>/
    #   <a href="/mycontroller/action/path/to">to</a>/
    #   <a href="/mycontroller/action/path/to/somewhere">somewhere</a>

    def breadcrumbs(path, split = '/', join = '/', href_prefix = '')
      atoms = path.split(split).reject{|a| a.empty?}
      crumbs = atoms.inject([]){|s,v| s << [s.last,v]}
      bread = crumbs.map do |a|
        href_path = href_prefix + a*'/'
        A(a[-1], :href=>(href_path))
      end
      bread.join(join)
    end
  end
end
