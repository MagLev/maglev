def generate_gemspec(version)
  Gem::Specification.new{|s|
    s.name = "ramaze"
    s.version = version
    s.summary = "Ramaze is a simple and modular web framework"
    s.description = s.summary

    s.author = "Michael 'manveru' Fellinger"
    s.email = "m.fellinger@gmail.com"
    s.homepage = "http://ramaze.rubyforge.org"
    s.rubyforge_project = "ramaze"

    s.bindir = "bin"
    s.require_path = "lib"

    s.executables = Dir["#{s.bindir}/*"].map{|f| File.basename(f) }
    s.files = FileList.new('**/*'){|fl|
      fl.exclude(/^pkg\//)
      fl.exclude(/^tags$/)
    }.sort

    s.platform = Gem::Platform::RUBY
    s.has_rdoc = true
    s.post_install_message = POST_INSTALL_MESSAGE

    s.add_dependency('rack', '>=0.3.0')
  }
end

def update_gemspec(spec)
  gemspec = <<-OUT.strip
Gem::Specification.new do |s|
  s.name = %name%
  s.version = %version%

  s.summary = %summary%
  s.description = %description%
  s.platform = %platform%
  s.has_rdoc = %has_rdoc%
  s.author = %author%
  s.email = %email%
  s.homepage = %homepage%
  s.executables = %executables%
  s.bindir = %bindir%
  s.require_path = %require_path%
  s.post_install_message = %post_install_message%

  %dependencies%

  s.files = %files%
end
  OUT

  gemspec.gsub!(/%(\w+)%/) do
    case key = $1
    when 'version'
      spec.version.to_s.dump
    when 'dependencies'
      spec.dependencies.map{|dep|
        "s.add_dependency('#{dep.name}', '#{dep.version_requirements}')"
      }.join("\n  ")
    else
      spec.send($1).pretty_inspect.strip
    end
  end

  File.open("ramaze.gemspec", 'w+'){|file| file.puts(gemspec) }
end

spec = generate_gemspec(version_today)
Rake::GemPackageTask.new(spec) do |pkg|
  pkg.need_zip = true
  pkg.need_tar = true
end
