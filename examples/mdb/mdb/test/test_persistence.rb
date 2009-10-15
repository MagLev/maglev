
ENV['MAGLEV_OPTS'] = '-MnoRlwrap'

Dir.chdir File.dirname(__FILE__) do
  system 'maglev-ruby p1.rb'
  system 'maglev-ruby p2.rb'
end

