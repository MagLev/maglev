BASEDIR = File.expand_path(File.join(File.dirname(__FILE__), '..'))

VERS = Ramaze::VERSION
COPYRIGHT = [
  "#          Copyright (c) #{Time.now.year} Michael Fellinger m.fellinger@gmail.com",
  "# All files in this distribution are subject to the terms of the Ruby license."
]
README = 'README.markdown'
CLEAN.include %w[
  **/.*.sw?
  *.gem
  .config
  **/*~
  **/{data.db,cache.yaml}
  *.yaml
  pkg
  rdoc
]
RDOC_OPTS = %W[
  --all
  --quiet
  --op rdoc
  --line-numbers
  --inline-source
  --main #{README}
  --opname index.html
  --title "Ramaze\ documentation"
  --exclude "^(spec|examples|bin|pkg)/"
  --exclude "lib/proto"
  --include "doc"
  --accessor "trait"
]
RDOC_FILES = %W[
  lib doc #{README} doc/FAQ doc/CHANGELOG
]
POST_INSTALL_MESSAGE = %{
#{'=' * 60}

Thank you for installing Ramaze!
You can now do following:

* Create a new project using the `ramaze' command:
    ramaze --create yourproject

#{'=' * 60}
}.strip

AUTHOR_MAP = {
  'andy@tinnedfruit.org'                    => 'Andy Smith',
  'ahoward'                                 => 'Ara T. Howard',
  'ara.t.howard@gmail.com'                  => 'Ara T. Howard',
  'blueonyx@dev-area.net'                   => 'Martin Hilbig',
  'clive@crous.co.za'                       => 'Clive Crous',
  'comp.lang.zenix+ramaze@gmail.com'        => 'Colin Shea',
  'evaryont@gmx.us'                         => 'Colin Shea',
  'jesusisramazing.10.pistos@geoshell.com'  => 'Pistos',
  'jesuswasramazing.10.pistos@geoshell.com' => 'Pistos',
  'keita.yamaguchi@gmail.com'               => 'Keita Yamaguchi',
  'leo.borisenko@gmail.com'                 => 'Leo Borisenko',
  'manveru@weez-int.com'                    => 'Michael Fellinger',
  'm.fellinger@gmail.com'                   => 'Michael Fellinger',
  'outtenr@gmail.com'                       => 'Richard Outten',
  'ryan@wonko.com'                          => 'Ryan Grove',
  'rff.rff@gmail.com'                       => 'Gabriele Renzi',
  'skaar@waste.org'                         => 'skaar',
  'stephan@spaceboyz.net'                   => 'Stephan Maka',
  'samcarr@gmail.com'                       => 'samcarr',
}

# * Browse and try the Examples in
#     #{File.join(Gem.path, 'gems', 'ramaze-' + VERS, 'examples')}
