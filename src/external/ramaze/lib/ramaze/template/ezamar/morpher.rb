#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

begin
  require 'hpricot'
rescue LoadError => ex
  Ramaze::Log.error "Please install hpricot (for example via `gem install hpricot`) to get morphing"
  puts ex
end

# This applies a morphing-replace for the template.
#
# To use the functionality of Morpher you will need to have hpricot
# installed, you will get one error in case you don't and the method
# will be replaced by a stub that simply returns the template.
#
# The method first checks if you use any morphers and just skips
# the step if you don't, this should give quite some speedup for
# smaller templates that don't use this functionality at all.
# the check works by searching the morphs with appended '='
# in the template. There may be a few cases where this won't work
# since we cannot make any assumptions on the format.
#
# If you want to turn this functionality off, either remove Morpher
# from:
#   Ramaze::Template::Ezamar::TRANSFORM_PIPELINE
# or do:
#   Ramaze::Morpher::MORPHS.clear
#
# The latter is a tad slower, but i mention the possibility in case you
# find good use for it.
#
# You can add your own morphers in Ramaze::Morpher::MORPHS
#
# For Example:
#
#   Morpher::MORPHS['if'] = '<?r %morph %expression ?>%content<?r end ?>'
#
# Now, assuming that some tag in your template is '<a if="@foo">x</a>'
#
# %morph stands for the name of your morph: 'if'
# %expression is the stuff you write in the attribute: '@foo'
# %content is the tag without the attribute (and all inside): '<a>x</a>'

class Ezamar::Morpher

  # Use this trait to define your custom morphs.
  MORPHS = {
            'if'     => '<?r %morph %expression ?>%content<?r end ?>',
            'unless' => '<?r %morph %expression ?>%content<?r end ?>',
            'for'    => '<?r %morph %expression ?>%content<?r end ?>',
            'each'   => '<?r %expression.%morph do |_e| ?>%content<?r end ?>',
            'times'  => '<?r %expression.%morph do |_t| ?>%content<?r end ?>',
  }

  # Since the functionality is best explained by examples, here they come.
  #
  # Example:
  #
  # if:
  #   <div if="@name">#@name</div>
  # morphs to:
  #   <?r if @name ?>
  #     <div>#@name</div>
  #   <?r end ?>
  #
  # unless:
  #   <div unless="@name">No Name</div>
  # morphs to:
  #   <?r unless @name ?>
  #     <div>No Name</div>
  #   <?r end ?>
  #
  # for:
  #   <div for="name in @names">#{name}</div>
  # morphs to:
  #   <?r for name in @names ?>
  #     <div>#{name}</div>
  #   <?r end ?>
  #
  # times:
  #   <div times="3">#{_t}<div>
  # morphs to:
  #   <?r 3.times do |_t| ?>
  #     <div>#{_t}</div>
  #   <?r end ?>
  #
  # each:
  #   <div each="[1,2,3]">#{_e}</div>
  # morphs to:
  #   <?r [1,2,3].each do |_e| ?>
  #     <div>#{_e}</div>
  #   <?r end ?>
  #
  # The latter two examples show you also one standard introduced by a
  # limitation of the replacement-system.
  #
  # When you yield a value, please name it by the first character(s) of
  # the morphs name, with an underscore prefixed.
  #
  # for each an _e, for times a _t.
  #
  # This is by far not the best way to handle it and might lead to problems
  # due to the lack of proper scoping in ruby (if you define an _e or _t
  # before the block it will be overwritten).
  #
  # So please be careful, I tried to come up with something that is both easy
  # to write and doesn't look outright awful while keeping an easy to remember
  # mnemonic.
  #
  # TODO:
  #   - Add pure Ruby implementation as a fall-back.

  def self.transform(template)
    template = template.to_s
    hp = Hpricot(template)

    MORPHS.each do |morph, replacement|
      hp.search("[@#{morph}]") do |elem|
        expr = elem[morph]

        elem.remove_attribute(morph)

        repl = replacement.
          sub('%morph', morph).
          sub('%expression', expr).
          sub('%content', elem.to_html)

        elem.swap(repl)
      end
    end

    hp.to_html
  end
end
