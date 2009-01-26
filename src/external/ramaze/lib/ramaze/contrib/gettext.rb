#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'ramaze/tool/localize'
require 'ramaze/contrib/gettext/mo'
require 'ramaze/contrib/gettext/po'

# Gettext helps transforming arbitrary text into localized forms using
# a simple regular expression and substituting occurences with translations
# stored in .mo files.
#
# == MO generation
#
# See http://www.gnu.org/software/gettext/ for a general overview over
# Gettext.  Generally it's easier to use a graphical translator like Poedit.
#
# The default language is en, a .po template file will be saved by default
# under `conf/locale_en.mo.pot`.  Individual languages are by default looked
# up at `conf/locale_fi.mo` for localization.
#
# == Usage:
#
#   Ramaze::Dispatcher::Action::FILTER << Ramaze::Tool::Gettext

module Ramaze
  class Tool::Gettext < Tool::Localize

    # Enable Localization
    trait :enable => true

    # Default language that is used if the browser don't suggests otherwise or
    # the language requested is not available.
    trait :default_language => 'en'

    # languages supported
    trait :languages => %w[ en ]

    # YAML files the localizations are saved to and loaded from, %s is
    # substituted by the values from trait[:languages]
    trait :file => 'conf/locale_%s.mo'.freeze

    # The pattern that is substituted with the translation of the current locale.
    trait :regex => /\[\[(.*?)\]\]/

    # Browsers may send different keys for the same language, this allows you to
    # do some coercion between what you use as keys and what the browser sends.
    trait :mapping => { 'en-us' => 'en', 'ja' => 'jp'}

    # When this is set to false, it will not save newly collected translatable
    # strings to disk.  Disable this for production use, as it slows the
    # application down.
    trait :collect => true


    # Load given locales from disk and save it into the dictionary.

    def self.load(*locales)
      Log.debug "loading locales: #{locales.inspect}"

      dict = trait[:dictionary] || {}

      locales.each do |locale|
        begin
          dict[locale] = ::MOFile.open(trait[:file] % locale)
        rescue Errno::ENOENT
          Log.error "couldn't load #{trait[:file] % locale}"
          dict[locale] = {}
        end
      end

      trait[:dictionary] = dict
    end

    # Reloads given locales from the disk to refresh the dictionary.

    def self.update
      trait[:dictionary] = nil
      dictionary.each do |locale, dict|
        if dict.kind_of?(MOFile)
          Log.debug("Reloading #{dict.filename}")
          dict.update!
        end
      end
    end

    # Stores given locales from the dictionary to disk.

    def self.store(*locales)
      keys = []
      dictionary.each do |locale, dict|
        keys.concat dict.keys
      end
      keys.delete ""

      data = ::GetText::RGetText.generate(keys.compact.uniq.sort.map {|x| [x] })
      file = (trait[:file] % trait[:default_language]) + '.pot'
      File.open(file, File::CREAT|File::TRUNC|File::WRONLY) do |fd|
        fd.write data
      end
    rescue Errno::ENOENT => e
      Log.error e
    end
  end
end

class Ramaze::Contrib::Gettext

  # Called by Ramaze::Contrib.load, adds Gettext to Action::Filter

  def self.startup
    Ramaze::Dispatcher::Action::FILTER << Ramaze::Tool::Gettext
  end
end
