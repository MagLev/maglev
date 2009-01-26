#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

$KCODE = 'UTF-8'
require 'ya2yaml'

# Localize helps transforming arbitrary text into localized forms using
# a simple regular expression and substituting occurences with predefined
# snippets stored in YAML files.
#
# == Usage:
#
#   Ramaze::Dispatcher::Action::FILTER << Ramaze::Tool::Localize

module Ramaze
  module Tool
    class Localize

      # Enable Localization
      trait :enable => true

      # Default language that is used if the browser don't suggests otherwise or
      # the language requested is not available.
      trait :default_language => 'en'

      # languages supported
      trait :languages => %w[ en ]

      # This block should respond with the correct location of the yaml file
      # for the given locale.
      # Alternative is setting the value as a string that used by String#% with
      # the locale as argument.
      # The YAML file is used for saving and loading the localization.
      #
      # Example:
      #   trait :file => "locale/%s.yaml"
      #
      # Please note that giving a relative String can lead to issues when the
      # application is started from another pwd.
      # That is why we, by default, use a dynamic lambda and set the path
      # relative to Global.root.
      trait :file => lambda{|locale|
        Ramaze::Global.root/"conf/locale_#{locale}.yaml"
      }

      # The pattern that is substituted with the translation of the current locale.
      trait :regex => /\[\[(.*?)\]\]/

      # Browsers may send different keys for the same language, this allows you to
      # do some coercion between what you use as keys and what the browser sends.
      trait :mapping => { /^en-/ => 'en', 'ja' => 'jp'}

      # When this is set to false, it will not save newly collected translatable
      # strings to disk.  Disable this for production use, as it slows the
      # application down.
      trait :collect => true

      class << self

        include Trinity

        # Enables being plugged into Dispatcher::Action::FILTER

        def call(response, options = {})
          return response unless trait[:enable]

          body = []

          response.body.each do |chunk|
            body << localize_body(chunk, options)
          end

          response.body = body
        end

        # Localizes a response body.  It reacts to a regular expression as given
        # in trait[:regex].  Every 'entity' in it will be translated, see
        # `localize` for more information.

        def localize_body(body, options)
          locale =
            if languages.include?(response["Content-Language"])
              response["Content-Language"]
            else
              (session[:LOCALE] || set_session_locale).to_s
            end

          body.gsub!(trait[:regex]) do
            localize($1, locale) unless $1.to_s.empty?
          end

          store(locale, default_language) if trait[:collect]

          body
        end

        # Localizes a single 'entity'.  If a translation in the chosen language is
        # not available, it falls back to the default language.

        def localize(str, locale)
          dict = dictionary
          dict[locale] ||= {}
          dict[default_language] ||= {}

          trans = dict[locale][str] ||= dict[default_language][str] ||= str
        rescue Object => ex
          Log.error(ex)
          str
        end

        # Sets session[:LOCALE] to one of the languages defined in the dictionary.
        # It first tries to honor the browsers accepted languages and then falls
        # back to the default language.

        def set_session_locale
          session[:LOCALE] = default_language
          accepted_langs = request.locales << default_language

          mapping = trait[:mapping]
          dict = dictionary

          accepted_langs.each do |language|
            if mapped = mapping.find{|k,v| k === language }
              language = mapped[1]
            end

            if dict.key?(language)
              session[:LOCALE] = language
              break
            end
          end

          session[:LOCALE]
        end

        # Returns the dictionary used for translation.

        def dictionary
          trait[:languages].map! {|x| x.to_s }.uniq!
          trait[:dictionary] || load(*languages)
        end

        # Load given locales from disk and save it into the dictionary.

        def load(*locales)
          Log.debug "loading locales: #{locales.inspect}"

          dict = trait[:dictionary] || {}

          locales.each do |locale|
            begin
              dict[locale] = YAML.load_file(file_for(locale))
            rescue Errno::ENOENT
              dict[locale] = {}
            end
          end

          trait[:dictionary] = dict
        end

        # Stores given locales from the dictionary to disk.

        def store(*locales)
          locales.uniq.compact.each do |locale|
            file = file_for(locale)
            data = dictionary[locale].ya2yaml

            Log.dev "saving localized to: #{file}"
            File.open(file, 'w+'){|fd| fd << data }
          end
        rescue Errno::ENOENT => e
          Log.error e
        end

        # Call trait[:file] with the passed locale if it reponds to that,
        # otherwise we call #to_s and % with the locale on it.

        def file_for(locale)
          file_source = trait[:file]

          if file_source.respond_to?(:call)
            file = file_source.call(locale)
          else
            file = file_source.to_s % locale
          end
        end

        # alias for trait[:languages]

        def languages
          trait[:languages]
        end

        # alias for trait[:default_language]

        def default_language
          trait[:default_language]
        end
      end
    end
  end
end
