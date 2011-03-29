require 'webtools/ruby'

module WebTools
  # The CodeBrowser is a ViewModel for MagLev code browsing.  It keeps the
  # entire state of the UI and returns structured data for use by a UI.
  class CodeBrowser
    def state
      { 'classNames' => WebTools::Ruby.class_and_module_names }
    end
  end
end
