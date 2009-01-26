#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze
  class Controller
    # TODO:
    #   * fix caching, see todos below
    #     FILTER = [ :cached, :default ] unless defined?(FILTER)
    FILTER = [ :default ] unless defined?(FILTER)

    class << self

      # Resolve an absolute path in the application by passing it to each
      # element of Ramaze::Controller::FILTER.
      # If an element does not respond to call it will be sent to self
      # instead, in either case with path as argument.

      def resolve(path, routed = false)
        Thread.current[:routed] = routed

        FILTER.each do |filter|
          answer = if filter.respond_to?(:call)
                     filter.call(path)
                   else
                     send(filter.to_s, path)
                   end
          return answer if answer
        end

        raise_no_filter(path)
      end

      # Default element of FILTER.
      # Looks up the path in Cache.resolved and returns it if found.

      def cached(path)
        if found = Cache.resolved[path]
          if found.respond_to?(:relaxed_hash)
            return found.dup
          else
            Log.warn("Found faulty `#{path}' in Cache.resolved, deleting it for sanity.")
            Cache.resolved.delete path
          end
        end

        nil
      end

      # Default element of FILTER.
      # The default handler that tries to find the best match for the given
      # path in terms of Controller/method/template and given arguments.
      # If a match is found it will be cached for further use.

      def default(path)
        mapping     = Global.mapping
        controllers = Global.controllers

        raise_no_controller(path) if controllers.empty? or mapping.empty?

        # TODO:
        #   * following code is dangerous (DoS):
        #     patterns = Cache.patterns[path] ||= pattern_for(path)

        first_controller = nil

        # Look through the possible ways of interpreting the path until we find
        # one that matches an existing controller action.
        patterns_for(path) do |controller, method, params|
          if controller = mapping[controller]
            first_controller ||= controller

            action = controller.resolve_action(method, *params)
            next unless action

            template = action.template

            valid_action =
              if Action.stack.size > 0 || Global.actionless_templates
                action.method or (params.empty? && template)
              else
                action.method
              end

            if valid_action
              # TODO:
              #   * dangerous as well
              #     Cache.resolved[path] = action
              return action
            end
          end
        end

        if !Thread.current[:routed] and new_path = Route.resolve(path)
          Log.dev("Routing from `#{path}' to `#{new_path}'")
          return resolve(new_path, true)
        end

        raise_no_action(first_controller, path) if first_controller
        raise_no_controller(path)
      end

      # Try to produce an Action from the given path and paremters with the
      # appropiate template if one exists.

      def resolve_action(path, *parameter)
        path, parameter = path.to_s, parameter.map{|e| e.to_s}
        # Use ancestral_trait so if template is set in superclass, it is still found.
        if info = ancestral_trait["#{path}_template"]
          template = info[:file]
          unless template
            controller, action = info.values_at :controller, :action
            # Controller may not have been explicitly set, in which case use self.
            controller ||= self
            template = controller.resolve_template(action)
          end
        end

        method, params = resolve_method(path, *parameter)

        if method or parameter.empty?
          template ||= resolve_template(path)
        end

        action =
          Action.create :path       => path,
                        :method     => method,
                        :params     => params,
                        :template   => template,
                        :controller => self
        return false unless action.valid_rest?
        action
      end

      # Search the #template_paths for a fitting template for path.
      # Only the first found possibility for the generated glob is returned.

      def resolve_template(path)
        path = path.to_s
        path_converted = path.split('__').inject{|s,v| File.join(s, v) }
        possible_paths = [path, path_converted].compact

        paths = template_paths.map{|pa|
          possible_paths.map{|a|
            File.join(pa, a)
          }
        }.flatten.uniq

        glob = "{#{paths.join(',')}}.{#{extension_order.join(',')}}"

        Dir[glob].first
      end

      # Composes an array with the template-paths to look up in the right order.
      # Usually this is composed of Global.view_root and the mapping of the
      # controller.

      def template_paths
        if paths = view_root
          paths
        else
          view_root(File.join(Global.view_root, Global.mapping.invert[self]))
        end
      end

      # Based on methodname and arity, tries to find the right method on
      # current controller.
      def resolve_method(name, *params)
        cam = cached_action_methods

        if cam.include?(name)
          method = name
        else
          name = name.gsub(/__/, '/')
          method = name if cam.include?(name)
        end

        if method
          arity = instance_method(method).arity
          if arity < 0 or params.size == arity
            return method, params
          end
        end

        return nil, []
      end

      # List or create a list of action methods to be cached

      def cached_action_methods
        Cache.action_methods[self] ||= action_methods
      end

      # methodnames that may be used for current controller.
      def action_methods
        ancs = relevant_ancestors + action_modules
        ancs.reverse.inject [] do |meths, anc|
          meths +
            anc.public_instance_methods(false).map{|im| im.to_s } -
            anc.private_instance_methods(false).map{|im| im.to_s }
        end
      end

      # Array of all modules (so including Ramaze helpers) that are included in
      # this controller and where the module is also in the Helper::LOOKUP set.
      # Hence this is the included modules whose public methods may be exposed
      # as actions of this controller.
      def action_modules
        Helper::LOOKUP.find_all {|mod| self.include?(mod)}
      end

      # Iterator that yields potential ways in which a given path could be mapped
      # to controller, action and params. It produces them in strict order, with
      # longest controller path favoured, then longest action path.

      def patterns_for path
        # Split into fragments, and remove empty ones (which split may have output).
        # The to_s is vital as sometimes we are passed an array.
        fragments = path.to_s.split '/'
        fragments.delete ''

        # Work through all the possible splits of controller and 'the rest' (action
        # + params) starting with longest possible controller.
        fragments.length.downto(0) do |ca_split|
          controller = '/' + fragments[0...ca_split].join('/')

          # Work on the remaining portion, generating all the action/params splits.
          fragments.length.downto(ca_split) do |ap_split|
            action = fragments[ca_split...ap_split].join '__'
            params = fragments[ap_split..-1]
            if action.empty?
              yield controller, 'index', params
            else
              yield controller, "#{action}__index", params
              yield controller, action, params
            end
          end
        end
      end

      # Uses custom defined engines and all available engines and throws it
      # against the extensions for the template to find the most likely
      # templating-engine to use ordered by priority and likelyhood.
      def extension_order
        t_extensions = Template::ENGINES
        all_extensions = t_extensions.values.flatten

        if engine = trait[:engine]
          c_extensions = t_extensions.select{|k,v| k == engine}.map{|k,v| v}.flatten
          return (c_extensions + all_extensions).uniq
        end

        all_extensions
      end

      # Raises Ramaze::Error::NoFilter
      # TODO:
      #   * is this called at all for anybody?
      #     I think everybody has filters.

      def raise_no_filter(path)
        raise Ramaze::Error::NoFilter, "No Filter found for `#{path}'"
      end

      # Raises Ramaze::Error::NoController

      def raise_no_controller(path)
        raise Ramaze::Error::NoController, "No Controller found for `#{path}'"
      end

      # Raises Ramaze::Error::NoAction

      def raise_no_action(controller, path)
        STATE[:controller] = controller
        raise Ramaze::Error::NoAction, "No Action found for `#{path}' on #{controller}"
      end
    end
  end
end
