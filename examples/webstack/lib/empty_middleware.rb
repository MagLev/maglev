# Rack middleware that does nothing.
# Used to measure rack overhead.
class EmptyMiddleware
  def initialize(app)
    @app = app
  end

  def call(env)
    @app.call env
  end
end
