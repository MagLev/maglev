class Env
    def [](val)
        val
    end
end

ENV = Env.new