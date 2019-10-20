defmodule ACTIVE do

    def compile(mod, path) do
      ACTIVE.compile(mod, path, false)
    end

    def compile(_, path, recompile) do
      case IEx.Helpers.c(to_string(path), :in_memory) do
        [] -> IO.puts("active_ex: Didn't compile #{path}")
              :ignore
        modules ->
           IO.inspect modules
           Enum.each(modules, fn(mod) ->
           case recompile do
                true -> IEx.Helpers.r(mod)
                false -> IEx.Helpers.l(mod)
            end
          end)
      end
    end

end
