defmodule ACTIVE do

    def compile(mod, path) do
      {status,code,msg} = :active_sh.exe(String.to_charlist("mix compile"))
      :io.format('~s',[msg])
    end

end
