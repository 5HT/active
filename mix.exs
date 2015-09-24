defmodule Active.Mixfile do
  use Mix.Project

  def project do
    [app: :active,
     version: "1.9",
     description: "Erlang active reloader",
     package: package]
  end

  defp package do
    [files: ~w(src LICENSE package.exs README.md rebar.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/active"}]
   end
end
