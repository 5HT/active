defmodule Active.Mixfile do
  use Mix.Project

  def project do
    [app: :active,
     version: "1.9.0",
     description: "Erlang active reloader",
     package: package,
     deps: deps]
  end

  defp package do
    [files: ~w(src LICENSE package.exs README.md rebar.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/active"}]
  end

  defp deps do [
    {:mad,                          github: "synrc/mad"},
    {:fs,                           github: "synrc/fs"},
  ]
  end
end
