defmodule ACTIVE.Mixfile do
  use Mix.Project

  def project do
    [app: :active,
     version: "5.10.1",
     description: "ACTIVE Runtime Auto-recompilation",
     deps: deps,
     docs: [],
     package: package]
  end

  defp package do
    [name: :active,
     files: ["include", "lib", "src", "LICENSE", "README.md", "rebar.config", "mix.exs"],
     maintainers: ["Vladimir Kirillov", "Namdak Tonpa"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/fs"}]
   end

  def application do
    [mod: {:active_app, []}, applications: [:fs]]
  end

  defp deps do
     [{:ex_doc, "~> 0.11", only: :dev},
      {:fs, "~> 4.10.1"}]
  end
end
