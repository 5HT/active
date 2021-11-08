defmodule ACTIVE.Mixfile do
  use Mix.Project

  def project do
    [app: :active,
     version: "7.11.0",
     description: "ACTIVE Continuous Compilation",
     deps: deps(),
     package: package()]
  end

  defp package do
    [name: :active,
     files: ["include", "lib", "src", "LICENSE", "README.md", "rebar.config", "mix.exs"],
     maintainers: ["Vladimir Kirillov", "Namdak Tonpa"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/synrc/active"}]
   end

  def application do
    [mod: {:active_app, []}, applications: [:fs]]
  end

  defp deps do
     [{:ex_doc, "~> 0.11", only: :dev},
      {:fs, "~> 6.1.1"}]
  end
end
