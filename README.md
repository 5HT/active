ACTIVE: Continuous Compilation
==============================

[![Actions Status](https://github.com/synrc/active/workflows/mix/badge.svg)](https://github.com/synrc/active/actions)
[![Build Status](https://travis-ci.com/synrc/active.svg?branch=master)](https://travis-ci.com/synrc/active)
[![Hex pm](http://img.shields.io/hexpm/v/active.svg?style=flat)](https://hex.pm/packages/active)

Active is a [sync](https://github.com/rustyio/sync) replacement
that uses native file-system OS async listeners to compile and
reload Erlang files, DTL templates and other files. It acts as
FS subscriber under supervision and uses
[mad](https://github.com/synrc/mad),
`IEx.Helpers.c` or you can add your own compiler, like `rebar3` or `make`.

Listen Folders
--------------

### One-level

```erlang
app(App,["ebin",Module|_]) -> load_ebin(App,Module);
app(App,["priv"|_]) -> compile(App);
app(App,["src"|_]) -> compile(App);
app(_,_)-> ok.
```

### Two-level

```erlang
otp(["deps",App|Rest]) -> app(App,Rest);
otp(["apps",App|Rest]) -> app(App,Rest);
otp([Some|Path]) -> app(top(),[Some|Path]);
otp(_) -> ok.
```

Usage
-----

On Mac/Linux/Windows just include into your rebar.config:

    {active, ".*", {git, "git://github.com/synrc/active", {tag,"1.9"}}}

NOTE: on Linux please install inotify-tools.

Credits
-------

* Maxim Sokhatsky
* Vladimir Kirillov
* Oleksandr Palchikovsky (Elixir)

OM A HUM
