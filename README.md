ACTIVE
======

Active is sync replacement that uses native FileSystem OS async
listeners to compile and reload Erlang files, DTL templates and
other files. It acts as FS subscriber under supervision and
uses MAD under the hood.

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

    {active, ".*", {git, "git://github.com/synrc/active", "HEAD"}}

NOTE: on Linux please install inotify-tools.

Credits
-------

* Maxim Sokhatsky
* Vladimir Kirillov

OM A HUM
