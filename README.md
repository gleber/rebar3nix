rebar3nix
=====

Export rebar3 dependencies for use in nix derivations

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3nix, {git, "https://github.com/dlesl/rebar3nix.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 nix
    ===> Fetching rebar3nix
    ===> Compiling rebar3nix
    <Plugin Output>

The plugin will write your dependencies to rebar.nix in a nix-consumable format.
