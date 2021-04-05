-module(rebar3nix).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3nix_prv:init(State),
    {ok, State1}.
