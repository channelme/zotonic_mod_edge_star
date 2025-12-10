
-module(m_rsc_star).
-author("Maas-Maarten Zeeman <maas@channel.me>").
-behaviour(zotonic_model).

-export([
    m_get/3
]).

-export([
    o/3,
    s/3
]).

m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

%%
%% Api
%%

%% @doc Return the list of objects with a certain predicate
o({_,_,_}=Triple, Predicate, Context) ->
    m_edge_star:objects(Triple, Predicate, Context);
o(Id, Predicate, Context) ->
    m_rsc:o(Id, Predicate, Context).

%% @doc Return the list of subjects with a certain predicate
s({_,_,_}=Triple, Predicate, Context) ->
    m_edge_star:subjects(Triple, Predicate, Context);
s(Id, Predicate, Context) ->
    m_rsc:s(Id, Predicate, Context).
