%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2025 Maas-Maarten Zeeman
%% @doc Edge star model for linking to edges.

%% Copyright 2025 Maas-Maarten Zeeman 
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


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

%% [TODO] needs some to be able to use in templates
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
