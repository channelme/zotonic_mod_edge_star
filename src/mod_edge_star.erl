%% @author Maas-Maarten Zeeman <maas@channel.me>
%% @copyright 2025 Maas-Maarten Zeeman
%% @doc RDF-star (1.2) inspired module which allows linking to edges.

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


-module(mod_edge_star).
-mod_title("Edge Star").
-mod_description("RDF-star inspired reify edges as resources to enable discussion and metadata about relationships").
-mod_prio(2).
-mod_schema(1).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    manage_schema/2,
    update_datamodel/1
]).

manage_schema(_Version, Context) ->
    update_datamodel(Context),
    m_edge_star:install(Context),
    ok.

update_datamodel(Context) ->
    z_datamodel:manage(?MODULE, datamodel(), Context).

datamodel() ->
    #datamodel{
       categories = [
                     {edge_resource, meta, [{title, <<"Edge Resource">>},
                                            {summary, <<"A reified edge, a resource representing a relationship between two other resources">>}
                                           ]}
                    ]
      }.

