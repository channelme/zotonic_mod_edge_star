

-module(mod_edge_star).
-mod_title("Edge Star").
-mod_description("RDF-star inspired reify edges as resources to enable discussion and metadata about relationships").
-mod_prio(300).
-mod_schema(1).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    manage_schema/2,
    update_datamodel/1
]).

manage_schema(_Version, Context) ->
    m_edge_star:install(Context),
    ok.

update_datamodel(Context) ->
    z_datamodel:manage(?MODULE, datamodel(), Context).

datamodel() ->
    #datamodel{
       categories = [
                     {edge_resource, meta, [{title, <<"Edge Reesource">>},
                                            {summary, <<"A reified edge, a resource representing a relationship between two other resources">>}
                                           ]}
                    ]
      }.

