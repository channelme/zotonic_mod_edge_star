%% @author Maas-Maarten Zeeman <maas@channel.me>
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


-module(m_edge_star).
-author("Maas-Maarten Zeeman <maas@channel.me>").
-behaviour(zotonic_model).

-export([
    m_get/3
]).

-export([
    insert/4, insert/5,
    subject_edge_ids/3,
    object_edge_ids/3,
    objects/3,
    subjects/3,
    get_edge_id/2,
    get_rsc_id/2,
    reify/2,
    install/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

%%
%% Model topic api
%%

m_get([ EdgeId, <<"is_reified">> | Rest ], _Msg, Context) ->
    case get_rsc_id(EdgeId, Context) of
        RscId when is_integer(RscId) ->
            {ok, {true, Rest}};
        _ ->
            {ok, {false, Rest}}
    end;

m_get([ EdgeId, <<"rsc_id">> | Rest ], _Msg, Context) ->
    case get_rsc_id(EdgeId, Context) of
        {error, _} ->
            {error, enoent};
        MaybeRsc ->
            {ok, {MaybeRsc, Rest}}
    end;

m_get([ <<"o">>, TS, TP, TO, Predicate | Rest ], _Msg, Context) ->
    {ok, {object_edge_ids({TS, TP, TO}, Predicate, Context), Rest}};
m_get([ <<"o">>, Id, Predicate | Rest ], _Msg, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true ->
            {ok, {object_edge_ids(Id, Predicate, Context), Rest}};
        false ->
            {error, eacces}
    end;

m_get([ <<"s">>, TS, TP, TO, Predicate | Rest ], _Msg, Context) ->
    {ok, {subject_edge_ids({TS, TP, TO}, Predicate, Context), Rest}};
m_get([ <<"s">>, Id, Predicate | Rest ], _Msg, Context) ->
    case z_acl:rsc_visible(Id, Context) of
        true ->
            {ok, {subject_edge_ids(Id, Predicate, Context), Rest}};
        false ->
            {error, eacces}
    end;

m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.

%%
%% Api
%%

object_edge_ids({_, _, _}=Triple, Pred, Context) ->
    case get_rsc_id(Triple, Context) of
        Object when is_integer(Object) ->
            [ maybe_expand_triple(RscId, EdgeId, Context) || {RscId, EdgeId} <- m_edge:object_edge_ids(Object, Pred, Context) ];
        undefined ->
            []
    end;
object_edge_ids(Id, Pred, Context) ->
    [ maybe_expand_triple(RscId, EdgeId, Context) || {RscId, EdgeId} <- m_edge:object_edge_ids(Id, Pred, Context) ].

subject_edge_ids({_, _, _}=Triple, Pred, Context) ->
    case get_rsc_id(Triple, Context) of
        Object when is_integer(Object) ->
            [ maybe_expand_triple(RscId, EdgeId, Context) || {RscId, EdgeId} <- m_edge:subject_edge_ids(Object, Pred, Context) ];
        undefined ->
            []
    end;
subject_edge_ids(Id, Pred, Context) ->
    [ maybe_expand_triple(RscId, EdgeId, Context) || {RscId, EdgeId} <- m_edge:subject_edge_ids(Id, Pred, Context) ].

subjects({_, _, _}, undefined, _Context) ->
    [];
subjects({_, _, _}=Triple, Predicate, Context) ->
    case get_rsc_id(Triple, Context) of
        Id when is_integer(Id) ->
            m_rsc:subjects(Id, Predicate, Context);
        undefined ->
            []
    end.

objects({_, _, _}, undefined, _Context) ->
    [];
objects({_, _, _}=Triple, Predicate, Context) ->
    case get_rsc_id(Triple, Context) of
        Id when is_integer(Id) ->
            m_rsc:objects(Id, Predicate, Context);
        undefined ->
            []
    end.

insert(Subject, Predicate, Object, Context) ->
    insert(Subject, Predicate, Object, [], Context).
insert(Subject, Predicate, {S, P, O}, Opts, Context) when is_tuple(O) ->
    case get_rsc_id(O, Context) of
        undefined -> {error, enoent};
        Id -> insert(Subject, Predicate, {S, P, Id}, Opts, Context)
    end;
insert(Subject, Predicate, {S, P, O}, Opts, Context) when is_integer(O) ->
    insert(Subject, Predicate, {edge, m_edge:get_id(S, P, O, Context)}, Opts, Context);
insert(_Subject, _Predicate, {edge, undefined}, _Opts, _Context) ->
    {error, enoent};
insert(Subject, Predicate, {edge, EdgeId}, Opts, Context) ->
    T = fun(Ctx) ->
                case reify(EdgeId, Ctx) of 
                    {ok, ReifiedEdgeId} ->
                        insert(Subject, Predicate, ReifiedEdgeId, Opts, Ctx);
                    {error, _}=Error ->
                        Error
                end
        end,
    case z_db:transaction(T, z_acl:sudo(Context)) of
        {ok, NewEdgeId} -> {ok, NewEdgeId};
        {error, _}=Error -> Error
    end;
insert(Subject, Predicate, Object, Opts, Context) when is_integer(Object) ->
    m_edge:insert(Subject, Predicate, Object, Opts, Context).

reify(EdgeId, Context) ->
    T = fun(Ctx) ->
                case get_rsc_id_raw(EdgeId, Ctx) of
                    Id when is_integer(Id) ->
                        {ok, Id};
                    undefined ->
                        case m_edge:get_triple(EdgeId, Ctx) of
                            {_, _, _} = Edge ->
                                Title = make_title(Edge, Ctx),
                                Body = make_body(EdgeId, Edge, Ctx),
                                SystemContentGroupId = m_rsc:rid(system_content_group, Ctx),

                                {ok, Id} = m_rsc:insert(#{ title => Title,
                                                           body => Body,
                                                           category => edge_resource,
                                                           is_published => true,
                                                           content_group_id => SystemContentGroupId,
                                                           edge_id => EdgeId,
                                                           name => <<"edge_", (z_convert:to_binary(EdgeId))/binary>>
                                                         },
                                                        Ctx),

                                1 = z_db:q("INSERT INTO edge_star (edge_id, rsc_id) VALUES ($1, $2)", [EdgeId, Id], Ctx),
                                {ok, Id};
                            undefined ->
                                {error, enoent}
                        end
                end
        end,

    case z_db:transaction(T, z_acl:sudo(Context)) of
        {ok, RscId} ->
            z_depcache:flush(RscId, Context),
            z_depcache:flush({edge_star, EdgeId}, Context),
            {ok, RscId};
        {error, _}=Error ->
            Error
    end.

get_rsc_id({Subject, Predicate, Object}, Context) when is_integer(Object) ->
    get_rsc_id(m_edge:get_id(Subject, Predicate, Object, Context), Context);
get_rsc_id({Subject, Predicate, Object}, Context) when is_tuple(Object) ->
    get_rsc_id({Subject, Predicate, get_rsc_id(Object, Context)}, Context);
get_rsc_id(undefined, _Context) ->
    undefined;
get_rsc_id(EdgeId, Context) ->
    Key = {edge_star, rsc_id, EdgeId},
    case z_depcache:get(Key, Context) of
        {ok, Value} ->
            Value;
        undefined ->
            case get_rsc_id_raw(EdgeId, Context) of
                undefined ->
                    _ = z_depcache:set(Key, undefined, ?DAY, [{edge_star, EdgeId}], Context),
                    undefined;
                RscId when is_integer(RscId) ->
                    _ = z_depcache:set(Key, RscId, ?DAY, [RscId, {edge_star, EdgeId}], Context),
                    RscId
            end
    end.

get_edge_id(RscId, Context) ->
    Key = {edge_star, edge_id, RscId},
    case z_depcache:get(Key, Context) of
        {ok, Value} ->
            Value;
        undefined ->
            case get_edge_id_raw(RscId, Context) of
                undefined ->
                    _ = z_depcache:set(Key, undefined, ?DAY, [ RscId ], Context),
                    undefined;
                EdgeId when is_integer(EdgeId) ->
                    _ = z_depcache:set(Key, EdgeId, ?DAY, [ RscId, {edge_star, EdgeId} ], Context)),
                    EdgeId
            end
    end.

install(Context) ->
    case z_db:table_exists(edge_star, Context) of
        false ->
            ok = z_db:create_table(
                   edge_star,
                   [#column_def{name=edge_id, type="integer", is_nullable=false, primary_key=true},
                    #column_def{name=rsc_id, type="integer", unique=true, is_nullable=false},
                    #column_def{name=created, type="timestamp with time zone", is_nullable=false, default="now()"}
                   ], Context),

            {ok, _, _} = z_db:equery("ALTER TABLE
                                          edge_star
                                     ADD CONSTRAINT
                                         fk_edge_star_edge_id
                                         FOREIGN KEY (edge_id)
                                         REFERENCES edge(id)
                                     ON UPDATE CASCADE
                                     ON DELETE CASCADE", Context),
            {ok, _, _} = z_db:equery("ALTER TABLE
                                         edge_star
                                     ADD CONSTRAINT
                                         fk_edge_star_rsc_id
                                         FOREIGN KEY (rsc_id)
                                         REFERENCES rsc(id)
                                     ON UPDATE CASCADE
                                     ON DELETE CASCADE", Context),

            z_db:flush(Context),

            ok;
        true ->
            ok
    end.

%%
%% Helpers
%%

get_edge_id_raw(RscId, Context) ->
    z_db:q1("SELECT edge_id FROM edge_star WHERE rsc_id = $1", [RscId], Context).

get_rsc_id_raw(EdgeId, Context) ->
    z_db:q1("SELECT rsc_id FROM edge_star WHERE edge_id = $1", [EdgeId], Context).

maybe_expand_triple(Id, EdgeId, Context) ->
    case m_rsc:is_a(Id, edge_resource, Context) of
        true ->
            Triple = {_S, _P, _O} = m_edge:get_triple(get_edge_id(Id, Context), Context),
            {Triple, EdgeId};
        false ->
            {Id, EdgeId}
    end.

make_title({Subject, Predicate, Object}, Context) ->
    SubjectTitle = resource_title(Subject, Context),
    PredicateName = z_convert:to_binary(Predicate),
    ObjectTitle = resource_title(Object, Context),
    z_convert:to_binary([ SubjectTitle, <<" →  "/utf8>>, PredicateName, <<" → "/utf8>>, ObjectTitle ]).

make_body(EdgeId, {Subject, Predicate, Object}, Context) ->
    SubjectTitle = resource_title(Subject, Context),
    PredicateName = z_convert:to_binary(Predicate),
    ObjectTitle = resource_title(Object, Context),
    
    iolist_to_binary([
        <<"<p>This resource represents the relationship: <strong>">>,
        z_html:escape(SubjectTitle),
        <<"</strong> ">>,
        z_html:escape(PredicateName),
        <<" <strong>">>,
        z_html:escape(ObjectTitle),
        <<"</strong></p>\n">>,
        <<"<p>Edge ID: ">>, integer_to_binary(EdgeId), <<"</p>">>
    ]).

resource_title(Id, Context) ->
    case m_rsc:p(Id, title, Context) of
        undefined ->
            <<"Resource ", (integer_to_binary(Id))/binary>>;
        T ->
            z_trans:trans(T, Context)
    end.

