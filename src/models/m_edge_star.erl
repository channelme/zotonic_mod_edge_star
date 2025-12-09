
-module(m_edge_star).

-export([
    insert/4, insert/5,
    reify/2,
    install/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

insert(Subject, Predicate, Object, Context) ->
    insert(Subject, Predicate, Object, [], Context).

insert(Subject, Predicate, {edge, EdgeId}, Opts, Context) ->
    T = fun(Ctx) ->
                case reify(EdgeId, Ctx) of 
                    {ok, ReifiedEdgeId} ->
                        insert(Subject, Predicate, ReifiedEdgeId, Opts, Ctx);
                    {error, _}=Error ->
                        Error
                end
        end,
    case z_db:transaction(T, Context) of
        {ok, EdgeId} ->
            {ok, EdgeId};
        {error, _}=Error ->
            Error
    end;
insert(Subject, Predicate, Object, Opts, Context) when is_integer(Object) ->
    m_edge:insert(Subject, Predicate, Object, Opts, Context).

reify(EdgeId, Context) ->
    T = fun(Ctx) ->
                case get_reified(EdgeId, Ctx) of
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
        {ok, Id} ->
            Id;
        {error, _}=Error ->
            Error
    end.

get_reified(EdgeId, Context) ->
    z_db:q1("SELECT
                 rsc_id
            FROM
                 edge_star
            WHERE
                edge_id = $1", [EdgeId], Context).

install(Context) ->
    case z_db:table_exists(edge_star, Context) of
        false ->
            ok = z_db:create_table(
                   edge_star,
                   [#column_def{name=edge_id, type="integer", is_nullable=false, primary_key=true},
                    #column_def{name=rsc_id, type="integer", unique=true, is_nullable=false},
                    #column_def{name=created, type="timestamp with time zone", is_nullable=false, default="now()"}
                   ], Context),

            {ok, _, _} = z_db:equery("alter table edge_star add constraint fk_edge_star_edge_id foreign key (edge_id) references edge(id) on update cascade on delete cascade", Context),
            {ok, _, _} = z_db:equery("alter table edge_star add constraint fk_edge_star_rsc_id foreign key (rsc_id) references rsc(id) on update cascade on delete cascade", Context),

            z_db:flush(Context),

            ok;
        true ->
            ok
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

