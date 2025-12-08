
-module(m_edge_star).

-export([install/1]).

install(Context) ->
    case z_db:table_exists(edge_star, Context) of
        false ->
            %% Messages
            ok = z_db:create_table(
                   edge_star,
                   [#column_def{name=edge_id, type="integer", is_nullable=false, primary_key=true},
                    #column_def{name=rsc_id, type="integer", is_nullable=false},
                    #column_def{name=created, type="timestamp with time zone", is_nullable=false, default="now()"}
                   ], Context),

            % Constraints
            {ok, _, _} = z_db:equery("alter table edge_star add constraint fk_edge_star_edge_id foreign key (edge_id) references edge(id) on update cascade on delete cascade", Context),
            {ok, _, _} = z_db:equery("alter table edge_star add constraint fk_edge_star_rsc_id foreign key (rsc_id) references rsc(id) on update cascade on delete cascade", Context),

            % Indices
            {ok, _, _} = z_db:equery("create index fki_edge_star_rsc_id on (rsc_id)", Context),

            z_db:flush(Context),

            ok;
        true ->
            ok
    end.

