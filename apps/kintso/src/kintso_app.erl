%%%-------------------------------------------------------------------
%% @doc kintso public API
%% @end
%%%-------------------------------------------------------------------

-module(kintso_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    create_tables(),
    Dispatch = cowboy_router:compile([
        {'_', [{"/sessions", kintso_sessions_handler, []},
               {"/bookings", kintso_bookings_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(kintso_http_listener, 100,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),
    kintso_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

create_tables() ->
    {atomic, ok} = kintso_session:create_table(),
    {atomic, ok} = kintso_booking:create_table(),
    {atomic, ok} = kintso_movie:create_table().
