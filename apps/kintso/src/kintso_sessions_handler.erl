-module(kintso_sessions_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([provide_json/2]).
-export([accept_json/2]).

init(Req, _State) ->
    {cowboy_rest, Req, #{}}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, provide_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, accept_json}
    ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

resource_exists(#{method := <<"GET">>} = Req, State) ->
    #{imdbId := ImdbId, screenId := ScreenId} = cowboy_req:match_qs([imdbId, screenId], Req),
    case kintso_session:get_session_info(ImdbId, ScreenId) of
        {ok, _SessionInfo} ->
            {true, Req, State};
        {error, session_not_found} ->
            {false, Req, State}
    end;
resource_exists(#{method := <<"POST">>} = Req, State) ->
    {ok, Json, Req1} = cowboy_req:read_body(Req),
    #{<<"imdbId">> := ImdbId, <<"screenId">> := ScreenId} = Session = jsx:decode(Json, [return_maps]),
    Exists = case kintso_session:get_session_info(ImdbId, ScreenId) of
        {ok, SessionInfo} ->
            true;
        {error, session_not_found} ->
            false
    end,
    {Exists, Req1, State#{session => Session}}.

provide_json(Req, State) ->
    #{imdbId := ImdbId, screenId := ScreenId} = cowboy_req:match_qs([imdbId, screenId], Req),
    {ok, SessionInfo} = kintso_session:get_session_info(ImdbId, ScreenId),
    Json = jsx:encode(SessionInfo),
    {Json, Req, State}.

accept_json(Req, #{session := Session} = State) ->
    case kintso_session:create_session(Session) of
        {ok, SessionId} ->
            {{true, <<"/sessions/", SessionId/binary>>}, Req, State};
        Error ->
            lager:info("Add session error: ~p", [Error]),
            {false, Req, State}
    end.
