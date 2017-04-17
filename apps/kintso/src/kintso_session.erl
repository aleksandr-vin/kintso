-module(kintso_session).

-export([create_table/0]).
-export([create_session/1]).
-export([get_available_seats/2]).
-export([get_session_info/2]).

-record(kintso_session, {
    id :: binary(),
    imdb_id :: binary(),
    screen_id :: binary(),
    available_seats :: integer()
}).

from_map(#{
    <<"imdbId">> := ImdbId,
    <<"screenId">> := ScreenId,
    <<"availableSeats">> := AvailableSeats
}) when
    is_binary(ImdbId),
    is_binary(ScreenId),
    is_integer(AvailableSeats)
->
    #kintso_session{
        id = generate_id(ImdbId, ScreenId),
        imdb_id = ImdbId,
        screen_id = ScreenId,
        available_seats = AvailableSeats
    }.

to_map(#kintso_session{
    imdb_id = ImdbId,
    screen_id = ScreenId,
    available_seats = AvailableSeats
}) ->
    #{
        <<"imdbId">> => ImdbId,
        <<"screenId">> => ScreenId,
        <<"availableSeats">> => AvailableSeats,
        <<"reservedSeats">> => kintso_booking:get_reserved_seats(ImdbId, ScreenId),
        <<"movieTitle">> => kintso_movie:get_title(ImdbId)
    }. 
        

create_table() ->
    mnesia:create_table(kintso_session, [{attributes, record_info(fields, kintso_session)}]).

create_session(Map) ->
    #kintso_session{id = SessionId} = Session = from_map(Map),
    case mnesia:transaction(fun create_transaction/1, [Session]) of
        {atomic, ok} ->
            {ok, SessionId};
        {atomic, Error} ->
            Error;
        Error ->
            Error
    end.

create_transaction(#kintso_session{id = Id} = Session) ->
    case mnesia:read(kintso_session, Id) of
        [] ->
            ok = mnesia:write(Session);
        [_ExistingSession] ->
            {error, already_exists}
    end.

get_session_info(ImdbId, ScreenId) ->
    get_session_info(generate_id(ImdbId, ScreenId)).

get_session_info(SessionId) ->
    case mnesia:dirty_read(kintso_session, SessionId) of
        [Session] ->
            Map = to_map(Session),
            {ok, Map};
        [] ->
            {error, session_not_found}
    end.
    

get_available_seats(ImdbId, ScreenId) ->
    case mnesia:read(kintso_session, generate_id(ImdbId, ScreenId)) of
        [#kintso_session{available_seats = Seats}] ->
            {ok, Seats};
        [] ->
            {error, session_not_found}
    end.

generate_id(ImdbId, ScreenId) ->
    <<ImdbId/binary, ScreenId/binary>>.
