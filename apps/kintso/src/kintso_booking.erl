-module(kintso_booking).

-export([create_table/0]).
-export([create/1]).
-export([get_reserved_seats/2]).

-record(kintso_booking, {
    id :: binary(),
    imdb_id :: binary(),
    screen_id :: binary(),
    available_seats :: integer()
}).

from_map(#{
    <<"imdbId">> := ImdbId,
    <<"screenId">> := ScreenId
}) when
    is_binary(ImdbId),
    is_binary(ScreenId)
->
    #kintso_booking{
        id = generate_id(),
        imdb_id = ImdbId,
        screen_id = ScreenId
    }.

to_map(#kintso_booking{
    imdb_id = ImdbId,
    screen_id = ScreenId
}) ->
    #{
        <<"imdbId">> => ImdbId,
        <<"screenId">> => ScreenId
    }. 
        

create_table() ->
    mnesia:create_table(kintso_booking, [{attributes, record_info(fields, kintso_booking)}]).

create(Map) ->
    #kintso_booking{id = BookingId} = Booking = from_map(Map),
    case mnesia:transaction(fun create_transaction/1, [Booking]) of
        {atomic, ok} ->
            {ok, BookingId};
        {atomic, Error} ->
            Error;
        Error ->
            Error
    end.

create_transaction(#kintso_booking{imdb_id = ImdbId, screen_id = ScreenId} = Booking) ->
    case kintso_session:get_available_seats(ImdbId, ScreenId) of
        {ok, AvailableSeats} ->
            MS = #kintso_booking{imdb_id = ImdbId, screen_id = ScreenId, _ = '_'},
            BookedSeats = mnesia:match_object(MS),
            if
                length(BookedSeats) < AvailableSeats  ->
                    mnesia:write(Booking);
                true ->
                    {error, no_seats}
            end;
        Error ->
            Error
    end.

get_reserved_seats(ImdbId, ScreenId) ->
    MS = #kintso_booking{imdb_id = ImdbId, screen_id = ScreenId, _ = '_'},
    Bookings = mnesia:dirty_match_object(MS),
    length(Bookings).


generate_id() ->
    re:replace(base64:encode(crypto:strong_rand_bytes(10)),"\\W","",[global, {return, binary}]).
