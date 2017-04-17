-module(kintso_bookings_handler).

-export([init/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([allowed_methods/2]).
-export([accept_json/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, prov_json}
    ], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, accept_json}
    ], Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

accept_json(Req, State) ->
    {ok, Json, Req1} = cowboy_req:read_body(Req),
    Booking = jsx:decode(Json, [return_maps]),
    case kintso_booking:create(Booking) of
        {ok, BookingId} ->
            {{true, <<"/bookings/", BookingId/binary>>}, Req1, State};
        Error ->
            lager:info("Booking error: ~p", [Error]),
            {false, Req1, State}
    end.
