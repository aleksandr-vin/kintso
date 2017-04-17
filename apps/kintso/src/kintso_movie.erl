-module(kintso_movie).

-export([create_table/0]).
-export([add_movie/2]).
-export([get_title/1]).

-record(kintso_movie, {
    imdb_id :: binary(),
    title :: binary()
}).


create_table() ->
    mnesia:create_table(kintso_movie, [{attributes, record_info(fields, kintso_movie)}]).

add_movie(ImdbId, Title) ->
    Movie = #kintso_movie{imdb_id = ImdbId, title = Title},
    mnesia:dirty_write(Movie).

get_title(ImdbId) ->
    case mnesia:dirty_read(kintso_movie, ImdbId) of
        [#kintso_movie{title = Title}] ->
            Title;
        [] ->
            <<"Unknown">>
    end.
