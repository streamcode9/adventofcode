-module(aoc).
-export([start/0]).

start() -> 
  {ok, Data} = file:read_file(<<"1.txt">>),
  Strings = lists:map(fun(X) -> string:trim(X) end, binary:split(Data, [<<"\n">>], [global])),
  DrawStr = <<"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1">>,
  DrawStrs = binary:split(DrawStr, [<<",">>], [global]),
  Draw = map_to_integer(DrawStrs),
  MatrixesList = file_to_matrixes(Strings),
  Matrixes = matrixes(MatrixesList, []),
  play(Draw, prepare(Matrixes)).

map_to_integer(List) -> lists:map(fun(X) -> {I, _} = string:to_integer(X), I end, List).

file_to_matrixes(Strings) -> lists:map(fun(X) -> map_to_integer(remove_empty_strings(string:split(X, " ", all))) end, Strings).

remove_empty_strings(List) -> lists:filter(fun(X) -> case string:length(X) == 0 of true -> false; false -> true end end, List).

matrixes([],                 Res) -> Res;
matrixes([H1,H2,H3,H4,H5|T], Res) -> matrixes(T, lists:append(Res, [[H1,H2,H3,H4,H5]])).

map_matrixes(F, Matrixes) ->
  lists:map(fun(M) ->
    lists:map(fun(Row) ->
      lists:map(fun(X) -> F(X) end, Row)
    end, M)
  end, Matrixes).

prepare(M) -> map_matrixes(fun(X) -> {X, false} end, M).

mark(N, {X,Marker}) -> case N == X of true -> {X,true}; false -> {X, Marker} end.

check_rows(Matrix) ->
  lists:any(fun(Row) -> lists:all(fun({_,X}) -> X == true end, Row) end, Matrix).

check_column(I, Matrix) ->
  lists:all(fun(Row) ->
    {_, X} = lists:nth(I, Row),
    X == true
  end, Matrix).

check_columns(Matrix) ->
  check_column(1, Matrix) or
  check_column(2, Matrix) or
  check_column(3, Matrix) or
  check_column(4, Matrix) or
  check_column(5, Matrix).

check_matrix(Matrix) ->
  RowsWin = check_rows(Matrix),
  ColsWin = check_columns(Matrix),
  RowsWin or ColsWin.

find_winner(Matrixes) ->
  lists:map(fun(X) ->
    HasWinner = check_matrix(X),
    {HasWinner, X}
  end, Matrixes).

sum(Matrix) -> lists:foldl(
  fun(Row, Acc) ->
    Acc + lists:foldl(fun({Num, IsMarked}, AccRow) ->
      case IsMarked of
        true -> AccRow;
        false -> AccRow + Num
      end
    end, 0, Row)
  end, 0, Matrix).

play([],           _) -> [];
play([H|T], Matrixes) ->
  MarkedMatrixes = map_matrixes(fun(X) -> mark(H, X) end, Matrixes),
  CheckedMatrixes = find_winner(MarkedMatrixes),
  Winners = lists:filter(fun({X,_}) -> X == true end, CheckedMatrixes),
  case length(Winners) > 0 of
    true ->
      Sums = lists:map(fun({IsW,M}) -> {IsW, sum(M) * H} end, CheckedMatrixes),
      io:format("~p ~p~n", [H, Sums]),
      case length(Winners) == length(Matrixes) of
        true -> [];
        false -> play(T, MarkedMatrixes)
      end;
    false -> play(T, MarkedMatrixes)
  end.
