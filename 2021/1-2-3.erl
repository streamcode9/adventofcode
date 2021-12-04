% Mix of functions to solve first 3 days
% file name must be the same as module name
% powerShell>& 'C:\Program Files\erl-24.1\bin\erlc.exe' .\aoc.erl
% powerShell>& 'C:\Program Files\erl-24.1\bin\erl.exe' -noshell -s aoc start -s init stop

-module(aoc).
-export([start/0]).

%log([A,B,C | T]) -> io:format("~p ~p ~p | ~p~n", [A, B, C, T]).

start() -> 
  {ok, Data} = file:read_file(<<"1.txt">>),
  Strings = lists:map(fun(X) -> string:trim(X) end, binary:split(Data, [<<"\n">>], [global])),
  Len = string:length(lists:nth(1, Strings)),
  Tuples = lists:map(fun(X) -> str_to_tuples(X, 0, Len, []) end, Strings),
  %Totals = calc(Tuples, []),
  CO2 = co2(Tuples, 1, fun(X) -> often_bit(X) end),
  CO2Answer = list_to_bit(CO2, Len-1, 0),
  io:format("Answer: ~p~n", [CO2Answer]),

  CO3 = co2(Tuples, 1, fun(X) -> seldom_bit(X) end),
  CO3Answer = list_to_bit(CO3, Len-1, 0),
  io:format("Answer: ~p~n", [CO3Answer]),

  io:format("Total Answer: ~p~n", [CO2Answer * CO3Answer]).

  %GammaBits = gamma(Totals),
  %Gamma = list_to_bit(GammaBits, Len-1, 0),
  %EpsilonBits = epsilon(Totals),
  %Epsilon = list_to_bit(EpsilonBits, Len-1, 0),
  %io:format("gamma ~p, epsilon ~p ==> ~p ~n", [Gamma, Epsilon, Gamma*Epsilon]).

% 3
list_to_bit([],      _, Res) -> Res;
list_to_bit([H|T], Pow, Res) -> list_to_bit(T, Pow-1, Res + math:pow(2,Pow) * H).

seldom_bit({Zero,One}) when One < Zero -> 1;
seldom_bit(_)                          -> 0.

often_bit({Zero,One}) when Zero > One -> 0;
often_bit(_)                          -> 1.

co2_filter(List, FilterParam, I) ->
  lists:filter(fun(X) ->
    case lists:nth(I, X) == FilterParam of
      true -> true;
      false -> false
    end
  end, List).

co2([R],  _,       _) -> R;
co2(List, I, CompFun) ->
  %io:format("~p ~p~n", [I, List]),
  Totals = calc(List, []),
  %io:format("~p~n", [Totals]),
  I_Total = lists:nth(I, Totals),
  %io:format("~p~n", [I_Total]),
  FilterParam = CompFun(I_Total),
  %io:format("~p~n", [FilterParam]),
  Res = co2_filter(List, FilterParam, I),
  %io:format("~p~n", [Res]),
  %io:format("--- ~n", []),
  co2(Res, I+1, CompFun).

%epsilon(ListOfTuples) -> lists:map(fun(X) -> seldom_bit(X) end, ListOfTuples).
%gamma(ListOfTuples) -> lists:map(fun(X) -> often_bit(X) end, ListOfTuples).

calc_cell(0, {Zero, One}) -> {Zero+1,One};
calc_cell(1, {Zero, One}) -> {Zero,  One+1}.

calc([],    Res) -> Res;
calc([H|T],  []) ->
  Res=lists:map(fun(X) -> calc_cell(X, {0,0}) end, H),
  %io:format("~p ~p~n", [H, Res]),
  calc(T, Res);
calc([H|T], Acc) ->
  Tuples=lists:zip(H,Acc),
  FoldedTuples = lists:map(fun({X, Tup}) -> calc_cell(X, Tup) end, Tuples),
  %io:format("~p ~p => ~p~n", [H, Acc, FoldedTuples]),
  calc(T, FoldedTuples).

f(S, I) -> {Int, _} = string:to_integer(string:slice(S, I, 1)), Int.

str_to_tuples(Str, Cur, Size, Tuples) ->
  case Cur < Size of
    true  -> str_to_tuples(Str, Cur+1, Size, lists:append(Tuples,[f(Str, Cur)]));
    false -> Tuples
  end.

% 2 day
%string_list_to_tuples([<<"forward">>, StrNum]) -> {Num, _} = string:to_integer(StrNum), {forward, Num};
%string_list_to_tuples([<<"down">>,    StrNum]) -> {Num, _} = string:to_integer(StrNum), {down,    Num};
%string_list_to_tuples([<<"up">>,      StrNum]) -> {Num, _} = string:to_integer(StrNum), {up,      Num}.

%step({forward, N}, {X, Y, A}) -> io:format("~p ~p ~p ~n f ~p =>", [X, Y, A, N]), {X + N, Y + N * A, A};
%step({down,    N}, {X, Y, A}) -> io:format("~p ~p ~p ~n d ~p =>", [X, Y, A, N]), {X,         Y, A + N};
%step({up,      N}, {X, Y, A}) -> io:format("~p ~p ~p ~n u ~p =>", [X, Y, A, N]), {X,         Y, A - N}.

% 1 day
%trans([H|T], Res) ->
%  Sum = H + lists:nth(1, T) + lists:nth(2, T),
%  case length(T) < 3 of
%    true  -> lists:append(Res, [Sum]);
%    false -> trans(T, lists:append(Res, [Sum]))
%  end.

%calc([],          _, Amnt)                  -> Amnt;
%calc([H|T],       0, Amnt)                  -> calc(T, H,    Amnt);
%calc([Next|T], Prev, Amnt) when Next > Prev -> calc(T, Next, Amnt + 1);
%calc([Next|T],    _, Amnt)                  -> calc(T, Next, Amnt).

%calc([],          _,            _, Amnt)                  -> Amnt;
%calc([H|T],       0, IsIncreasing, Amnt)                  -> calc(T, H,    IsIncreasing, Amnt);
%calc([Next|T], Prev,        false, Amnt) when Next > Prev -> calc(T, Next,         true, Amnt + 1);
%calc([Next|T], Prev,         true, Amnt) when Next > Prev -> calc(T, Next,         true, Amnt);
%calc([Next|T],    _,            _, Amnt)                  -> calc(T, Next,        false, Amnt).
