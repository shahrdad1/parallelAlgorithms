%% file_comment
-module(trace_macro).

-define(MilliSecondsPerMegaSecond, 1000000000). %% 1 Mega. Sec.  == 1000000000 Milli. Sec.
-define(SecondsPerMegaSecond, 1000000).			%% 1 Mega. Sec.  == 1000000 Sec.
	
-define(MilliSecondsPerSecond, 1000).			%% 1 Sec.        == 1000 Milli. Sec.
-define(MicroSecondsPerMilliSecond, 1000).		%% 1 Milli. Sec. == 1000 Micro. Sec.

-define(UnixEpoch, calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})). 

-define(Now2Timestamp(X), encode_Timestamp(X)). 



-ifdef(debug).
-define(Trace(X),trace_line(X)).
-else.
-define(Trace(X),void).
-endif.

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0,encode_Timestamp/1,trace_line/1]).

%%
%% API Functions
%%

start() -> loop(5).

loop(0) -> void;
loop(N) ->  ?Trace(N),loop(N-1).

%%
%% Local Functions
%%



encode_Timestamp ({MegaSeconds, Seconds, MicroSeconds}) ->
					{{Year,Month,Day},{HH,MM,SS}}=
						calendar:now_to_universal_time({MegaSeconds, Seconds, MicroSeconds}),
					{Year,Month,Day,HH,MM,SS,MicroSeconds}.

trace_line(X) -> {Year,Month,Day,HH,MM,SS,MicroSeconds} = ?Now2Timestamp(erlang:now()),
							io:format("~p/~p/~p - ~p:~p:~p:~p [Machine:~p * Module:~p * Line# ~p] --> ~p~n",
								[Year,Month,Day,HH,MM,SS,MicroSeconds, ?MACHINE, ?MODULE,?LINE,X]).

