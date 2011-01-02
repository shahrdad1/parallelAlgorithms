%% Author: Shahrdad
%% Created: May 2, 2010
%% Description: TODO: Add description to calandar_test


%% Erlang provides basic, but important ways of manipulating time and dates. 
%% erlang:localtime built-in function returns the current time as a tuple of tuples:
%% 
%% {{Year,Month,Day},{Hour,Min,Seconds}}.
%% 
%% The erlang:now built-in function returns the current time as a tuple:
%% 
%% {MegaSecs,Secs,Microsecs}.



-module(calendar_test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([trace_line/1,encode_Timestamp/1,encode_Millis/1,encode_Sec/1]).
-export([current_local_time/0,current_universal_time/0, gregorian_seconds_to_datetime/1]).
-export([string_to_date/0,now_to_local_time/0,now_to_universal_time/0,now_to_datetime/0]).
-export([seconds_to_daystime/1, seconds_to_time/1,time_difference_demo/0,time_to_seconds_demo/0,valid_date_demo/0]).

%% SOME USEFULL MACROS

-define(MilliSecondsPerMegaSecond, 1000000000). %% 1 Mega. Sec.  == 1000000000 Milli. Sec.
-define(SecondsPerMegaSecond, 1000000).			%% 1 Mega. Sec.  == 1000000 Sec.
	
-define(MilliSecondsPerSecond, 1000).			%% 1 Sec.        == 1000 Milli. Sec.
-define(MicroSecondsPerMilliSecond, 1000).		%% 1 Milli. Sec. == 1000 Micro. Sec.

-define(UnixEpoch, calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})). 

-define(Now2MilliSeconds(X), encode_Millis(X)). 

-define(Now2Seconds(X), encode_Sec(X)). 

-define(Now2Timestamp(X), encode_Timestamp(X)).

-define(Trace(X),trace_line(X)).

%%
%% API Functions
%%


current_local_time()-> 	{CurrentDate,CurrentTime} = calendar:local_time(),
					   	{Year,Month,Day} = CurrentDate,
					   	{HH,MM,SS} = CurrentTime,
						io:format("Current local time: ~p/~p/~p ~p:~p:~p ~n", [Year,Month,Day,HH,MM,SS]),
						io:format("number of gregorian days starting with year 0: ~p ~n",[calendar:date_to_gregorian_days(CurrentDate)]),
						io:format("number of gregorian seconds starting with year 0: ~p ~n",[calendar:datetime_to_gregorian_seconds({CurrentDate,CurrentTime})]),
						io:format("day of the week given Year, Month and Day: ~p ~n ", [calendar:day_of_the_week(CurrentDate)]),
						io:format("number of days in a month: ~p ~n ", [calendar:last_day_of_the_month(Year,Month)]).

current_universal_time()-> {CurrentDate,CurrentTime} = calendar:universal_time(),
					   	{Year,Month,Day} = CurrentDate,
					   	{HH,MM,SS} = CurrentTime,
						io:format("Current universal time: ~p/~p/~p ~p:~p:~p ~n", [Year,Month,Day,HH,MM,SS]),
						io:format("number of gregorian days starting with year 0: ~p ~n",[calendar:date_to_gregorian_days(CurrentDate)]),
						io:format("number of gregorian seconds starting with year 0: ~p ~n",[calendar:datetime_to_gregorian_seconds({CurrentDate,CurrentTime})]),
						io:format("day of the week given Year, Month and Day: ~p ~n ", [calendar:day_of_the_week(CurrentDate)]),
						io:format("number of days in a month: ~p ~n ", [calendar:last_day_of_the_month(Year,Month)]),
						{LocalDate, LocalTime}=calendar:universal_time_to_local_time({CurrentDate,CurrentTime}),
						io:format("Universal Date and Time transformed into Local Date and Time: "),
						format_date(LocalDate),
						format_time(LocalTime).

gregorian_seconds_to_datetime(Seconds) -> {Date,Time}=calendar:gregorian_seconds_to_datetime(Seconds),
										  format_date(Date),
										  format_time(Time).

string_to_date()-> 
	{Date,Time}=httpd_util:convert_request_date("Wed, 28 Apr 2004 17:22:10-05:00"),
	format_date(Date),
	format_time(Time),
	[{Date1,Time1}]=calendar:local_time_to_universal_time_dst({Date,Time}),
	format_date(Date1),
	format_time(Time1).


now_to_local_time()-> 	{Date,Time}=calendar:now_to_local_time(erlang:now()),
					  	format_date(Date),
						format_time(Time).

now_to_universal_time()-> {Date,Time}=calendar:now_to_universal_time(erlang:now()),
					  	format_date(Date),
						format_time(Time).

now_to_datetime()-> {Date,Time}=calendar:now_to_datetime(erlang:now()),
					  	format_date(Date),
						format_time(Time).

%% This function transforms a given number of seconds into days, hours, minutes, and seconds. 
%% The Time part is always non-negative, but Days is negative if the argument Seconds is.

seconds_to_daystime(Seconds)->{Days,Time}=calendar:seconds_to_daystime(Seconds),
						io:format("Days: ~p ~n",[Days]),							  
						format_time(Time).



%% This function computes the time from the given number of seconds. 
%% Seconds must be less than the number of seconds per day (86400).

seconds_to_time(Seconds) when Seconds < 86400 -> Time=calendar:seconds_to_time(Seconds),
							format_time(Time);

seconds_to_time(_) -> throw ("The arg must be less than 86400").

%% This function computes the number of seconds since midnight up to the specified time.

time_to_seconds_demo() -> {_, Time}=httpd_util:convert_request_date("Mon, 15 Sep 2010 12:11:44 GMT"),
						  calendar:time_to_seconds(Time).


time_difference_demo() -> {Date1, Time1}=httpd_util:convert_request_date("Sat, 28 Aug 2004 01:19:37 GMT"),
						  {Date2, Time2}=httpd_util:convert_request_date("Sat, 30 Aug 2004 01:19:39 GMT"),
						  Seconds1 = calendar:datetime_to_gregorian_seconds({Date1, Time1}),
						  Seconds2 = calendar:datetime_to_gregorian_seconds({Date2, Time2}),
						  case Seconds1 < Seconds2 of
							  true -> SecondsDiff = Seconds2 - Seconds1;
							  false -> SecondsDiff = Seconds1 - Seconds2
						  end,
						  seconds_to_daystime(SecondsDiff).


valid_date_demo() -> calendar:valid_date({2010,5,5}).

%%
%% Local Functions
%%

encode_Millis({MegaSeconds, Seconds, MicroSeconds}) ->
							MegaSeconds * ?MilliSecondsPerMegaSecond + 
	 						Seconds * ?MilliSecondsPerSecond + 
							MicroSeconds div ?MicroSecondsPerMilliSecond.

encode_Timestamp ({MegaSeconds, Seconds, MicroSeconds}) ->
					{{Year,Month,Day},{HH,MM,SS}}=
						calendar:now_to_universal_time({MegaSeconds, Seconds, MicroSeconds}),
					{Year,Month,Day,HH,MM,SS,MicroSeconds}.

trace_line(X) -> {Year,Month,Day,HH,MM,SS,MicroSeconds} = ?Now2Timestamp(erlang:now()),
							io:format("~p/~p/~p - ~p:~p:~p:~p [~p - ~p - ~p] --> ~p~n",
								[Year,Month,Day,HH,MM,SS,MicroSeconds, ?MACHINE, ?MODULE,?LINE,X]).

encode_Sec({MegaSeconds, Seconds, _MicroSeconds}) ->
							MegaSeconds * ?SecondsPerMegaSecond + Seconds.




format_date(Date)-> {Year,Month,Day} = Date,
					io:format("Date: ~p/~p/~p ~n", [Year,Month,Day]).

format_time(Time)-> {HH,MM,SS} = Time,
					io:format("Time: ~p:~p:~p ~n ~n", [HH,MM,SS]).



%% The Erlang httpd_util module :
%% 
%% 1> CurrTime = erlang:universaltime().
%% {{2004,8,28},{8,15,38}}

%% 2> httpd_util:rfc1123_date(CurrTime).
%% "Sat, 28 Aug 2004 08:15:38 GMT"
%% 
%% Most applications that use RFC 1123 or RFC 822 time formats require all output in GMT. 
%% However, Erlang does not provide support for RFC 822 formatted time, since it assumes all 
%% input times are GMT.
%% 
%% 3> LocalTime = erlang:localtime().
%% {{2004,8,28},{1,19,37}}
%% 
%% 4> httpd_util:rfc1123_date(LocalTime).
%% "Sat, 28 Aug 2004 01:19:37 GMT"
%% 
%% If you need to parse dates as input, you can use httpd_util:convert_request_date function:
%% 
%% 5> DateString = "Wed, 28 Apr 2004 17:22:10 Z".
%% "Wed, 28 Apr 2004 17:22:10 Z"
%% 
%% 6> httpd_util:convert_request_date("Wed, 28 Apr 2004 17:22:10-05:00"). 
%% {{2004,4,28},{17,22,10}}
%% 
%% 7> httpd_util:convert_request_date(DateString).
%% {{2004,4,28},{17,22,10}}
%% 
%% RFC 1123 (and RFC 822) are used frequently as the basis for exchanging dates and times, 
%% particularly in Internet applications.

%% 148> httpd_util:rfc1123_date().
%% "Mon, 03 May 2010 02:57:10 GMT"
%% 
%% 149> httpd_util:convert_request_date("Mon, 03 May 2010 02:57:10 GMT").
%% {{2010,5,3},{2,57,10}}
%% 
%% 154> calendar:universal_time().
%% {{2010,5,3},{3,5,36}}
%% 
%% 155> calendar:local_time_to_universal_time_dst({{2010,5,3},{2,57,10}}).
%% [{{2010,5,3},{6,57,10}}]
%% 
%% 156> httpd_util:rfc1123_date().
%% "Mon, 03 May 2010 03:08:51 GMT"
%% 
%% 157> httpd_util:rfc1123_date().
%% "Mon, 03 May 2010 03:08:57 GMT"
%% 
%% 158> httpd_util:rfc1123_date({{2010,5,3},{6,57,10}}).
%% "Mon, 03 May 2010 10:57:10 GMT"
%% 
%% 160> erlang:now().                
%% {1272,856313,187000}
