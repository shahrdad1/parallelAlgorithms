
-module(timer_test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([apply_after_test/2,print_message/2, send_after_test/2, send/3,sleep/1]).
-export([apply_interval_test/2, cancle_messages/1,slowFunction/0,tc_test/0, now_diff_test/1]).

%%
%% API Functions
%%

%% This module provides useful functions related to time. Unless otherwise stated, time is always 
%% measured in milliseconds. 
%% All timer functions return immediately, regardless of work carried out by another process. 

%% Successful evaluations of the timer functions yield return values containing a timer reference, 
%% denoted TRef below. By using cancel/1, the returned reference can be used to cancel any requested action. A TRef is an Erlang term, the contents of which must not be altered. 


%% A one-shot timer, i.e. a timer created by evaluating any of the functions apply_after/4, 
%% send_after/3, send_after/2, exit_after/3, exit_after/2, kill_after/2, and kill_after/1 
%% is not linked to any process. Hence, such a timer is removed only when it reaches its timeout, 
%% or if it is explicitly removed by a call to cancel/1.

apply_after_test(Time, Message) -> 
	{ok, Tref}=timer:apply_after(Time, timer_test, print_message, [Time, Message]),
	io:format("You will see ~p after ~p milliseconds...~n",[Message,Time]).




%% An interval timer, i.e. a timer created by evaluating any of the functions apply_interval/4, 
%% send_interval/3, and send_interval/2, is linked to the process towards which the timer performs 
%% its task. 

%% Evaluates apply(Module, Function, Arguments) repeatedly at intervals of Time. Returns {ok, TRef}, or {error, Reason}.
apply_interval_test(Time, Message) ->  
	{ok, Tref}=timer:apply_interval(Time, timer_test, print_message, [Time, Message]),
	timer:apply_after(10000, timer_test, cancle_messages, [Tref]).

	
	
tc_test() -> 
	io:format("\"slowFunction\" starts ...~n"),
	{Time, RetunValue} = timer:tc(timer_test, slowFunction, []), 
	io:format("It took ~p Microsecond for \"slowFunction\" to complete and returned ~p. \n",[Time, RetunValue]).
	


now_diff_test(Time) -> T1 = now(),
				   timer:sleep(Time),
				   T2 = now(),
				   io:format("it took ~p microseconds for timer:sleep() statement to run~n", 
							 [timer:now_diff(T2, T1)-Time*1000]).



send_after_test(Time, Message) -> spawn(timer_test, send, [self(),Time,Message]).




%%
%% Local Functions
%%

send(Pid, Time, Message) -> receive 
								after 
									Time -> Pid ! Message
							end.



sleep(Time) ->receive
			   after
				   Time -> true
		   end.


print_message(Time,Message) -> io:format("Here is the message after ~p Milis: ~p~n", [Time,Message]).

cancle_messages(TRef)->timer:cancel(TRef),
					   io:format("enough is enough!!").

slowFunction() -> timer:sleep(3000).
