%% Author: Shahrdad
%% Created: Apr 17, 2010
%% Description: TODO: Add description to test
-module(try_test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([demo/0,demo1/0, demo2/0, sqrt/1]).
-export([test_gaurds/1, test1/1, even_filter/1, member/2]).
%%
%% API Functions
%%

%%
%% we can trap and distinguish all the forms of exception
%% that a function can raise
%%

demo () -> [test_try_catch (X) || X <- [0,1,2,3,4,5,6,"asdf",8,wefd,{p,q},[a,"2",3]]].

test_try_catch (X) ->
	
		try exception_generator(X) of 
			{ok,X} -> {ok,X};
			{"EXIT", X} -> {ok,X}
		catch 
			exit:  E -> {exit,E, {stackTrace,erlang:get_stacktrace()}};
			throw: E -> {throw,E, {stackTrace,erlang:get_stacktrace()}};
			
			%%Internal errors detected by the Erlang runtime system always have the tag error.
 			error: {ErrorTag, E} -> {internal_error_detected_by_Erlang,{ErrorTag, E, {stackTrace,erlang:get_stacktrace()}}};
			
			%% Erlang Error thrown by the code
 			error: E -> {erlang_error_thrown_intentionally,E, {stackTrace,erlang:get_stacktrace()}}

		after 
			io:format("Cleaning up after exception_generator called with value: ~p ~n",[X])
		end.	

%% ------------ Shortened Form -------------

demo1 () -> [test_try_catch1(X) || X <- [0,1,2,3,4,5,6,"asdf",8,wefd,{p,q},[a,"2",3]]].

test_try_catch1 (X) ->
	
		try exception_generator(X) 
			
		catch 
			exit:  E -> {exit,E, {stackTrace,erlang:get_stacktrace()}};
			throw: E -> {throw,E, {stackTrace,erlang:get_stacktrace()}};
			
			%%Internal errors detected by the Erlang runtime system always have the tag error.
 			error: {ErrorTag, E} -> {internal_error_detected_by_Erlang,{ErrorTag, E, {stackTrace,erlang:get_stacktrace()}}};
			
			%% Erlang Error thrown by the code
 			error: E -> {erlang_error_thrown_intentionally,E, {stackTrace,erlang:get_stacktrace()}}

		end.	


%% ------------ Primitive catch ---------------

demo2 () -> [{X, catch exception_generator(X)} || X <- [0,1,2,3,4,5,6,"asdf",8,wefd,{p,q},[a,"2",3]] ].


exception_generator(X) ->
	case X of 
		0 -> exit ("0");
		1 -> throw ("Business Exception: 1 is not not a valid input.");
		2 -> erlang:error("Crashed ! Caused by value 2");
		3 -> {"EXIT", 3};
		4 -> {ok,4}
	end.


%% ------------ Improving Error Messages ---------------

sqrt (X) when X < 0 -> erlang:error({squareRootNegativeArgument, X});
sqrt (X)		 	-> math:sqrt(X).






%% ***********************************************


test_gaurds(X) -> 
	case X rem 2 of 
		0 when X rem 2 == 0; true   -> io:format("First clause");
		0 when X > 50  -> io:format("Second clause");
		0 when X > 0 ; X=:=0 , X<1 ; X< -1 -> io:format("Third clause");
		1 when X > 0 -> io:format("Forth clause")
	end.

test1(L) when is_list(L) , length(L)>0 -> length(L).

even_filter([])->[];
even_filter([H|T]) when H rem 2 == 0 -> [H|even_filter(T)];
even_filter([_|T]) -> even_filter(T).

member(_,[])->false;
member(H,[H|_]) -> true;
member(X,[_|T]) -> member (X,T).



fetch() ->  Key = 12, Dict = dict:new(),
		try dict:fetch(Key, Dict) of
			Value -> io:format("~p --> ~p ~n", [Key, Value]),
					 Value
		catch Class1:Pattern1 ->
			case Class1 of 
			  error -> io:format("Key ~p not found in dictionary ~p ~n",[Key, Dict]),
			   			key_not_found; %%client code matches this value to see if the lookup was successfull

			  Other -> io:format("~p:~p" ,[Other,Pattern1])
			end
		end.

