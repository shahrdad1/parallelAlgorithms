%% Author: Shahrdad
%% Created: Apr 17, 2010
%% Description: TODO: Add description to test
-module(try_catch_test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test_gaurds/1, test1/1, even_filter/1, member/2, test_try_catch1/1]).
-export([test_try_catch2/1,test_try_catch3/1,fetch/0]).
%%
%% API Functions
%%

%%
%% TODO: Add description of test_gaurds/function_arity
%%
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


test_try_catch1(X) -> Val = 12,
					try (X=Val) of 
						  Val -> {normal,Val}
					catch 
						  Class1:ExceptionPattern1 -> 
							  {ErrorMessage, MatchResult} = ExceptionPattern1,
							  case ErrorMessage of 
								  badmatch -> 
					  				io:format(" ~p:Invalid Input value ~p ~n", [Class1,MatchResult])
							  end
					end.

test_try_catch2(X) -> Val = 12,
					try (X=Val) of 
						  Val -> {normal,Val}
					catch 
						  Class1:ExceptionPattern1 -> 
							  {ErrorMessage, MatchResult} = ExceptionPattern1,
							  case ErrorMessage of 
								  badmatch -> throw (atom_to_list(Class1) ++ ":Invalid Input value: " ++ integer_to_list(MatchResult))
%% 					  				io:format(" ~p:Invalid Input value ~p ~n", [Class1,MatchResult])
							  end
					end.


test_throw (X) when is_integer(X) -> 
	case X of 
	  0	-> 1/X;
	  1	-> throw (['One',{badarith, [{exception,return_error,1},{test,test_throw,X}]}]);
	  2 -> X=0;
	  3 -> exit("Out of patience!")
		
	end.

test_try_catch3(X) ->
	try test_throw(X) 
	catch
		Class1:ExceptionPattern1 -> 
			case Class1 of 
				throw -> 
						[Description,{ErrorMessage, [{Exception,Return_Error,Value},{Module,Function,Arg}]}]=ExceptionPattern1,
						io:format(" ~p:~p:~p ~p ~n", [Class1,Description,Module,Function]);
				
				error ->
						
						 case ExceptionPattern1 of
							 
							  badmatch -> 
					  				io:format("  ~p:ExceptionPattern1 is ~p ~n", [Class1,ExceptionPattern1]);
							 
							 {ErrorMessage1, MatchResult1} ->
							 	io:format("  ~p:ExceptionPattern1 is ~p ~n", [ErrorMessage1,MatchResult1]);
							 
							  badarith -> 
								 	io:format("  ~p:ExceptionPattern1 is ~p ~n", [Class1,ExceptionPattern1]);
							 
							   _ -> 	
									io:format(" Other =>  ~p:ExceptionPattern1 is ~p ~n", [Class1,ExceptionPattern1])
						 end;
				exit ->
						io:format(" ~p:ExceptionPattern1 is ~p ~n", [Class1,ExceptionPattern1]);
				
				_ -> 	io:format(" Other =>  ~p:ExceptionPattern1 is ~p ~n", [Class1,ExceptionPattern1])
			
			end
	end.


%%
%% Local Functions
%%


