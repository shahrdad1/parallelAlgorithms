%% Author: gik
%% Created: Oct 15, 2010
%% Description: TODO: Add description to factoril
-module(factoril).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/1]).

%%
%% API Functions
%%

start ([A,B]) ->
	Arg1_List = atom_to_list(A),
	Arg1_Int  = list_to_integer(Arg1_List),
	
	Arg2_List = atom_to_list(B),
	Arg2_Int  = list_to_integer(Arg2_List),
	
	case Arg1_Int < Arg2_Int of
		true -> 
			Result = fac(Arg1_Int),
			io:format ("factoril ~p = ~p ~n", [Arg1_Int,Result]);
		
		_ ->
			Result = fac(Arg2_Int),
			io:format ("factoril ~p = ~p ~n", [Arg2_Int,Result])
	end.
		



fac (0) -> 1;
fac (N) -> N*fac(N-1).

%%
%% Local Functions
%%

