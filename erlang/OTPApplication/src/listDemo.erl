%% Author: Shahrdad
%% Created: Feb 13, 2010
%% Description: TODO: Add description to listDemo
-module(listDemo).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([nTimes/3]).

%%
%% API Functions
%%

%%
%% TODO: Add description of createList/function_arity
%%

nTimes([H|T],N, Result) -> nTimes(T,N,[H*N|Result]); 					%% result is in opposite order of original list.

%%nTimes([H|T],N,Result) -> nTimes(T,N,lists:append(Result,[N*H])); 		%% result is in the same order of original list.

nTimes([],N,Result)	 -> Result.



%%
%% Local Functions
%%

