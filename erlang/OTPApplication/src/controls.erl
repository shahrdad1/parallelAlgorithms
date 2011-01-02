%% Author: Shahrdad
%% Created: Jan 30, 2010
%% Description: TODO: Add description to controls
-module(controls).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([for/3]).
-export([map/2]).
-export([map1/2]).
-export([sum/1]).
%%
%% API Functions
%%

%%
%% TODO: Add description of for/function_arity
%%
for(Max,Max,F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1,Max,F)].



map(F,[H|T])	->[F(H)|map(F,T)];
map(_,[])		->[].


% Using List comprehension to define map
map1(F,L)		-> [F(X) || X<-L].

sum([H|T])		->H+sum(T);
sum([])			->0.

%%
%% Local Functions
%%

