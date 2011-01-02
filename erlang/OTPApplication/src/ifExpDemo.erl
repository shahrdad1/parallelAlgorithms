%% Author: Shahrdad
%% Created: Feb 7, 2010
%% Description: TODO: Add description to ifExpDemo
-module(ifExpDemo).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([filter/1]).

%%
%% API Functions
%%

%%
%% TODO: Add description of filter/function_arity
%%
filter([H|T]) -> if
					   (H rem 2 == 0) -> [H|filter(T)];
					   (H rem 2 == 1) -> filter(T)
				end;
filter([])	-> [].
					
	


%%
%% Local Functions
%%

