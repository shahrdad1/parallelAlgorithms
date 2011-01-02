%% Author: Shahrdad
%% Created: Mar 6, 2010
%% Description: TODO: Add description to breakPoints
-module(breakPoints).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([breakAtInsert/1]).

%%
%% API Functions
%%

breakAtInsert(Bindings)	->	case int:get_binding('Value', Bindings) of
								{value,susan}	-> true;
								_	-> false
							end.

%%
%% Local Functions
%%

