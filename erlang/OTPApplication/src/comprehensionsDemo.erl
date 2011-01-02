%% Author: Shahrdad
%% Created: Jan 31, 2010
%% Description: TODO: Add description to comprehensionsDemo
-module(comprehensionsDemo).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([quickSort/1]).
-export([quickSort1/1]).
-export([pythag/1]).

%%
%% API Functions
%%

%%
%% one version of QSort
%%
quickSort([])->[];
quickSort([Pivot|T])	-> quickSort([H || H<-T , H<Pivot]) ++[Pivot]++ quickSort([H || H<-T , H>=Pivot]).

%%
%% Better version of QSort
%%
quickSort1([])->[];

quickSort1([H|T]) -> {Smaller, Grater}=partition(H, T),
					 lists:append(quickSort1(Smaller), [H|quickSort1(Grater)]).

%% First production rule to add accumulator lists 
partition(Pivot,T) 											-> partition(Pivot, T, [],[]).

%% Second production rule to fill accumulator lists with all the elements less(grater) than a given pivot
partition(Pivot, [], Smaller,Grater)						-> {Smaller, Grater};
partition(Pivot, [H|T], Smaller,Grater) when H >  Pivot 	-> partition(Pivot, T, Smaller,[H|Grater]);
partition(Pivot, [H|T], Smaller,Grater)	when H =< Pivot 	-> partition(Pivot, T, [H|Smaller],Grater).



%Pythagor function
pythag(N)	-> [{A,B,C} || A <- lists:seq(1, N), B<-lists:seq(1, N), C<-lists:seq(1, N), A+B+C=<N, A*A+B*B=:=C*C].

%%
%% Local Functions
%%

