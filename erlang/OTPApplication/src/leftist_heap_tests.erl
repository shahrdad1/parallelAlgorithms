-module(leftist_heap_tests).


%% How to run:
%% leftist_heap_tests:test().

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%

createHeap_test () ->
	?assertEqual(leftist_heap:createHeap(),
				 {1,{4,{5,{6,nil,nil},{11,nil,nil}},{8,{9,{10,nil,nil},nil},nil}},{3,nil,nil}}).
	
insert_test () -> 	Heap = leftist_heap:createHeap(),
					?assertEqual(leftist_heap:insert(7, Heap), 
								 {1,{4,{5,{6,nil,nil},{11,nil,nil}},{8,{9,{10,nil,nil},nil},nil}},{3,{7,nil,nil},nil}}),	
					Heap1 = leftist_heap:createHeap(),
					?assertEqual(leftist_heap:insert(18, Heap1), 
								 {1,{4,{5,{6,nil,nil},{11,nil,nil}},{8,{9,{10,nil,nil},nil},nil}},{3,{18,nil,nil},nil}}),

					Heap2 = {1,{4,{5,{6,nil,nil},{11,nil,nil}},{8,{9,{10,nil,nil},nil},nil}},{3,{18,nil,nil},nil}},
					?assertEqual(leftist_heap:insert(16, Heap2), 
								 {1,{4,{5,{6,nil,nil},{11,nil,nil}},{8,{9,{10,nil,nil},nil},nil}},{3,{18,nil,nil},{16,nil,nil}}}),

					Heap3 = {1,{4,{5,{6,nil,nil},{11,nil,nil}},{8,{9,{10,nil,nil},nil},nil}},{3,{18,nil,nil},{16,nil,nil}}},
					?assertEqual(leftist_heap:insert(15, Heap3), 
								 {1,{4,{5,{6,nil,nil},{11,nil,nil}},{8,{9,{10,nil,nil},nil},nil}},{3,{18,nil,nil},{15,{16,nil,nil},nil}}}).


find_min_test () -> Heap = leftist_heap:createHeap(),
					?assertEqual(leftist_heap:find_min(Heap),1).

delete_min_test () -> Heap = leftist_heap:createHeap(),
					  ?assertEqual(leftist_heap:delete_min(Heap),{3,{4,{5,{6,nil,nil},{11,nil,nil}},{8,{9,{10,nil,nil},nil},nil}},nil}).


%%
%% Local Functions
%%

