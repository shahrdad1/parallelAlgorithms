%% red_black_tree:createRBTree([{8,8},{1,1},{2,2},{9,9},{7,7},{6,6},{3,3},{13,13}])

%% Author: sshadab
%% Created: 2010-08-31
%% Description: TODO: Add description to red_black_tree
-module(red_black_tree).

-define(Now2Timestamp(X), encode_Timestamp(X)). 
-define(Trace(IoDevice,X),trace_line(IoDevice,X)).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([createRBTree/1,balance/2]).
-export([encode_Timestamp/1,trace_line/2]).
%%
%% API Functions
%%

%% Red-Black tree ==> {key, Value, Color , Left subtree, right subtree}
createRBTree(L)	-> 
	IoDevice = open_log_file("./log/Red_Black_Tree.log"),
	insert_nodes(L,nil,IoDevice).

insert_nodes([],Tree,IoDevice) -> Tree;
insert_nodes([{Key,Value}|T], Tree,IoDevice) -> 
	insert_nodes(T, recolor_root_to_black (insert (Key,Value,Tree,IoDevice),IoDevice),IoDevice).
	
insert (Key, Value, nil,IoDevice) ->
	{Key, Value, red ,nil, nil};

insert (Key, Value,{Key,_,Color,LSubtree1, RSubtree1},_) -> 
	{Key,Value,Color,LSubtree1, RSubtree1};

insert (Key, Value, {Key1,Value1,Color, LSubtree1, RSubtree1},IoDevice) -> 
			?Trace(IoDevice,string_format( "Insert ~w to {~w,~w,~w,~w} ~n",[Key,Key1,Color,LSubtree1,RSubtree1],?LINE)),
			case Key < Key1 of 
				true ->
					balance({ Key1, Value1,Color, insert(Key,Value,LSubtree1,IoDevice),RSubtree1},IoDevice);
				false ->
					balance({ Key1, Value1,Color, LSubtree1, insert (Key,Value,RSubtree1,IoDevice)},IoDevice)
			end.




%%
%% Local Functions
%%

%% balance (T B (T R (T R a x b) y c) z d) 
%%          = T R (T B a x b) y (T B c z d)

balance ({RootKey,RootValue,black, %% z
		  {LChildKey,LChildValue,red, %% y
			{LGrandChildKey,LGrandChildValue,red,  %% x
			 GrandChildLSubtree,GrandChildRSubtree %% a , b
			}, 
		   	ChildRSubtree %%c
		  },
		  RSubtree %%d
		 },IoDevice) -> 
			?Trace(IoDevice,string_format( "balance (T B (T R (T R a x b) y c) z d) -> T R (T B a x b) y (T B c z d)~n",[],?LINE)),
			?Trace(IoDevice,string_format( "balance z => {~w,black}~n",[RootKey],?LINE)),
			?Trace(IoDevice,string_format( "balance x => {~w,red}~n",[LGrandChildKey],?LINE)),
			?Trace(IoDevice,string_format( "balance d => ~w ~n",[RSubtree],?LINE)),
			?Trace(IoDevice,string_format( "balance a => ~w ~n",[GrandChildLSubtree],?LINE)),
			?Trace(IoDevice,string_format( "balance y => {~w,red}~n",[LChildKey],?LINE)),
			?Trace(IoDevice,string_format( "balance b => ~w ~n",[GrandChildRSubtree],?LINE)),
			?Trace(IoDevice,string_format( "balance c => ~w ~n",[ChildRSubtree],?LINE)),

			{LChildKey,LChildValue,red, %% y
				{LGrandChildKey,LGrandChildValue,black, %% x
			 		GrandChildLSubtree,GrandChildRSubtree %% a , b
				},
				{RootKey,RootValue,black, %% z
				 ChildRSubtree, %%c
				 RSubtree %%d
				}
			};



%% balance (T B (T R a x (T R b y c)) z d) 
%%          = T R (T B a x b) y (T B c z d)

balance ({RootKey,RootValue,black, %% z
		  {LChildKey,LChildValue,red, %%x 
		   ChildLSubtree, %%a
		   {RGrandChildKey,RGrandChildValue,red, %% y
			GrandChildLSubtree,GrandChildRSubtree %% b , c
		   }
		  },
		  RSubtree %%d
		 },IoDevice) ->
			?Trace(IoDevice,string_format( "balance (T B (T R a x (T R b y c)) z d) -> T R (T B a x b) y (T B c z d)~n",[],?LINE)),
			?Trace(IoDevice,string_format( "balance z => {~w,black}~n",[RootKey],?LINE)),
			?Trace(IoDevice,string_format( "balance x => {~w,red}~n",[LChildKey],?LINE)),
			?Trace(IoDevice,string_format( "balance d => ~w ~n",[RSubtree],?LINE)),
			?Trace(IoDevice,string_format( "balance a => ~w ~n",[ChildLSubtree],?LINE)),
			?Trace(IoDevice,string_format( "balance y => {~w,red}~n",[RGrandChildKey],?LINE)),
			?Trace(IoDevice,string_format( "balance b => ~w ~n",[GrandChildLSubtree],?LINE)),
			?Trace(IoDevice,string_format( "balance c => ~w ~n",[GrandChildRSubtree],?LINE)),

			{RGrandChildKey,RGrandChildValue,red, %% y
				{LChildKey,LChildValue,black, %%x 
		   			ChildLSubtree,GrandChildLSubtree %% a , b
				},
				{RootKey,RootValue,black, %%z
				 GrandChildRSubtree,RSubtree %% c , d
				}
			};


%% balance (T B a x (T R (T R b y c) z d)) 
%%          = T R (T B a x b) y (T B c z d)

balance({RootKey,RootValue,black, %% x
		 LSubtree, %% a
		 {RChildKey,RChildValue,red, %% z
		  {LGrandChildKey,LGrandChildValue,red, %% y
		   GrandChildLSubtree,GrandChildRSubtree %% b , c
		  },
		  ChildRSubtree % d
		 }
		},IoDevice) -> 
	
			?Trace(IoDevice,string_format( "balance (T B a x (T R (T R b y c) z d)) -> T R (T B a x b) y (T B c z d)~n",[],?LINE)),
			?Trace(IoDevice,string_format( "balance x => {~w,black}~n",[RootKey],?LINE)),
			?Trace(IoDevice,string_format( "balance a => ~w ~n",[LSubtree],?LINE)),
			?Trace(IoDevice,string_format( "balance z => {~w,red}~n",[RChildKey],?LINE)),
			?Trace(IoDevice,string_format( "balance d => ~w ~n",[ChildRSubtree],?LINE)),
			?Trace(IoDevice,string_format( "balance y => {~w,red}~n",[LGrandChildKey],?LINE)),
			?Trace(IoDevice,string_format( "balance b => ~w ~n",[GrandChildLSubtree],?LINE)),
			?Trace(IoDevice,string_format( "balance c => ~w ~n",[GrandChildRSubtree],?LINE)),
	
		{LGrandChildKey,LGrandChildValue,red, %% y
			   {RootKey,RootValue,black, %% x
				LSubtree, %% a
				GrandChildLSubtree %% b
			   },
			   {RChildKey,RChildValue,black,
				GrandChildRSubtree,ChildRSubtree % c , d
			   }
		};
				
				
%% balance (T B a x (T R b y (T R c z d))) 
%%          = T R (T B a x b) y (T B c z d)

				
balance({RootKey,RootValue,black, %% x
		 LSubtree, %% a
 		 {RChildKey,RChildValue,red, %% y
		  ChildLSubtree, %% b
		  {RGrandChildKey,RGrandChildValue,red, %% z
		   GrandChildLSubtree,GrandChildRSubtree %% c , d
		  }
		 }
		},IoDevice) ->
			?Trace(IoDevice,string_format( "balance (T B a x (T R b y (T R c z d))) -> T R (T B a x b) y (T B c z d)~n",[],?LINE)),
			?Trace(IoDevice,string_format( "balance x => {~w,black}~n",[RootKey],?LINE)),
			?Trace(IoDevice,string_format( "balance a => ~w ~n",[LSubtree],?LINE)),
			?Trace(IoDevice,string_format( "balance y => {~w,red}~n",[RChildKey],?LINE)),
			?Trace(IoDevice,string_format( "balance b => ~w ~n",[ChildLSubtree],?LINE)),
			?Trace(IoDevice,string_format( "balance z => {~w,red}~n",[RGrandChildKey],?LINE)),
			?Trace(IoDevice,string_format( "balance c => ~w ~n",[GrandChildLSubtree],?LINE)),
			?Trace(IoDevice,string_format( "balance d => ~w ~n",[GrandChildRSubtree],?LINE)),
	
			{RChildKey,RChildValue,red, %% y
			   {RootKey,RootValue,black, %%x
				LSubtree, %% a
				ChildLSubtree %% b
			   },
			   {RGrandChildKey,RGrandChildValue,black, %% z
				GrandChildLSubtree,GrandChildRSubtree %% c , d
			   }
			};

balance(T,IoDevice) ->
	?Trace(IoDevice,string_format( "balance(~w ) -> ~w~n",[T,T],?LINE)),
	T.
				

recolor_root_to_black({Key, Val, red , L, R},IoDevice) -> 
	?Trace(IoDevice,string_format( "recolor_root_to_black({~w,red,~w,~w})~n",[Key,L,R],?LINE)),
	{Key, Val, black , L, R};

recolor_root_to_black(T,IoDevice)->T.


open_log_file (LogFileName) ->
				case file:open(LogFileName, [write,{encoding, utf8}]) of 
					{ok,IoDevice} -> IoDevice;
					{error,Reason} -> 
								io:format("File Open Error: ~p ~n",[file:format_error(Reason)])
				end.

string_format(Pattern, Values, LineNo) -> lists:flatten(io_lib:format("Line# ~w : "++Pattern, [LineNo|Values])).


trace_line(IoDevice,X) -> {Year,Month,Day,HH,MM,SS,MicroSeconds} = ?Now2Timestamp(erlang:now()),
							io:format(IoDevice,"~p/~p/~p - ~p:~p:~p:~p [Machine:~p * Module:~p ] --> ~p~n",
								[Year,Month,Day,HH,MM,SS,MicroSeconds, ?MACHINE, ?MODULE, X]).



encode_Timestamp ({MegaSeconds, Seconds, MicroSeconds}) ->
					{{Year,Month,Day},{HH,MM,SS}}=
						calendar:now_to_universal_time({MegaSeconds, Seconds, MicroSeconds}),
					{Year,Month,Day,HH,MM,SS,MicroSeconds}.

