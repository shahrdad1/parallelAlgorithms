%% See Page #6 from Leighton

-module(systolic_array).
-export([run_from_make/1,sort/5]).
-include("trace.hrl").

run_from_make ([TimeInterval]) ->
    {TI,_} = string:to_integer(atom_to_list(TimeInterval)),
    run(TI).


run(TimeInterval) ->

    IODevice = open_log_file("./log/Systolic Array.log"),
    register(pomper,self()),
    register(firstNode, spawn(systolic_array,  sort, [IODevice,firstNode, {pomper,secondNode},-999,TimeInterval])),
    register(secondNode,spawn(systolic_array,  sort, [IODevice,secondNode,{firstNode,thirdNode},-999,TimeInterval])),
    register(thirdNode, spawn(systolic_array,  sort, [IODevice,thirdNode, {secondNode,forthNode},-999,TimeInterval])),
    register(forthNode, spawn(systolic_array,  sort, [IODevice,forthNode, {thirdNode,fifthNode},-999,TimeInterval])),
    register(fifthNode, spawn(systolic_array,  sort, [IODevice,fifthNode, {forthNode,nil},-999,TimeInterval])),

    %% Pomp values to the first (leftmost) node:
    firstNode ! {pomper,2},
    firstNode ! {pomper,1},
    firstNode ! {pomper,5},
    firstNode ! {pomper,0},
    firstNode ! {pomper,3},
    
    firstNode  ! {pomper,start},
    secondNode ! {pomper,start},
    thirdNode  ! {pomper,start},
    forthNode  ! {pomper,start},
    fifthNode  ! {pomper,start},

    sleep(1000).


read(IODevice,CurrentNode,{LeftNode,RightNode},StoredNumber, FailedReadsAcc)->


    receive 

	%% Read what the left node sent to current node on each tick of current node's clock 

	{LeftNode,ReceivedNumber} when is_integer(ReceivedNumber)->

	    ?Trace(IODevice,string_format(" Phase#1: ~w <== ~w from ~w",[CurrentNode,ReceivedNumber, LeftNode],?LINE)),

	    case StoredNumber of

		-999->
		    {ReceivedNumber,0};

		_->
		    %% Keep the smaller in the node and pass the bigger to the right node
		    case StoredNumber < ReceivedNumber of 
			true -> 
			    ?Trace(IODevice,string_format(" Phase#1: ~w ==> ~w to ~w",[CurrentNode,ReceivedNumber, RightNode],?LINE)),
			    RightNode ! {CurrentNode,ReceivedNumber}, 
			    {StoredNumber,0};

			false ->
			    ?Trace(IODevice,string_format(" Phase#1: ~w ==> ~w to ~w",[CurrentNode,StoredNumber, RightNode],?LINE)),
			    RightNode ! {CurrentNode,StoredNumber},
			    {ReceivedNumber,0}
		    end
	    end;

	%% I had to put handler for stop here because this is the recieve statement that is waiting when stop message is sent

	{pomper,stop} -> ?Trace(IODevice,string_format(" Phase#1: ~w locally stored Number is ~w",[CurrentNode,StoredNumber],?LINE)),
			 stop;
	

	{_AnyOtherNode,Number}-> ?Trace(IODevice,string_format(" Phase#1: Message received from wrong Node: ~w <== ~w from ~w ",
							       [CurrentNode,Number,_AnyOtherNode],?LINE))

    after 0 -> 
	   {StoredNumber,FailedReadsAcc+1}
 
    end.


nodeClock (IODevice,CurrentNode,{LeftNode,RightNode}=Adjacents,StoredNumber,TimeInterval,FailedReadsAcc) ->

    receive 


    after 
	TimeInterval ->

	    {Value,NumberOfFailedReads} = read(IODevice,CurrentNode,Adjacents,StoredNumber,FailedReadsAcc),

	    case NumberOfFailedReads > 9 of

		true-> 

		    %% Time to start phase 2:
		    %% Here the idea is Each node keeps reading messages from its right node 
		    %% and as soon as it reads something, it substitute its local value 
		    %% with its received value and passes its local value to its left node.
		    %% The right most node has to start this process and shouldn't try to read
		    %% because there is no node on its right hand side. It also needs to pass a '-999' to its 
		    %% left node after it passed its local value. This is used as an indicator 
		    %% for each node saying that no more value comes from its right node.
		    %% When a node receives -999, it should pass its local value to its left 
		    %% node, saves -999 as its local value.
		    %% The left most node prints its local value when it receives a value 
		    %% from its right node, and substitute its local value with the recieved value
		    %% until it receives -999 which means process is done.

		    %% ?Trace(IODevice,string_format("~w locally stored Number is ~w",[CurrentNode,StoredNumber],?LINE)),

		    case RightNode of

			nil ->

			    ?Trace(IODevice,string_format(" Phase#2: ~w ==> ~w to ~w",[CurrentNode,StoredNumber, LeftNode],?LINE)),
			    LeftNode ! StoredNumber,

			    case StoredNumber=/=-999 of

				%% When rightmost node's stored value is Not -999
				true -> 
				    nodeClock(IODevice,CurrentNode,Adjacents,-999,TimeInterval,NumberOfFailedReads);

				%% When rightmost node's stored value is -999
				false -> stop

			    end;

			_NonNilRightNode ->

			    receive

				-999->

				    ?Trace(IODevice,string_format(" Phase#2: ~w <== ~w from ~w",[CurrentNode,-999, RightNode],?LINE)),
				    case LeftNode of

					%% when leftmost node receives -999
					pomper -> 

					    ?Trace(IODevice,string_format(" Phase#2: ~w outputs: ~w",[CurrentNode,StoredNumber],?LINE)),
					    stop;

					%% when a node in middle receives -999

					_OtherLeftNode ->

					    ?Trace(IODevice,string_format(" Phase#2: ~w ==> ~w to ~w",[CurrentNode,StoredNumber, LeftNode],?LINE)),
					    LeftNode ! StoredNumber,

					    ?Trace(IODevice,string_format(" Phase#2: ~w ==> ~w to ~w",[CurrentNode,-999, LeftNode],?LINE)),
					    LeftNode ! -999,
					    stop

				    end;


				ValueToOutput->

				    ?Trace(IODevice,string_format(" Phase#2: ~w <== ~w from ~w",[CurrentNode,ValueToOutput, RightNode],?LINE)),

				    case LeftNode of

					%% when leftmost node receives a value (other than -999) from its right node 
					pomper ->
					    ?Trace(IODevice,string_format(" Phase#2: ~w outputs: ~w",[CurrentNode,StoredNumber],?LINE)),
					    nodeClock(IODevice,CurrentNode,Adjacents,ValueToOutput,TimeInterval,NumberOfFailedReads);


					%% when a node in middle receives a value (other than -999) from its right node

       					_OtherLeftNode ->
					    ?Trace(IODevice,string_format(" Phase#2: ~w ==> ~w to ~w",[CurrentNode,StoredNumber, LeftNode],?LINE)),
					    LeftNode ! StoredNumber,
					    nodeClock(IODevice,CurrentNode,Adjacents,ValueToOutput,TimeInterval,NumberOfFailedReads)
				    end

			    after 0->
				    %% ?Trace(IODevice,string_format(" Phase#2: ~w: Nothing to read!",[CurrentNode],?LINE)),
				    nodeClock(IODevice,CurrentNode,Adjacents,StoredNumber,TimeInterval,NumberOfFailedReads)

			    end
		    end;

		false ->

		    case Value of 
			stop->stop;
			NewNumber -> nodeClock(IODevice,CurrentNode,Adjacents,NewNumber,TimeInterval,NumberOfFailedReads)

		    end
	    end
    end.

	    

sort(IODevice,CurrentNode,Adjacents,StoredNumber,TimeInterval)->

    %% This causes no node start reading data before all data is ready.

    receive
	{pomper,start}->

	    ?Trace(IODevice,string_format(" Phase#1: ~w is starting... ",[CurrentNode],?LINE)),

	    case nodeClock(IODevice,CurrentNode,Adjacents,StoredNumber,TimeInterval,0) of

		stop -> ?Trace(IODevice,string_format("~w is stopping ....",[CurrentNode],?LINE)),
	        void
	    end
    end.


sleep(T)->
    receive
    after T -> void
    end.


