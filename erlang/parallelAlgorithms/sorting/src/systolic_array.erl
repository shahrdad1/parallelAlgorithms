%% See Page #6 from Leighton

-module(systolic_array).
-export([sort/6]).
-include("trace.hrl").


read(Logger,CurrentNode,{LeftNode,RightNode},StoredNumber, FailedReadsAcc,DataPomper)->


    receive 

	%% Read what the left node sent to current node on each tick of current node's clock 

	{LeftNode,ReceivedNumber} when is_integer(ReceivedNumber)->

	    Logger ! {info,?Log_Line("Phase#1: ~w <== ~w from ~w", [CurrentNode,ReceivedNumber, LeftNode],?LINE)},

	    case StoredNumber of

		-999->
		    {ReceivedNumber,0};

		_->
		    %% Keep the smaller in the node and pass the bigger to the right node
		    case StoredNumber < ReceivedNumber of 
			true -> 
			    Logger ! {info,?Log_Line("Phase#1: ~w ==> ~w to ~w", [CurrentNode,ReceivedNumber, RightNode],?LINE)},
			    RightNode ! {CurrentNode,ReceivedNumber}, 
			    {StoredNumber,0};

			false ->
			    Logger ! {info,?Log_Line("Phase#1: ~w ==> ~w to ~w", [CurrentNode,StoredNumber, RightNode],?LINE)},
			    RightNode ! {CurrentNode,StoredNumber},
			    {ReceivedNumber,0}
		    end
	    end;

	%% I had to put handler for stop here because this is the recieve statement that is waiting when stop message is sent

	{{DataPomper,_NodeName},stop} -> 
	    Logger ! {info,?Log_Line("Phase#1: ~w locally stored Number is ~w", [CurrentNode,StoredNumber],?LINE)},
	    stop;


	{_AnyOtherNode,Number}-> 
	    Logger ! {info,?Log_Line("Phase#1: Message received from wrong Node: ~w <== ~w from ~w ", [CurrentNode,Number,_AnyOtherNode],?LINE)}

    after 0 -> 
	    {StoredNumber,FailedReadsAcc+1}

    end.


nodeClock (Logger,CurrentNodeProcessName,{LeftNode,RightNode}=Adjacents,StoredNumber,TimeInterval,FailedReadsAcc,DataPomper) ->

    receive 


    after 
	TimeInterval ->

	    {Value,NumberOfFailedReads} = read(Logger,CurrentNodeProcessName,Adjacents,StoredNumber,FailedReadsAcc,DataPomper),

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

		    %% ?Trace(IODevice,string_format("~w locally stored Number is ~w",[CurrentNodeProcessName,StoredNumber],?LINE)),

		    case RightNode of

			nil ->

			    Logger ! {info,?Log_Line("Phase#2: ~w ==> ~w to ~w",[CurrentNodeProcessName,StoredNumber, LeftNode],?LINE)},

			    LeftNode ! StoredNumber,

			    case StoredNumber=/=-999 of

				%% When rightmost node's stored value is Not -999
				true -> 
				    nodeClock(Logger,CurrentNodeProcessName,Adjacents,-999,TimeInterval,NumberOfFailedReads,DataPomper);

				%% When rightmost node's stored value is -999
				false -> stop

			    end;

			_NonNilRightNode ->

			    receive

				-999->

				    Logger ! {info,?Log_Line("Phase#2: ~w <== ~w from ~w",[CurrentNodeProcessName,-999, RightNode],?LINE)},

				    case LeftNode of

					%% when leftmost node receives -999
					{DataPomper,_NodeName} -> 

					    Logger ! {info,?Log_Line("Phase#2: ~w outputs: ~w",[CurrentNodeProcessName,StoredNumber],?LINE)},

					    stop;

					%% when a node in middle receives -999

					_OtherLeftNode ->

					    Logger ! {info,?Log_Line("Phase#2: ~w ==> ~w to ~w",
								     [CurrentNodeProcessName,StoredNumber, LeftNode],?LINE)},

					    LeftNode ! StoredNumber,

					    Logger ! {info,?Log_Line("Phase#2: ~w ==> ~w to ~w",
								     [CurrentNodeProcessName,-999, LeftNode],?LINE)},

					    LeftNode ! -999,
					    stop

				    end;


				ValueToOutput->

				    Logger ! {info,?Log_Line("Phase#2: ~w <== ~w from ~w",
								     [CurrentNodeProcessName,ValueToOutput, RightNode],?LINE)},

				    case LeftNode of

					%% when leftmost node receives a value (other than -999) from its right node 
					{DataPomper,_NodeName} ->


					    Logger ! {info,?Log_Line("Phase#2: ~w outputs: ~w",
								     [CurrentNodeProcessName,StoredNumber],?LINE)},

					    nodeClock(Logger,CurrentNodeProcessName,Adjacents,ValueToOutput,
						      TimeInterval,NumberOfFailedReads,DataPomper);


					%% when a node in middle receives a value (other than -999) from its right node

       					_OtherLeftNode ->

					    Logger ! {info,?Log_Line("Phase#2: ~w ==> ~w to ~w",
								     [CurrentNodeProcessName,StoredNumber, LeftNode],?LINE)},

					    LeftNode ! StoredNumber,
					    nodeClock(Logger,CurrentNodeProcessName,Adjacents,ValueToOutput,
						      TimeInterval,NumberOfFailedReads,DataPomper)
				    end

			    after 0->
				    nodeClock(Logger,CurrentNodeProcessName,Adjacents,StoredNumber,TimeInterval,NumberOfFailedReads,DataPomper)

			    end
		    end;

		false ->

		    case Value of 
			stop->stop;
			NewNumber -> nodeClock(Logger,CurrentNodeProcessName,Adjacents,NewNumber,TimeInterval,NumberOfFailedReads,DataPomper)

		    end
	    end
    end.

	    

sort({LoggerProcessName,LoggerNode}=Logger,
     {CurrentNode,CurrentProcessName}=CurrentNodeProcessName,
     {LeftNodeProcessName,RightNodeProcessName}=Adjacents,
     StoredNumber,TimeInterval,DataPomper)->

    %% This causes no node start reading data before all data is ready.
    register(CurrentNode,self()),

    Logger ! {info,?Log_Line("Pid: ~w is Registered under the name:~w",[self(),CurrentProcessName],?LINE)},

    receive
	{{DataPomper,_NodeName}=Master,start}->

	    Logger ! {info,?Log_Line("Phase#1: ~w with node ~w on its left and node ~w on its right is starting . . .",
				     [CurrentNodeProcessName,LeftNodeProcessName,RightNodeProcessName],?LINE)},

	    case nodeClock(Logger,CurrentNodeProcessName,Adjacents,StoredNumber,TimeInterval,0,DataPomper) of

		stop -> 
		    Logger ! {info,?Log_Line("~w is stopping . . . ",[CurrentNodeProcessName],?LINE)},

	        void
	    end
    end.


sleep(T)->
    receive
    after T -> void
    end.
