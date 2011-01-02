%% Author: sshadab
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 											latin 1												%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ISO 8859-1 encodes what it refers to as "Latin alphabet no. 1," consisting of 191 characters from the Latin script. 
%% This character-encoding scheme is used throughout The Americas, Western Europe, Oceania, and much of Africa. 
%% It is also commonly used in most standard romanizations of East-Asian languages.
%% Each character is encoded as a single eight-bit code value. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% 												Standard Input/Output									 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% All Erlang processes have a default standard IO device. This device is used when no IoDevice argument is 
%% specified in the above function calls. However, it is sometimes desirable to use an explicit IoDevice argument 
%% which refers to the default IO device. This is the case with functions that can access either a file or the 
%% default IO device. The atom standard_io has this special meaning:

%% 							io:read(standard_io, 'enter>').

%% There is always a process registered under the name of user. This can be used for sending output to the user.

%% Created: 2010-05-20
%% Description: TODO: Add description to file_test
-module(file_test).
-include_lib("kernel/include/file.hrl").
%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test_random_write/0,eval_test/1, test_read_id3_tag/0, read_test/1, read_file_test/1, read_file_info_test/1,read_line_test/2]).
-export([script_test/1, truncate_test/1,write_test/2, write_file_test/3]).
-import(lists, [filter/2, map/2, reverse/1]).
%%
%% API Functions
%%

%% Open/create a new file in current folder with a given name and given Mode and write to it:
%% Usage: file_test:random_write("c:/Test.txt", [write,{encoding, utf8}]).
random_write(FileName,Mode) ->
	case file:open(FileName, Mode) of 
		{ok,IoDevice} -> 
			case file:pwrite(IoDevice, 10,"Shahrdad!") of
				ok -> file:close(IoDevice);
				{error,Reason} -> io:format("Write Error: ~p ~n",[file:format_error(Reason)])
			end;
		{error,Reason} -> io:format("File Open Error: ~p ~n",[file:format_error(Reason)])
	end.

test_random_write() -> random_write("c:/Test.txt", [write,{encoding, utf8}]).
										 
%% Reads and evaluates Erlang expressions, separated by '.' 
%% (or ',', a sequence of expressions is also an expression), from Filename
eval_test(X) -> case file:eval(X) of
				   	ok -> io:format("File ~p evaluated susccessfully.~n" , [X]),
						  KeyValueList = consult_test(X),
						  lists:map(fun (KeyValueTuple) -> {Key, Value} = KeyValueTuple,
											 io:format("~p : ~p ~n ", [Key, Value]) 
									end, 
									KeyValueList);
						  
					{error, ErrorCode} -> io:format("Error Code ~p happend when evaluating file ~p ~n ", [ErrorCode, X]);
					{error, {Line, Mod, Term}} -> io:format("Error @ line# ~p , Module: ~p , term: ~p ~n ",[Line, Mod, Term])
				end.

					
consult_test(Filename) ->
	case file:consult(Filename) of
		{ok, Terms} -> Terms;
		{error, ErrorCode} -> ErrorCode;
		{error, Error} -> Error;
		{error, {Line , Mod, Term}} -> io:format("Error @ line# ~p , Module: ~p , term: ~p ~n ",[Line, Mod, Term])
	end.
	
read_test(FileName) ->
	case file:open(FileName, [read,binary]) of
		{ok, S} -> 
			case file:read(S, 200) of
				{ok, Data} -> binary_to_list(trim(Data));
				eof -> file:close(S);
				{error, Reason} -> file:format_error(Reason)
			end;
		
		{error, Reason} -> file:format_error(Reason)
	end.
		
		
read_file_test(FileName) -> case file:read_file(FileName) of
								{ok, DataL} -> binary_to_list(trim(DataL));
								{error, Reason} -> file:format_error(Reason)
							end.
				   

read_file_info_test(FileName) -> case file:read_file_info(FileName) of
									 {ok, FileInfo} -> [{type, FileInfo#file_info.type},
														{size, FileInfo#file_info.size},
														{access, FileInfo#file_info.access},
														{lastTimeFileRead, FileInfo#file_info.atime},
														{latTimeFileWritten, FileInfo#file_info.mtime},
														{mode, FileInfo#file_info.mode}
														];
									 {error, Reason} -> file:format_error(Reason)
								 end.



%% Reads a line of bytes/characters from the file referenced by IoDevice. 
%% Lines are defined to be delimited by the linefeed (LF, \n) character, 
%% but any carriage return (CR, \r) followed by a newline is also treated 
%% as a single LF character (the carriage return is silently ignored). 
%% The line is returned including the LF, but excluding any CR immediately followed by a LF.



read_line_test(FileName, FirstNLines) ->
	
	
	case file:open(FileName, [read,binary]) of
		
		{ok, S} ->  NLines = read_n_lines(S,FirstNLines, []),
					lists:map(fun (Line) -> io:format(" => ~p~n", [Line]) end , NLines);
				
		
		{error, Reason} -> file:format_error(Reason)
	end.

read_n_lines (S,FirstNLine, LinesAcc) ->
	
	case FirstNLine == 0 of
		
		true -> lists:reverse(LinesAcc);
		
		false -> case file:read_line(S) of 
					 
					{ok, Data} -> read_n_lines (S,FirstNLine -1, [binary_to_list (Data)|LinesAcc]);
					 
					eof -> file:close(S),
						   lists:reverse(LinesAcc);

					{error, Reason} -> file:format_error(Reason)
				 end
	end.
								   
		
	
script_test(FileName) -> case file:script(FileName) of
							 {ok, Value} -> io:format("Value of the last expression is: ~p ~n", [Value]);
							 {error, Reason} -> file:format_error(Reason)
						 end.

truncate_test(FileName) -> 	case file:open(FileName, [read,binary]) of
								{ok, S} -> case file:truncate(S) of 
											   ok -> io:format("Truncate was successfull."),
													 file:close(S);
											   {error, Reason} -> file:format_error(Reason)
										   end;
								{error, Reason} -> file:format_error(Reason)
							end.

write_test(FileName, Text) -> case file:open(FileName, [write,binary, raw]) of
								{ok, S} -> case file:write(S, list_to_binary(Text)) of 
											   ok -> io:format("Write was successfull.");
											   {error, Reason} -> file:format_error(Reason)
										   end,
											file:close(S);
								{error, Reason} -> file:format_error(Reason)
							end.

write_file_test(FileName, Bytes, Modes) -> case file:write_file(FileName, Bytes, Modes) of
										   
											   ok -> io:format("Write was successfull.");
											   
											   {error, Reason} -> file:format_error(Reason)
										   end.

%% {<<16#12345678:32/big>>,<<16#12345678:32/little>>,<<16#12345678:32/native>>,<<16#12345678:32>>}.



%% The bitorder in ID3v2 is most significant bit first (MSB). The
%%    byteorder in multibyte numbers is most significant byte first (e.g.
%%    $12345678 would be encoded $12 34 56 78), also known as big endian
%%    and network byte order.

%% The ID3v2 tag header, which should be the first information in the file, is 10 bytes as follows: 
%% (http://www.id3.org/id3v2.3.0)
%% ID3v2/file identifier   "ID3" 
%% ID3v2 version           $03 00
%% ID3v2 flags             %abc00000 
%% ID3v2 size              4 * %0xxxxxxx

test_read_id3_tag() -> read_id3_tag("c:/Gypsy-Allegria.mp3").

read_id3_tag(File) -> 
	
%% 	binary : Read operations on the file will return binaries rather than lists.
	
%% 	raw    : Allows faster access to a file and has the following limitations:
%% 
%% 			1- The functions in the io module cannot be used. 
%% 				Instead, use the read/2, read_line/1 and write/2 functions.
%% 
%% 			2- Especially if read_line/1 is to be used on a raw file, 
%% 				it is recommended to combine this option with the 
%% 				{read_ahead, Size} option as line oriented I/O is 	
%% 				inefficient without buffering.
%% 
%% 			3- Only the Erlang process which opened the file can use it. 
%% 				A remote Erlang file server cannot be used; 
%% 				the computer on which the Erlang node is running must have access to the file system 
%% 				(directly or through NFS). 
%% 

	case file:open(File, [read,binary,raw]) of
		
		{ok,IoDevice} -> Size = filelib:file_size(File),
						 
						 Result = parse_header_tag(read_portion(IoDevice, 0, 6)),
						{"ID3v2.", [{version1,_},{version2,_}, {unsynchronisation, _},{extendedHeader, ExtendedHeader},
							{experimentalIndicator, _}, {restOfTheFlags,_}]}=Result,
						 
						io:format("Line 64"), 
						Result1 = parse_header_size_tag(read_portion(IoDevice, 6, 4)),
						 
						case ExtendedHeader of
							%%	NO EXTENDED HEADER, therefore text information frame starts here
							
							<<0:1>> -> Result2 = parse_Frame_header(read_portion(IoDevice, 10, 10)),
									   <<TextEncoding/binary>> = read_portion(IoDevice, 20, 1),
									   Result3 = {textEncoding, TextEncoding},
									   Result4 = read_to_null(IoDevice,21,[]),
									   file:close(IoDevice),
									   {Result,Result1,Result2,Result3,Result4};
						 
						 	_ 		-> io:format("Extended Header not supported Yet!")
						 end;
		
		{error,Reason} -> io:format("File Open Error: ~p ~n",[file:format_error(Reason)])
	end.


read_portion(IoDevice, From, Length) when is_integer(From) , is_integer(Length), From >= 0, Length >0 ->
		case file:pread(IoDevice, From, Length) of
							
			eof  		-> file:close(IoDevice);
			{error, Reason} -> io:format("Unable to read the file due to ~p ~n", file:format_error(Reason));
			{ok, DataL} -> DataL
		
		
		end;

read_portion(_,_,_) -> error.	


parse_header_tag(<<$I,$D,$3, 
			   Version1:8/big-unsigned-integer-unit:1,
			   Version2:8/big-unsigned-integer-unit:1,
			   Unsynchronisation:1/bitstring-unit:1,
			   ExtendedHeader:1/bitstring-unit:1,
			   ExperimentalIndicator:1/bitstring-unit:1,
			   RestOfTheFlags:5/bitstring-unit:1
			>>) ->

	case Unsynchronisation of 
		<<0:1/unsigned-integer-unit:1>> -> io:format("Unsynchronisation flag: false ~n");
		<<1:1/unsigned-integer-unit:1>> -> io:format("Unsynchronisation flag: true ~n")
	end,
	case ExtendedHeader of 
		<<0:1/unsigned-integer-unit:1>> -> io:format("ExtendedHeader flag: false ~n");
		<<1:1/unsigned-integer-unit:1>> -> io:format("ExtendedHeader flag: true ~n")
	end,
	if  
 		ExperimentalIndicator =:= <<0:1/unsigned-integer-unit:1>> -> io:format("Unsynchronisation flag: false ~n");
 		ExperimentalIndicator =:= <<1:1/unsigned-integer-unit:1>> -> io:format("Unsynchronisation flag: true ~n")
	end,

 	{"ID3v2.", [{version1,Version1},{version2,Version2},
			  {unsynchronisation, Unsynchronisation},
			  {extendedHeader, ExtendedHeader},
			  {experimentalIndicator, ExperimentalIndicator},
			  {restOfTheFlags,RestOfTheFlags}
			  ]};

parse_header_tag(_) -> error.


%% The ID3v2 tag size is encoded with four bytes where the most 
%% significant bit (bit 7) is set to zero in every byte, making 
%% a total of 28 bits. The zeroed bits are ignored, so a 257 bytes 
%% long tag is represented as $00 00 02 01.

%% First convert the 4 byte value to the binary number system, 
%% then rewrite the number by removing the 7th bit from each byte, 
%% acquiring a 28-bit number defining the length of the tag.


parse_header_size_tag(<<_Msb1:1/unsigned-bitstring-unit:1,Part1:6/unsigned-bitstring-unit:1,Lsb1:1/unsigned-bitstring-unit:1,
						_Msb2:1/unsigned-bitstring-unit:1,Part2:6/unsigned-bitstring-unit:1,Lsb2:1/unsigned-bitstring-unit:1,
						_Msb3:1/unsigned-bitstring-unit:1,Part3:6/unsigned-bitstring-unit:1,Lsb3:1/unsigned-bitstring-unit:1,
			   			_Msb4:1/unsigned-bitstring-unit:1,Part4:7/unsigned-bitstring-unit:1
					>>) ->
	
	<<TagSize:32/big-integer-unsigned-unit:1>> = 
		<<0:4/big-integer-unsigned-unit:1,Part1:6/unsigned-bitstring-unit:1,Lsb1:1/unsigned-bitstring-unit:1,
		  Part2:6/unsigned-bitstring-unit:1,Lsb2:1/unsigned-bitstring-unit:1,
		  Part3:6/unsigned-bitstring-unit:1,Lsb3:1/unsigned-bitstring-unit:1,
		  Part4:7/unsigned-bitstring-unit:1>>,
	
 	{"Header tag Size", [{tagSize, TagSize}]};


parse_header_size_tag(_) -> error.

%% Frame ID       		$xx xx xx xx (four characters)
%% Size           		$xx xx xx xx
%% Flag Header 2 bytes: abc00000 ijk00000

%% The frame ID is followed by a size descriptor, making a total header size of ten bytes 
%% in every frame. The size is calculated as frame size excluding frame header 
%% (frame size - 10).
parse_Frame_header(<<$T,ID:3/binary, 
			   FrameSize:32/big-unsigned-integer-unit:1,
			   A:1/bitstring-unit:1,B:1/bitstring-unit:1,C:1/bitstring-unit:1,0:5,
			   I:1/bitstring-unit:1,J:1/bitstring-unit:1,K:1/bitstring-unit:1,0:5
			 >>) ->
	Artist = 
		case binary_to_list(ID) of 
			"PE1" -> "Lead artist(s)/Lead performer(s)/Soloist(s)/Performing group";
			"PE2" -> "Band/Orchestra/Accompaniment"; 
			"PE3" -> "Conductor"; 
			"PE4" -> "Interpreted" 
		end,
	
	
	[{artist,Artist}, {frameSize,FrameSize}, {tagAlterPreservation, A},
	 {fileAlterPreservation,B},{readOnly,C},{compression,I}, {encryption,J},{groupingIdentity,K}];

parse_Frame_header(_) -> error.


%%  Text frames have one byte just after the header, that represents the text encoding; 
%% 	the actual text follows this byte (ID3v2.3 Sec 4.2.) 
%% 	Then comes an optional short content description terminated by a NULL byte, 
%%  after which the actual comment text is stored. (ID3v2.3 Sec 4.11.)
	
read_to_null(IoDevice,Position, Accum) ->
	
		<<CurrentByte/binary>> = read_portion(IoDevice, Position, 1),

		case CurrentByte of
			%% Null ckaracter in the byte stream can be either of
			%% <<0:8/big-unsigned-integer-unit:1>> or <<$\0>> or <<0>>

			<<$\0>>	-> {text, lists:reverse(Accum)};
			<<CurrentCharacter:8/big-unsigned-integer-unit:1>> 	-> 
				read_to_null(IoDevice,Position+1, [CurrentCharacter|Accum])
		end.
	




%% A nice trick to trim the spaces from a binary
trim(Bin) -> list_to_binary(trim_blanks(binary_to_list(Bin))).

trim_blanks(L)-> reverse(skip_blanks_and_zero(reverse(L))).

%% Note that $\s is 'Space' character (just like $a)

skip_blanks_and_zero([$\s|T]) -> skip_blanks_and_zero(T);
skip_blanks_and_zero([0|T]) -> skip_blanks_and_zero(T);
skip_blanks_and_zero (T) -> T.



%%
%% Local Functions
%%

