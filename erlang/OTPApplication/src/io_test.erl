%% Author: Shahrdad
%% Created: Jun 5, 2010
%% Description: TODO: Add description to io_test


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
%% 												Standard Input/Output									 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% All Erlang processes have a default standard IO device. This device is used when no IoDevice argument is 
%% specified in the above function calls. However, it is sometimes desirable to use an explicit IoDevice argument 
%% which refers to the default IO device. This is the case with functions that can access either a file or the 
%% default IO device. The atom standard_io has this special meaning:

%% 							io:read(standard_io, 'enter>').

%% There is always a process registered under the name of user. This can be used for sending output to the user.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 											latin 1												%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ISO 8859-1 encodes what it refers to as "Latin alphabet no. 1," consisting of 191 characters from the Latin script. 
%% This character-encoding scheme is used throughout The Americas, Western Europe, Oceania, and much of Africa. 
%% It is also commonly used in most standard romanizations of East-Asian languages.
%% Each character is encoded as a single eight-bit code value. 

-module(io_test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([put_chars_test/2, copy_first_N_characters/3, echo/0, getopts_test/1]).
-export([setopts_test/2, write_term/2, read_term/1, read_term_at_line/2]).
-export([fwrite_test1/0, fwrite_test2/0, fwrite_test3/0, fwrite_test4/0]).
-export([fwrite_test5/0,fwrite_test6/0,fwrite_test7/0,fwrite_test8/0, fwrite_test9/0]).
-export([fwrite_test10/0,fwrite_test11/0,fwrite_test12/0,fwrite_test13/0]).
-export([fwrite_test14/0,fwrite_test15/0, fwrite_test16/0, fwrite_test17/0]).

-export([fread_test1/0,fread_test2/0,fread_test3/0,fread_test4/0]).
-export([fread_test5/0, fread_test6/0, fread_test7/0, fread_test8/0, fread_test9/0]).
-export([fread_test10/0, fread_test11/0, fread_test12/0, fread_test13/0]).
-export([fread_test14/0, fread_test15/0, fread_test16/0, scan_erl_exprs_test/2, scan_erl_form_test/2]).
%%
%% API Functions
%%

put_chars_test(Chars,FileName) -> case open_file(FileName,[write,binary]) of
									  error -> io:format("Unable to open file: ~p ~n", [FileName]);
									  IoDevice -> 
								  				io:put_chars(IoDevice, Chars),
								  				io:nl(IoDevice),
								  				io:put_chars(IoDevice, "Ha Ha Ha !!!"),
								  				file:close(IoDevice)
								  end.


copy_first_N_characters(FileNameRead, FilenNameWrite, N) -> 
	case open_file(FileNameRead, [read,binary]) of
		error 	-> io:format("Unable to open file: ~p ~n", [FileNameRead]);
		IoDeviceRead 	-> 
						case io:get_chars(IoDeviceRead, "", N) of
		
							eof ->	file:close(IoDeviceRead);
							
							{error,Reason} -> file:format_error(Reason);
		
							Data ->
								
								case open_file(FilenNameWrite,[write,binary]) of
									error 	-> io:format("Unable to open file: ~p ~n", [FilenNameWrite]);
									IoDeviceWrite ->
													io:put_chars(IoDeviceWrite, Data),
													file:close(IoDeviceWrite),
													file:close(IoDeviceRead)
								end
						end
	end.

						

echo () -> 
	case io:get_line("Type What you want to echo >") of

							eof ->	io:format("End of file!");
							
							{error,Reason} -> file:format_error(Reason);
		
							Data -> io:format("~p~n", [Data])
	end.


getopts_test(FileName) ->
	case open_file(FileName, [read]) of
		error 	-> io:format("Unable to open file: ~p ~n", [FileName]);
		IoDevice-> OptList = io:getopts(IoDevice),
				   io:format("~p~n", [OptList]),
				   file:close(IoDevice)
	end.


setopts_test(FileName, Opts) -> 
%% 	first create a file
	case open_file(FileName, [read]) of
		error 	-> io:format("Unable to open file: ~p ~n", [FileName]);
		IoDevice-> 
				io:setopts(IoDevice, Opts),
				case io:get_line(IoDevice) of
					eof ->	io:format("End of file!");
							
					{error,Reason} -> file:format_error(Reason);
		
					Data -> io:format("~p~n", [Data])
				end,
				file:close(IoDevice)
	end.


write_term(FileName, Term) -> 
	TermToWrite = list_to_term(Term),
	case open_file(FileName,[write,binary]) of
		  error -> io:format("Unable to open file: ~p ~n", [FileName]);
		  IoDeviceWrite -> 
					io:write(IoDeviceWrite,TermToWrite),
					file:close(IoDeviceWrite)
	end.

%% Each term in the file must end with a dot
read_term(FileName) ->				
	case open_file(FileName,[read,binary]) of
			error -> io:format("Unable to open file: ~p ~n", [FileName]);
			IoDeviceRead -> 
							case io:read(IoDeviceRead,"") of
								eof -> io:format("End of file ~p ~n" , [FileName]),
									   file:close(IoDeviceRead);

								{error, ErrorInfo} -> io:format("Error ~p ~n" , [ErrorInfo]),
									   				  file:close(IoDeviceRead);

								{ok , TermRead} -> io:format("Here is the term in the file: ~p ~n", [TermRead]),
											   file:close(IoDeviceRead)
							end
	end.

%% For some reason, this function only reads the first line in the file
read_term_at_line(FileName, N) -> 
	case open_file(FileName,[read,binary]) of
			error -> io:format("Unable to open file: ~p ~n", [FileName]);
			IoDeviceRead -> 
							case io:read(IoDeviceRead,"",N) of
								{eof, EndLine} -> io:format("End of file ~p ~p ~n" , [FileName, EndLine]),
									   file:close(IoDeviceRead);

								{error, ErrorInfo, EndLine} -> io:format("Error ~p ~p ~n" , [ErrorInfo, EndLine]),
									   				  file:close(IoDeviceRead);

								{ok , TermRead, EndLine} -> io:format("Term: ~p Endline: ~p ~n", [TermRead, EndLine]),
											   file:close(IoDeviceRead)
							end
	end.




scan_erl_exprs_test(FileName,StartLine) ->				
	case open_file(FileName,[read,binary]) of
			error -> io:format("Unable to open file: ~p ~n", [FileName]);
			IoDeviceRead -> 
							case io:scan_erl_exprs(IoDeviceRead,"", StartLine) of
								{eof , EndLine} -> io:format("End of file ~p ~n" , [FileName]),
									   				file:close(IoDeviceRead);

								{error, ErrorInfo, EndLine} -> io:format("Error ~p ~n" , [ErrorInfo]),
									   				  			file:close(IoDeviceRead);

								{ok, Tokens, EndLine} -> io:format("Here is the term in the file: ~p EndLine: ~p ~n", [Tokens,EndLine]),
											   			file:close(IoDeviceRead)
							end
	end.



scan_erl_form_test(FileName,StartLine) ->				
	case open_file(FileName,[read,binary]) of
			error -> io:format("Unable to open file: ~p ~n", [FileName]);
			IoDeviceRead -> 
							case io:scan_erl_form(IoDeviceRead,"", StartLine) of
								{eof , EndLine} -> io:format("End of file ~p ~n" , [FileName]),
									   				file:close(IoDeviceRead);

								{error, ErrorInfo, EndLine} -> io:format("Error ~p ~n" , [ErrorInfo]),
									   				  			file:close(IoDeviceRead);

								{ok, Tokens, EndLine} -> io:format("Here is the term in the file: ~p EndLine: ~p ~n", [Tokens,EndLine]),
											   			file:close(IoDeviceRead)
							end		
	end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 										fwrite(Format) ->										%
%% 								fwrite([IoDevice,] Format, Data) -> ok							%
%% 										format(Format) ->										%
%% 								format([IoDevice,] Format, Data) -> ok							%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The general format of a control sequence is ~F.P.PadModC.
%% The character C determines the type of control sequence to be used.
%% The characters F and P are optional numeric arguments. 
%% If F, P, or Pad is *, the next argument in Data is used as the numeric value of F or P.

%% F is the field width of the printed argument. 
%% A negative value means that the argument will be left justified within the field, 
%% otherwise it will be right justified. 
%% If no field width is specified, the required print width will be used. 
%% If the field width specified is too small, then the whole field will be filled with * characters.

%% P is the precision of the printed argument. 
%% A default value is used if no precision is specified. 
%% The interpretation of precision depends on the control sequences. 
%% Unless otherwise specified, the argument within is used to determine print width.

%% Pad is the padding character. 
%% This is the character used to pad the printed representation of the argument so that 
%% it conforms to the specified field width and precision. 
%% Only one padding character can be specified and, whenever applicable, 
%% it is used for both the field width and precision. 
%% The default padding character is ' ' (space).

%% Mod is the control sequence modifier. 
%% It is either a single character (currently only 't', for unicode translation, is supported) 
%% that changes the interpretation of Data.
%% Here they are:

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = c  ===> ~F.P.Padc												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The corresponding argument should be a number that will be interpreted as an ASCII code. 
%% P is the number of times the character is printed and 
%% it defaults to the field width, which in turn defaults to 1. 
%% |     aaaaa|bbbbb     |ccccc|

fwrite_test1() -> io:fwrite("|~10.5c|~-10.5c|~5c|~n", [$a, $b, $c]).
fwrite_test2() -> io:fwrite("|~10.5.#c|~-10.5._c|~5c|~n", [$a, $b, $c]).


%% If you don't use 't' (Unicode translation modifier) 
%% The corresponding argument should be an integer less than or equal to 255, 
%% otherwise it is masked with 16#FF:

fwrite_test3() -> io:fwrite("~c~n",[1024]).


%% However if the Unicode translation modifier ('t') is in effect, 
%% the corresponding integer argument can be any number representing a valid unicode 
%% codepoint:

fwrite_test4() -> io:fwrite("~tc~n",[1024]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = f  ===> ~F.P.Padf												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The corresponding argument is a float which is written as [-]ddd.ddd. 
%% P is the number of is the number of digits after the decimal point. 
%% The default precision is 6 and it cannot be less than 1.


fwrite_test5() -> io:fwrite("~15.4.#f|~-15.4._f|~15.2f|~15.9f|~n",
							[123456.1234, 123456.1234, 123456.1234, 123456.1234]).


%% Note that the last argument is formatted as ~15.9f which means that 
%% the total field width of the printed argument is 15 and 9 position of which 
%% is dedicated to precision. Therefore there is only 6 elements left for 
%% "123456." which is apperently small. Since the field width specified is too small, 
%% the whole field will be filled with * characters.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = e  ===> ~F.P.Pade												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The argument is a float which is written as [-]d.ddde+-ddd. 
%% P is the number of digits written. 
%% The default precision is 6 and it cannot be less than 2.

%% Note 1: e+-ddd means that there cannot be more than 3 digits. 
%% I.e. e+1234 is wrong


fwrite_test6() -> io:fwrite("~20.8.#e|~-20.10.#e|~20.15.#e|~21.15.#e|~n",
					[1234.1234e+123, 1234.1234e+123, 1234.1234e+123, 1234.1234e+123]).

%% Note 2: The format ~20.15.#e means the total length of field is 20 characters and 
%% 15 position of which is reserved for digits. The argument is 1234.1234e+123 which 
%% has 11 digits. Since Erlang converts 1234.1234e+123 to 1.23412340000000e+126 which 
%% takes 18 character(digit) and leaves 2 character for the rest ('.' , 'e' and '+' ) which 
%% is apperantly small. Therefore it will be printed as stars.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = g  ===> ~F.P.Padg												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The argument should be a float., 
%% if it is >= 0.1 and < 10000.0 the argument will be written as f. 
%% if the argument doesn't fall in that range , it is written in the e format. 
%% P is the number of significant digits. It defaults to 6 and should not be less than 2. 
%% If the absolute value of the float does not allow it to be written in the f format 
%% with the desired number of significant digits, it is also written in the e format.

fwrite_test7() -> io:fwrite("~15.3.#g|~-15.5.#g|~18.8.#g|~10.2.#g|~n",
					[1234.1234, 1234.1234, 1234.1234, 1234.1234]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = s  ===> ~F.P.Pads												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Prints the corresponding argument with the string syntax.

%% This format can be used for printing any object and truncating the output so 
%% it fits a specified field: the printed argument is truncated to the given precision 
%% and field width.

%% The argument should be:
%% _if no Unicode translation modifier is present, an I/O list, a binary, or an atom:

fwrite_test8() -> io:fwrite("|~12s|~n", [io_lib:write({hey, hey, hey})]).

%% 	_If the Unicode translation modifier ('t') is in effect, the argument should be a chardata(), 
%% 		meaning that binaries are in UTF-8. The characters are printed without quotes. 
%% A list with integers larger than 255 is considered an error if the Unicode 
%% translation modifier is not given:

fwrite_test9() -> io:fwrite("~ts~n",[[1024]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = p  ===> ~F.P.Padp												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Writes data with the standard syntax. This is used to output Erlang terms. 
%% Atoms are printed within quotes if they contain embedded non-printable characters, 
%% and floats are printed accurately as the shortest, correctly rounded string.
%% this format breaks terms whose printed representation is longer than one line into many 
%% lines and indents each line sensibly. It also tries to detect lists of printable 
%% characters and to output these as strings.

%% F specifies the maximum line length. It defaults to 80. 
%% The precision (P) specifies the initial indentation of the term

fwrite_test10() -> 
	T = [{attributes,[[{id,age,1.50000},{mode,explicit},
					{typename,"INTEGER"}], [{id,cho},{mode,explicit},{typename,'Cho'}]]},
					{typename,'Person'},{tag,{'PRIVATE',3}},{mode,implicit}],
					io:fwrite("~60.10p~n", [T]).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = P  ===> ~F.P.PadP												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Writes data in the same way as ~p, 
%% but takes an extra argument which is the maximum depth to which terms are printed. 
%% Anything below this depth is replaced with ....

fwrite_test11() -> 
	T = [{attributes,[[{id,age,1.50000},{mode,explicit},
					{typename,"INTEGER"}], [{id,cho},{mode,explicit},{typename,'Cho'}]]},
					{typename,'Person'},{tag,{'PRIVATE',3}},{mode,implicit}],
					io:fwrite("~60.10P~n", [T,10]).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = B  ===> ~F.P.PadB												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Writes an integer in base 2..36, the default base is 10. 
%% A leading dash is printed for negative integers.
%% F is the total field length.
%% P is field selects base.


fwrite_test12() -> io:fwrite("~10.16.#B~n", [31]),
				   io:fwrite("~10.2B~n", [-19]),
					io:fwrite("~-10.36._B~n", [5*36+35]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = X  ===> ~F.P.PadX												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Like B, but takes an extra argument that is a prefix to insert before the number, 
%% but after the leading dash, if any.
%% 
%% The prefix can be a possibly deep list of characters or an atom.

fwrite_test13() -> io:fwrite("~X~n", [31,"10#"]),
				   io:fwrite("~10.16.#X~n", [-31,"0x"]),
				   io:fwrite("~-10.16.#X~n", [-31,"0x"]),
				   io:fwrite("~-30.16._X~n", [-31,"integer in base 16:"]).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = #  ===> ~F.P.Pad#												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Like B, but prints the number with an Erlang style '#'-separated base prefix.
fwrite_test14() -> io:fwrite("~10#~n", [31]),
				   io:fwrite("~20.16._#~n", [-31]).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = b  ===> ~F.P.Padb												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Like B, but prints lowercase letters.
fwrite_test15() -> io:fwrite("~10.16.#b~n", [31]),
				   io:fwrite("~10.2b~n", [-19]),
					io:fwrite("~-10.36._b~n", [5*36+35]).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = x  ===> ~F.P.Padx												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Like X, but prints lower case letters.

fwrite_test16() -> io:fwrite("~x~n", [31,"10#"]),
				   io:fwrite("~10.16.#x~n", [-31,"0X"]),
				   io:fwrite("~-10.16.#x~n", [-31,"0X"]),
				   io:fwrite("~-30.16._x~n", [-31,"INTEGER BASE 16:"]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					 ModC = +  ===> ~F.P.Pad+												 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Like #, but prints lower case letters.
fwrite_test17() -> io:fwrite("~10+~n", [31]),
				   io:fwrite("~20.16._+~n", [-31]).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 					fread([IoDevice,] Prompt, Format) -> Result								 %	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Reads characters from the standard input (IoDevice), prompting it with Prompt. 
%% Interprets the characters in accordance with Format. 
%% Format contains control sequences which directs the interpretation of the input.
%% 
%% Format may contain:
%% 
%%     * White space characters (SPACE, TAB and NEWLINE) which cause input to be read to 
%% 		the next non-white space character.
%% 
%%     * Ordinary characters which must match the next input character.
%% 
%%     * Control sequences, which have the general format ~*FMC.
%% 
%% 		The character * is an optional return suppression character. 
%% 		It provides a method to specify a field which is to be omitted.
%% 
%% 		F is the field width of the input field.
%% 
%% 		M is an optional translation modifier (of which 't' is the only currently supported, 
%% 		meaning Unicode translation).
%% 
%% 		C determines the type of control sequence.
%% 
%%    	* Unless otherwise specified, leading white-space is ignored for all control sequences. 
%% 		An input field cannot be more than one line wide.
%% 
%% 		The following control sequences are available:



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 									C = ~  ===> ~*FM~										 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A single ~ is expected in the input.
fread_test1()-> io:fread("Prompt> ","~1~").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 									C = d  ===> ~*FMd										 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A decimal integer is expected.
fread_test2()-> io:fread("Prompt> ","~1d").
fread_test3()-> io:fread("Prompt> ","~3d").
fread_test4()-> io:fread("Prompt> ","~6d").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 									C = u  ===> ~*FMu										 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% An unsigned integer in base 2..36 is expected. 
%% The field width parameter is used to specify base. 
%% Leading white-space characters are not skipped.

fread_test5()-> io:fread("Prompt> ","~8u").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 									C = -  ===> ~*FM-										 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% An optional sign character is expected. A sign character '-' gives the return value -1. 
%% Sign character '+' or none gives 1. The field width parameter is ignored. 
%% Leading white-space characters are not skipped.

fread_test6()->io:fread("Prompt> ","~-").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 									C = #  ===> ~*FM#										 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% An integer in base 2..36 with Erlang-style base prefix (for example "16#ffff") is expected.

fread_test7()->io:fread("Prompt> ","~#").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 									C = f  ===> ~*FMf										 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A floating point number is expected. It must follow the Erlang floating point number syntax.

fread_test8()->io:fread("Prompt> ","~f").

fread_test14() -> io:fread('enter>', "~f~f~f").

fread_test15() -> io:fread('enter>', "~10f~d").

fread_test16() -> io:fread('enter>', ":~10s:~10c:").
%% enter>:   alan   :   joe    :




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 									C = s  ===> ~*FMs										 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A string of non-white-space characters is read. 
%% F: number of characters to be read and all trailing white-space characters are stripped. 
%% An Erlang string (list of characters) is returned.
%% Note that if you mention 'F' then you cannot enter less than 'F' characters.
%% If Unicode translation is in effect (~ts), characters larger than 255 are accepted, 
%% otherwise not. 
%% With the translation modifier, the list returned may as a consequence also contain integers larger 
%% than 255:

fread_test9() -> io:fread("Prompt> ","~5s").


fread_test10() -> io:fread("Prompt> ","~6ts").




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 									C = a  ===> ~*FMa										 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Similar to s, but the resulting string is converted into an atom.
%% The Unicode translation modifier is not allowed (atoms can not contain characters beyond the latin1 range).
%% Note that if you mention 'F' then you cannot enter less than 'F' characters.
fread_test11() -> io:fread("Prompt> ","~5a").





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 									C = c  ===> ~*FMc										 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The 'F' number of characters equal to the field width are read (default is 1) and returned as an Erlang string.
%% Leading and trailing white-space characters are NOT omitted (as they are with s).
%% Note that if you mention 'F' then you cannot enter less than 'F' characters.
%% The Unicode translation modifier works as with s.

fread_test12() -> io:fread("Prompt> ","~15c").

fread_test13() -> io:fread("Prompt> ","~6tc").






%%
%% Local Functions
%%

open_file(FileName,Mode) ->
	case file:open(FileName, Mode) of 
		{ok,IoDevice} -> IoDevice;
		{error,Reason} -> io:format("File Open Error: ~p ~n",[file:format_error(Reason)]), error
	end.


list_to_term(TermInString) -> 
	case erl_scan:string(TermInString++".") of
		{ok,T,_} -> case erl_parse:parse_term(T) of
						{ok, Term} ->  Term;
						{error,Error} -> Error
					end;
		X -> X
	
	end.
