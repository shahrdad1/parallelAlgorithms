
-module(string_test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([span_test/0,cspan_test/0, token_test1/0, token_test2/0, join_test/0, chars_test/0, chars_test1/0]).
-export([copies_test/0,words_test/0, sub_word_test/0, strip_test/0, to_float_test1/0, to_float_test2/0]).
-export([left_test/0, right_test/0, centre_test/0, sub_string_test/0,substr_test/0,to_integer_test1/0,to_integer_test2/0]).
%%
%% API Functions
%%


%% This is how span(String1, String2) works:
%% String1 is being scanned from the left to right, each charachter is checked against all 
%% characters in String2. if it was fornd , then counter counts it and it goes
%% to next character. This process stops when it hits the first character which cannot 
%% be found in String2.

span_test() -> io:format("~p~n",[string:span("\t    abcdef", "a \t")]),
			   io:format("~p~n",[string:span("\t    abcdef", " \t")]),
			   io:format("~p~n",[string:span("\t    abcdef", "\t ")]),
			   io:format("~p~n",[string:span("\t    abcdef", "d ")]),
			   io:format("~p~n",[string:span("\t    abcdef", "\t")]).





%% This is how span(String1, String2) works:
%% String1 is being scanned from the left to right, each charachter is checked against all 
%% characters in String2. if it was NOT fornd , then counter counts it and it goes
%% to next character. This process stops when it hits the first character which is found in String2.

cspan_test() -> io:format("~p~n",[string:cspan("\t    abcdef", "a \t")]),
			   io:format("~p~n",[string:cspan("\t    abcdef", " \t")]),
			   io:format("~p~n",[string:cspan("\t    abcdef", "a")]),
			   io:format("~p~n",[string:cspan("\t    abcdef", "d")]),
			   io:format("~p~n",[string:cspan("\t    abcdef", "ce")]).




token_test1() -> string:tokens(" abcc def 3rc     crvbr334gr4       ","c").
token_test2() -> string:tokens(" abcc def 3rc     crvbr334gr4       ","c ").



join_test() -> string:join(["one", "two", "three"], ", ").


chars_test() -> string:chars($a, 5).
chars_test1() -> string:chars($a, 5, "bcd").


copies_test() -> string:copies("abcdfe",4).



words_test() -> string:words("String1.String2.String3....", $.).


sub_word_test() -> string:sub_word("String1.String2.String3....",2, $.).


strip_test() -> string:strip(".....String1.String2.String3....", both, $.).





%% Returns the String with the length adjusted in accordance with Number. 
%% The left margin is fixed. If the length(String) < Number, String is padded with character (last argument).

left_test() -> string:left("Hello",10,$.).


right_test() -> string:right("Hello", 10, $.).


%% Returns a string, where String is centred in the string and surrounded by blanks or characters. 
%% The resulting string will have the length Number.
centre_test() -> string:centre("Hello", 10, $.).




%% Returns a substring of String, starting at the position 4 (1 based index) to and including the 8 position.
sub_string_test() -> string:sub_string("Hello World", 4, 8).
%%										12345678901


%% Returns a substring of String, starting at the position 4 (1 based index), 
%% and ending at the length 5.
substr_test() -> string:substr("Hello World", 4, 5).
%%								12345678901
%%								   12345   





%% to_float(String) -> {Float,Rest} | {error,Reason}

%% Argument String is expected to start with a valid text represented float 
%% (the digits being ASCII values). 
%% Remaining characters in the string after the float are returned in Rest (which is String).


to_float_test1() ->{F1,Fs} = string:to_float("1.0-1.0e-1"),
				   {F2,[]} = string:to_float(Fs),
				   F1+F2.


to_float_test2() -> string:to_float("-1.5eX").







%% to_integer(String) -> {Int,Rest} | {error,Reason} 
%% 
%% Argument String is expected to start with a valid text represented integer (the digits being ASCII values). Remaining characters in the 
%% string after the integer are returned in Rest.


to_integer_test1() -> {I1,Is} = string:to_integer("33+22"),
					  {I2,[]} = string:to_integer(Is),
					  I1-I2.


to_integer_test2() -> string:to_integer("0.5").







%%
%% Local Functions
%%

