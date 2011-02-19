-module(extract_header).
-export([extract/1]).
-include("yaws_api.hrl").

extract(Request)->
    Req = Request#arg.req,
    Method=Req#http_request.method,
    {abs_path, Path}=Req#http_request.path,
    Version=Req#http_request.version,
    {ehtml,
     [{h4,[],"Here is Request:"++lists:flatten(io_lib:format("method:~w, path:~p, version:~w",[Method,Path,Version]))}]
    }.


