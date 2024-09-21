-module(interpreter).

-import(erlbox, [success/0, success/1]).

-export([parse/1, exec/1, eval/1]).

-include_lib("erlbox/include/erlbox.hrl").

-type code() :: string().

-type expression() :: erl_parse:abstract_expr().

%% NOTE The host app is responsible to catch expression
-spec parse(code()) -> expression().
parse(Code) ->
    %% TODO Indicate error (throw)
    Exp = interpreter_parse:process(_ = interpreter_scan:process(Code)),
    
    translate(Exp).

translate(Code) ->
    %% TODO
    Code.

%% TODO Introduce ENV (optional)

%% NOTE The host app is responsible to catch expression
-spec exec(expression()) -> success() | success(term()).
exec(Exp) ->
    io:format("~tp", [Exp]),
    %% TODO Indicate error 
    %% TODO Run the code
    success().

file(Filename) ->
    {ok, Bin} = erl_prim_loader:read_file(Filename),
    
    Res = binary_to_list(Bin),
    Res.

eval(Filename) ->
    Code = file(Filename),
    
    exec(_Exp = parse(Code)).