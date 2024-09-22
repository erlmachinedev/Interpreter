-module(interpreter).

-import(erlbox, [success/0, success/1, success/2]).

-export([parse/1]).

-export([exec/1, exec/2, eval/1, eval/2]).

-include_lib("erlbox/include/erlbox.hrl").

-type code() :: string().

-type exp() :: erl_parse:abstract_expr().
-type env() :: map().

%% TODO Introduce Lua datatype

%% NOTE The host app is responsible to catch expression
-spec parse(code()) -> exp().
parse(Code) ->
    %% TODO Indicate error (throw)
    Exp = interpreter_parse:process(_ = interpreter_scan:process(Code)),
    
    translate(Exp).

translate(Code) ->
    %% TODO
    Code.

exec(Exp) ->
    exec(Exp, _Env = #{}).
    
-spec exec(exp(), env()) -> success(term(), env()) | failure(env()).
exec(Exp, Env) ->
    io:format("~tp ~tp", [Exp, Env]),
    %% TODO Indicate error 
    %% TODO Run the code
    success(nil, Env).

file(Filename) ->
    {ok, Bin} = erl_prim_loader:read_file(Filename),
    
    Res = binary_to_list(Bin),
    Res.

eval(Filename) ->
    eval(Filename, _Env = #{}).
    
eval(Filename, Env) ->
    Code = file(Filename),
    
    exec(_Exp = parse(Code), Env).
    
%%% Expression API

%% NOTE The expression should produce runtime error (assing wrong arg, etc).