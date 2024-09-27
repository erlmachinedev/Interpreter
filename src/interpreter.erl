-module(interpreter).

-import(erlbox, [success/0, success/1, success/2]).

-export([parse/1]).

-export([exec/1, exec/2, eval/1, eval/2]).

-include_lib("erlbox/include/erlbox.hrl").

-type code() :: string().

-type exp() :: erl_parse:abstract_expr().
-type env() :: map().

%% TODO Introduce Lua datatype

%% NOTE The host app is responsible to catch exception
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

%% TODO Consider return type name {value, Value, NewBindings} | Value

-spec exec(exp(), env(), function() | none) -> return(term(), env()).
exec(Exp, Env) ->
    io:format("~tp ~tp", [Exp, Env]),
    %% TODO Indicate error 
    %% TODO Run the code
    
    %% TODO Add ENV to Bindings Var
    
    erl_eval:expr(Exp, Env, Fun).

file(Filename) ->
    {ok, Bin} = erl_prim_loader:read_file(Filename),
    
    Res = binary_to_list(Bin),
    Res.

%% TODO eval returns Value

eval(Filename) ->
    eval(Filename, _Env = #{}).


-spec eval(filename(), env()) -> term().
eval(Filename, Env) ->
    Code = file(Filename),
    
    exec(_Exp = parse(Code), Env).
    
%%% Expression API

%% NOTE The expression should produce runtime error (passing wrong args, etc).

%% NOTE The Fun expression (function declaration)