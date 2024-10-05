-module(interpreter).

-import(erl_syntax, [module_qualifier/2, application/2]).

-import(erl_syntax, [atom/1]).
-import(erl_syntax, [list/2]).
-import(erl_syntax, [string/1]).

-import(erl_syntax, [revert/1]).

-import(erlbox, [success/0, success/1, success/2]).

-export([parse/1]).

-export([exec/1, exec/2, exec/3]).

-export([eval/1]).
-export([eval/2]).
-export([eval/3]).

-include_lib("erlbox/include/erlbox.hrl").

-type code() :: string().

-type exp() :: erl_parse:abstract_expr().
-type env() :: map().

-type return(Res, Env) :: {value, Res, Env}.

-type filename() :: file:filename().

%% TODO Introduce Lua datatype

%% NOTE The host app is responsible to catch exception
-spec parse(code()) -> exp().
parse(Code) ->
    %% TODO Test indicated error (throw)
    Exp = interpreter_parse:process(_ = interpreter_scan:process(Code)),
    
    translate(Exp).

translate(Code) ->
    io:format("~tp", [Code]),
    
    Mod = atom(io),
    Fun = atom(format),
    
    Node = module_qualifier(Mod, Fun),

    Test = string("~tp:~tp(test)"),
    List = list([Mod, Fun], none),

    Res = application(Node, [_Output = atom(user), Test, List]),
    
    revert(Res).

%% TODO Dedicated nodes to construct the type

exec(Exp) ->
    exec(Exp, _Env = #{}).

%% TODO Consider return type name {value, Value, NewBindings} | Value

exec(Exp, Env) ->
    exec(Exp, Env, _Fun = none).

-spec exec(exp(), env(), function() | none) -> return(term(), env()).
exec(Exp, Env, Fun) ->
    %io:format(user, "~tp ~tp ~tp~n", [Exp, Env, Fun]),
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

eval(Filename, Env) ->
    eval(Filename, Env, _Fun = none).

-spec eval(filename(), env(), function() | none) -> return(term(), env()).
eval(Filename, Env, Fun) ->
    Code = file(Filename),
    
    exec(_Exp = parse(Code), Env, Fun).
    
%%% Expression API

%% NOTE The expression should produce runtime error (passing wrong args, etc).

%% NOTE The Fun expression (function declaration) is stored in ENV