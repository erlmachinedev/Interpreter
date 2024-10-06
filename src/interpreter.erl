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

-type env() :: map().

-type return(Res, Env) :: {value, Res, Env}.

-type filename() :: file:filename().

%% TODO Introduce Lua datatype

-spec parse(code()) -> term().
parse(Code) ->
    Res = interpreter_parse:process(_ = interpreter_scan:process(Code)),
    
    translate(Res).

translate(Code) ->
    io:format(user, "Code: ~tp~n", [Code]),
    
    Mod = atom(io),
    Fun = atom(format),
    
    Node = module_qualifier(Mod, Fun),

    Test = string("~tp:~tp(test)~n"),
    List = list([Mod, Fun], none),

    Res = application(Node, [_Output = atom(user), Test, List]),
    
    revert(Res).


%% NOTE The translation is divided onto terminals and nonterminals parts
%% NOTE The terminal can call nonterminal and vice versa 

%% NOTE The each node is represented via application (even terminals)

statement([], Acc) ->
    Acc;

statement([{_Tag = 'assign', _, Vars, Exps}|T], Acc) ->
    Namelist = namelist(Vars),
    
    

statement([{_Tag ='NAME', _, Name}|T], Acc)

%% Expression API

namelist(List) ->
    [ Name || {_Tag ='NAME', _, Name} <- List ].

%chunk()
%block()

%exp()

%var(Name, Env) ->
%    maps:get(Name, Env, _Default = nil).

%statement() 

%% TODO Dedicated nodes to construct the type

exec(Exp) ->
    exec(Exp, _Env = #{}).

%% TODO Consider return type name {value, Value, NewBindings} | Value

exec(Exp, Env) ->
    exec(Exp, Env, _Fun = none).

-spec exec(term(), env(), function() | none) -> return(term(), env()).
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