-module(interpreter_tests).

-import(interpreter, [parse/1]).

-import(interpreter, [exec/1, exec/2, exec/3]).

-import(interpreter, [eval/1]).
-import(interpreter, [eval/2]).
-import(interpreter, [eval/3]).

-export([]).

-include_lib("eunit/include/eunit.hrl").

exec_test_() ->
    %% TODO Hide warnings from the build
    %% TODO Read from file (Mock)
    
    %% NOTE Exception Reason -> ct:print(user, "~ts~n", [Reason])
    
    [ exception(Code) || Code <- ["№", "1()"] ],
    
    [ fun () -> Test = "a = 1;",

                Code = parse(Test),
                
                ?debugVal(Code),
                
                {value, Res, Env} = interpreter:exec(Code),
                
                ?debugVal(Res),
                ?debugVal(Env)
                
                %% TODO eval is placed at the end and validated against exec
      end,
      
      fun () -> Test = "function test (a) return a end",
      
                Code = interpreter:parse(Test),
                
                ?debugVal(Code),
                      
                interpreter:exec(Code)
      end,
      
      fun () -> Test = "for i = 10, 1, -1 do print (i) end",
                Code = interpreter:parse(Test),
      
                ?debugVal(Code),
                  
                interpreter:exec(Code)
      end,
      
      fun () -> Test = "for i = 15, 1, -1 do print (i) end",
                Code = interpreter:parse(Test),
      
                ?debugVal(Code),
                  
                interpreter:exec(Code)
      end
    ].
    
exception(Code) ->
    {_, {_Reason, _Stacktrace}} = catch(parse(Code)).