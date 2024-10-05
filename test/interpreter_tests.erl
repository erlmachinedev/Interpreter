-module(interpreter_tests).
-export([]).

-include_lib("eunit/include/eunit.hrl").

interpreter_test_() ->
    %% TODO Hide warnings from the build
    %% TODO Indicate parse error (Code)
    
    %% TODO Read from file (Mock)
    
    [ fun () -> Test = "a = 1",
                Code = interpreter:parse(Test),
                
                ?debugVal(Code),
                
                interpreter:exec(_Code = interpreter:parse(Test))
                %Res0 = interpreter:eval(File)
      end,
      
      fun () -> Test = "function test (a) return a end",
                Code = interpreter:parse(Test),
                
                ?debugVal(Code),
                      
                interpreter:exec(Code)
                %Res0 = interpreter:eval(File)
      end,
      
      fun () -> Test = "a = 0; function test () return a end",
                Code = interpreter:parse(Test),
      
                ?debugVal(Code),
                  
                interpreter:exec(Code)
                %Res0 = interpreter:eval(File)
      end
    ].
    