-module(interpreter_tests).
-export([]).

-include_lib("eunit/include/eunit.hrl").

interpreter_test() ->
    Test = "a = 1",
    
    ?debugVal(interpreter:parse(Test)),
    
    %% TODO Hide warnings from the build
    %% TODO Indicate parse error (Code)
    
    %% TODO Read from file (Mock)
    
    interpreter:exec(interpreter:parse(Test)),
    
    [ fun () -> interpreter:exec(_Code = interpreter:parse(Test))
                %Res0 = interpreter:eval(File)
      end
    ].
    