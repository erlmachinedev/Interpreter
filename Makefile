PROJECT = interpreter
PROJECT_DESCRIPTION = "Embedded program to execute Lua code"
PROJECT_VERSION = 0.0.1

DEPS = erlbox

dep_erlbox = git git@github.com:erlmachinedev/erlbox.git

TEST_DEPS = meck

dep_meck = git git@github.com:eproxus/meck.git

include erlang.mk
