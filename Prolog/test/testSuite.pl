:- module(testSuite,[init/0]).
:- use_module(library(plunit)).

init :- 
    consult('test/Handlers/userHandler.plt'),
    consult('test/Handlers/utils.plt'),
    run_tests.

