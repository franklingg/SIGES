:- encoding(utf8).

:- use_module("./src/TUI/screens.pl").
:- use_module("./src/TUI/listeners.pl").
:- use_module("./src/utils.pl").
:- use_module("./test/testSuite.pl").

start:- systemStart(S),
        main(S).

main(Type):-
    (current_prolog_flag(unix, _), utils:cls;true), % cls only works on unix systems
    screen(Type,Content,NextScreens),
    writeln(Content),
    screenListener(Type,Next),
    (nonvar(Next),!;
    promptChoice(NextScreens, Next)),
    main(Next).

test:-
    testSuite:init,
    halt.
    