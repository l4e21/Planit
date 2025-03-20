:- module(snap, []).

:- use_module(library(pce)).
:- use_module(library(clpfd)).

:- use_module(repl).
:- use_module(time).

save(Dir, Name, Imports) :-
    format(string(File), "~s/~s.pl", [Dir, Name]),
    expand_file_name(File, [FileExpanded]),
    (exists_file(FileExpanded)
    -> write("Already exists. Overwrite? Y/N"),
       nl,
       (read('Y')
       -> true
       ; fail)
    ; true),
    open(FileExpanded, write, Stream),
    with_output_to(Stream,
                   (format(":- module(~s, []).\n", [Name]),
                    forall(member(Import, Imports),
                           format(":- use_module(~w).\n",
                                  [Import])),
                    listing(Name:_))),
    close(Stream).

save(Dir) :-
    save(Dir,
         snap,
         [library(pce),
          library(clpfd),
          time,
          repl]),
    save(Dir,
         repl,
         [library(pce)]),
    save(Dir,
         time,
         [library(pce),
          library(clpfd)]).

:- multifile app/2.
:- discontiguous app/2.

launcher :-
    new(Dialog, dialog("Welcome to Planit")),

    forall(app(Name, Module:Fn/_Arity),
           (compound_name_arguments(App1, Fn, [AppDialog]), App = Module:App1, App, send(Dialog, append, button(Name, message(AppDialog, open))))),
    
    send(Dialog, open).


get_method(Obj, Name, Type, Summary) :-
    get(Obj, class, Class),
    (get(Class, get_methods, MethodChain), Type="get"; get(Class, send_methods, MethodChain), Type="send"),
    chain_list(MethodChain, Methods),
    member(Method, Methods),
    get(Method, name, Name),
    get(Method, summary, SummaryObj),
    get(SummaryObj, value, Summary).

% ?- new(P, window), get_method(P, Name, Type, Summary).
    
% ?- snap:launcher.

% ?- snap:save("~/prolog/").

% ?- assertz(time:rainy(2025/3/D)).
%@ true.

% ?- time:make_entry(entry(2025/3/D, 13:50, 17:20, "Board Games"), true, time:rainy(2025/3/D)).
%@ true.

% ?- time:make_entry(entry(2025/3/19, 10:0, 11:40, "See Doctor"), true, true).
%@ true.
