:- module(snap, [save/1, launcher/1, get_method/4]).

:- use_module(library(pce)).
:- use_module(library(clpfd)).

:- use_module(repl).
:- use_module(time).

save(Dir, Name, ExtraDirectives) :-
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
    module_property(Name, exports(Exports)),
    with_output_to(Stream,
                   (format(":- module(~s, ~w).\n", [Name, Exports]),
                    
                    forall(member(Directive, ExtraDirectives), format(":- ~w.\n", [Directive])),
                    
                    listing(Name:_))),
    close(Stream).

:- multifile to_save/2.

to_save(snap, [use_module(library(pce)),
               use_module(library(clpfd)),
               use_module(time, [monthly_calendar/1, daily_calendar/1]),
               use_module(repl, [repl/1])]).

save(Dir) :-
    forall(to_save(Name, ExtraDirectives),
           save(Dir, Name, ExtraDirectives)).
    %% save(Dir,
    %%      snap,
         
    %%      [use_module(library(pce)),
    %%       use_module(library(clpfd)),
    %%       use_module(time, [monthly_calendar/1, daily_calendar/1]),
    %%       use_module(repl, [repl/1])]),
    %% save(Dir,
    %%      repl,
    %%      [use_module(library(pce))]),
    %% save(Dir,
    %%      time,
    %%      [use_module(library(pce)),
    %%       use_module(library(clpfd))]).

:- multifile app/2.

launcher(Dialog) :-
    new(Dialog, dialog("Welcome to Planit")),
    forall(app(Name, App), send(Dialog, append, button(Name, message(@prolog, App)))),    
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
    
% ?- snap:launcher(D).

% ?- save("~/prolog/timeplans/").

% ?- module_property(snap, exports(E)).

% ?- assertz(time:rainy(2025/3/D)).

% ?- make_entry(entry(2025/3/D, 10:0, 17:20, "Board Games"), true, time:rainy(2025/3/D)).

% ?- time:make_entry(entry(2025/3/19, 10:0, 11:40, "See Doctor"), true, true).

% ?- time:make_entry(entry(2025/3/20, 9:0, 17:0, "Work on planner"), true, true).
