:- module(snap, [save_source/1, save_bin/1, launcher/1, get_method/4]).

:- use_module(library(pce)).
:- use_module(library(clpfd)).

:- use_module(repl, [planit_repl/0, planit_scratch/0]).
:- use_module(datime, [monthly_calendar/0, daily_calendar/0, make_entry/3, entry/4, now/2, time_delta/3, same_time/2, before/2, after/2]).

save_source(Dir, Name, ExtraDirectives) :-
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
               use_module(datime, [monthly_calendar/0, daily_calendar/0, make_entry/3, entry/4, now/2, time_delta/3, same_time/2, before/2, after/2]),
               use_module(repl, [planit_repl/0, planit_scratch/0])]).


save_source(Dir) :-
    forall(to_save(Name, ExtraDirectives),
           save_source(Dir, Name, ExtraDirectives)).

save_bin(Dir) :-
    format(string(File), "~s/planit", [Dir]),
    expand_file_name(File, [FileExpanded]),
    qsave_program(FileExpanded).


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
    
% ?- launcher(D).
%@ D = @11795411662916/dialog.

% ?- save_source("~/prolog/timeplans/").

% ?- save_bin("~/prolog/timeplans/").

% ?- qsave_program('../timeplans/timeplan').

% ?- module_property(snap, exports(E)).

% ?- assertz(datime:rainy(2025/3/D)).

% ?- datime:make_entry(entry(2025/3/D, 10:0, 17:20, "Board Games"), true, time:rainy(2025/3/D)).

% ?- datime:make_entry(entry(2025/3/19, 10:0, 11:40, "See Doctor"), true, true).

% ?- datime:make_entry(entry(2025/3/20, 9:0, 17:0, "Work on planner"), true, true).

% ?- qsave_program('../timeplans/timeplan').
