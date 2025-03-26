:- module(snap, [save_source/1, save_bin/1, launcher/1, get_method/4]).

:- use_module(library(pce)).
:- use_module(library(clpfd)).

:- use_module(repl, [planit_repl/0, planit_scratch/0]).
:- use_module(datime, [monthly_calendar/0, daily_calendar/0, make_entry/4, entry/3, now/2, time_delta/3, same_time/2, before/2, after/2]).

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
               use_module(datime, [monthly_calendar/0, daily_calendar/0, make_entry/4, entry/4, now/2, time_delta/3, same_time/2, before/2, after/2, assign_cols/2]),
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

% ?- save_source("~/prolog/timeplans/").

% ?- save_bin("~/prolog/timeplans/").

% ?- qsave_program('../timeplans/timeplan').

% ?- module_property(snap, exports(E)).

% ?- use_module(library(clpfd)).

% ?- assertz(datime:rainy(2025/3/D)).

% ?- datime:entry(Y/M/D, Hr:Min, I1), datime:entry(Y/M/D, Hr:Min, I2), I1 \== I2.

% ?- datime:make_entry(2025/3/D, Hr:Min, "Working", (datime:rainy(2025/3/D), label([Hr, Min, D]), \+ (member(I, ["See Doctor"]), datime:entry(2025/3/D, Hr:Min, I)))).

% ?- valid_time(H:M), entry(2025/3/19, H:M, I).

% ?- clause(datime:entry(A, B, C), Body).

% ?- datime:make_entry(2025/3/19, Hr:Min, "See Doctor", (Hr #> 1 #/\ Hr #=< 3)).

% ?- qsave_program('../timeplans/timeplan').



% ?- assertz(a(3) :- (B \= 3, \+ a(B))).

% ?- a(3).
