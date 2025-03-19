:- module(snap, []).
:- use_module(library(pce)).
:- use_module(library(clpfd)).
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
          time]),
    save(Dir,
         time,
         [library(pce),
          library(clpfd)]).

launcher :-
    new(Dialog, dialog("Welcome to Planit")),
    time:now(Yr/Month/Day, _Hr:_Min),
    time:monthly_calendar(Yr, Month, MonthlyCal),
    time:daily_calendar(Yr, Month, Day, DailyCal),
    
    send(Dialog, append, button('monthly calendar', message(MonthlyCal, open)), below),
    send(Dialog, append, button('daily calendar', message(DailyCal, open)), below),
    send(Dialog, open).
    
% ?- snap:launcher.

% ?- snap:save("~/prolog/").
