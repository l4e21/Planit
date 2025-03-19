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
    
% ?- snap:save("~/prolog/").

% ?- listing(time:_).

