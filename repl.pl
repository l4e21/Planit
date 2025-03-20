:- module(repl, []).

:- use_module(library(pce)).

%% Problem: Append-only, no prepend. Try device instead (like list_browser but uses graphicals instead of dict_items)
repl_run(CmdAtom, Log) :-
    %% Third arg is variable bindings
    atom_to_term(CmdAtom, Cmd, Bindings),
    %% term_string(Cmd, CmdStr),
    catch((Cmd,
           term_string(Bindings, BindingsStr),
           (BindingsStr == "[]"
           -> send(Log, append, "TRUE")
           ; send(Log, append, BindingsStr))),
          _,
          send(Log, append, "FAIL")).

repl(D) :-
    new(D, dialog("REPL")),
    send(D, append, new(Align, dialog_group(aligner, group))),
    send(Align, append, new(N1, text_item('REPL'))),
    send(Align, append, new(Log, list_browser)),
    send(Log, font, font(courier, roman, 14)),
    send(D, append, button(enter, message(@prolog, repl_run, N1?selection, Log))),
    send(D, default_button, enter).

:- multifile snap:app/2.
:- discontiguous snap:app/2.

snap:app("REPL", repl:repl/1).
