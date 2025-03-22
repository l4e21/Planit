:- module(repl, [planit_repl/0, planit_scratch/0]).

:- use_module(library(pce)).

:- multifile snap:to_save/2.

snap:to_save(repl,
             [use_module(library(pce))]).

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

planit_repl :-
    new(D, dialog("REPL")),
    send(D, append, new(Align, dialog_group(aligner, group))),
    send(Align, append, new(N1, text_item('REPL'))),
    send(Align, append, new(Log, list_browser)),
    send(Log, font, font(courier, roman, 14)),
    send(D, append, button(enter, message(@prolog, repl_run, N1?selection, Log))),
    send(D, default_button, enter),
    send(D, open).


add_text(Dialog, PreText) :-
    new(Group, dialog_group('Text')),
    new(Editor, editor),
    send(Editor, name, 'Editor'),
    
    send(Editor, size, size(80, 5)),
    send(Editor, contents, PreText),
    
    send(Group, append, Editor),
    send(Dialog, append, Group, below),
    send(Dialog, layout),
    send(Dialog, fit).

add_text(Dialog) :-
    new(Group, dialog_group('Text')),
    new(Editor, editor),
    send(Editor, name, 'Editor'),
    send(Editor, size, size(80, 5)),
    send(Group, append, Editor),
    send(Dialog, append, Group, below),
    send(Dialog, layout),
    send(Dialog, fit).

run_editor_code(Editor, Output) :-
    get(Editor, contents, string(CmdStr)),
    atom_string(CmdAtom, CmdStr),
    atom_to_term(CmdAtom, Cmd, Bindings),
    catch((Cmd,
           term_string(Bindings, Bindingstr),
           (Bindingstr == "[]"
           -> Result = "TRUE\n"
           ; string_concat(Bindingstr, "\n", Result))),
          _,
          (Result = "FAIL\n")),
    send(Output, caret, 0),
    send(Output, insert, Result).

add_code(Dialog, PreText) :-
    new(Group, dialog_group('Code')),
    new(Editor, editor),
    send(Editor, name, 'Editor'),
    new(Output, editor),
    send(Output, name, 'Output'),
    
    send(Editor, size, size(80, 5)),
    send(Output, size, size(80, 5)),
    send(Group, append, Editor),
    send(Group, append, Output, right),
    send(Output, editable, @off),
    send(Editor, contents, PreText),
    send(Dialog, append, Group, below),
    send(Group, append, new(_, button('Run', message(@prolog, run_editor_code, Editor, Output)))),
    send(Dialog, layout),
    send(Dialog, fit).

add_code(Dialog) :-
    new(Group, dialog_group('Code')),
    new(Editor, editor),
    send(Editor, name, 'Editor'),
    new(Output, editor),
    send(Output, name, 'Output'),
    send(Editor, size, size(80, 5)),
    send(Output, size, size(80, 5)),
    send(Group, append, Editor),
    send(Group, append, Output, right),
    send(Output, editable, @off),
    send(Dialog, append, Group, below),
    send(Group, append, new(_, button('Run', message(@prolog, run_editor_code, Editor, Output)))),
    send(Dialog, layout),
    send(Dialog, fit).

% ?- manpce(object).

save_page(Dialog, Pagename) :-
    get(Dialog, members, Chain),
    chain_list(Chain, L),
    findall(T-Entry,
            (
                member(Obj, L),
                (get(Obj, name, 'Code')
                -> T = code,
                   get(Obj, member, 'Editor', Editor),
                   get(Editor, contents, string(Entry))
                ;  get(Obj, name, 'Text'),
                   T = text,
                   get(Obj, member, 'Editor', Editor),
                   get(Editor, contents, string(Entry)))
            ),
            Entries),
    retractall(page(Pagename, _)),
    assertz(page(Pagename, Entries)),
    send(@display, inform, "Done").

load_page(Dialog, Pagename) :-
    get(Dialog, members, Chain),
    chain_list(Chain, L),
    forall((member(Obj, L),
            (get(Obj, name, 'Text') -> true; get(Obj, name, 'Code'))),
           send(Dialog, delete, Obj)
          ),
    page(Pagename, Entries),
    forall(member(Type-Text, Entries),
           (Type == text
           -> add_text(Dialog, Text)
           ; Type == code, add_code(Dialog, Text))),
    
    send(Dialog, fit),
    send(Dialog, layout).

planit_scratch :-
    new(Frame, frame("Page Editor")),
    send(Frame, append, new(Dialog, dialog)),
    send(Dialog, name, "Dialog"),
    
    new(ButtonGroup, dialog_group('Buttons')),
    send(ButtonGroup, append, new(TI, text_item('page name')), below),
    send(ButtonGroup, append,
         new(_,
             button('+ Text',
                    message(@prolog, add_text, Dialog)))),
    send(ButtonGroup, append,
         new(_,
             button('+ Code',
                    message(@prolog, add_code, Dialog)))),
    send(ButtonGroup, append,
         new(_,
             button('Save',
                    message(@prolog, save_page, Dialog, TI?selection)))),
    send(ButtonGroup, append,
         new(_,
             button('Load',
                    message(@prolog, load_page, Dialog, TI?selection)))),
    send(Dialog, append, ButtonGroup, below),
    
    send(Dialog, fit),
    send(Frame, open).

:- multifile snap:app/2.
:- discontiguous snap:app/2.

snap:app("REPL", planit_repl).
snap:app("Page Editor", planit_scratch).
