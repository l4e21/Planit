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


add_text(Frame, PreText) :-
    get(Frame, member, 'Window', Window),
    get(Window, member, 'Dialog', Dialog),
    new(Group, dialog_group('Text')),
    new(Editor, editor),
    send(Editor, name, 'Editor'),
    
    send(Editor, size, size(80, 5)),
    send(Editor, contents, PreText),
    
    send(Group, append, Editor),
    new(ButtonGroup, dialog_group('Buttons')),
    send(Group, append, ButtonGroup, right),
    send(ButtonGroup, append, new(_, button('Remove', message(@prolog, remove_group, Dialog, Group))), below),
    send(Dialog, append, Group, below),

    get(Dialog, size, DialogSize),
    get(DialogSize, width, W),
    get(DialogSize, height, H),
    send(Dialog, size, size(W, H+120)),
    send(Dialog, layout),
    send(Dialog, compute),
    send(Dialog, fit).

add_text(Frame) :-
    get(Frame, member, 'Window', Window),
    get(Window, member, 'Dialog', Dialog),
    new(Group, dialog_group('Text')),
    new(Editor, editor),
    send(Editor, name, 'Editor'),
    send(Editor, size, size(80, 5)),
    send(Group, append, Editor),
    new(ButtonGroup, dialog_group('Buttons')),
    send(Group, append, ButtonGroup, right),
    send(ButtonGroup, append, new(_, button('Remove', message(@prolog, remove_group, Dialog, Group))), below),
    
    send(Dialog, append, Group, below),
    get(Dialog, size, DialogSize),
    get(DialogSize, width, W),
    get(DialogSize, height, H),
    send(Dialog, size, size(W, H+120)),

    send(Dialog, layout),
    send(Dialog, compute),
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

add_code(Frame, PreText) :-
    get(Frame, member, 'Window', Window),
    get(Window, member, 'Dialog', Dialog),
    new(Group, dialog_group('Code')),
    new(ButtonGroup, dialog_group('Buttons')),
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
    send(Group, append, ButtonGroup, right),
    send(ButtonGroup, append, new(_, button('Run', message(@prolog, run_editor_code, Editor, Output)))),
    send(ButtonGroup, append, new(_, button('Remove', message(@prolog, remove_group, Dialog, Group))), below),
    send(Dialog, append, Group, below),
    
    get(Dialog, size, DialogSize),
    get(DialogSize, width, W),
    get(DialogSize, height, H),
    send(Dialog, size, size(W, H+120)),
    send(Dialog, layout),
    send(Dialog, compute),
    send(Dialog, fit).

% ?- manpce(dialog_group).

remove_group(Dialog, Group) :-
    send(Dialog, delete, Group),
    get(Dialog, size, Size),
    get(Size, width, W),
    get(Size, height, H),
    send(Dialog, size, size(W, H-120)),
    send(Dialog, compute),
    send(Dialog, fit),
    send(Dialog, layout).

add_code(Frame) :-
    get(Frame, member, 'Window', Window),
    get(Window, member, 'Dialog', Dialog),
    new(Group, dialog_group('Code')),
    new(ButtonGroup, dialog_group('Buttons')),
    new(Editor, editor),
    send(Editor, name, 'Editor'),
    new(Output, editor),
    send(Output, name, 'Output'),
    send(Editor, size, size(80, 5)),
    send(Output, size, size(80, 5)),
    send(Group, append, Editor),
    send(Group, append, Output, right),
    send(Output, editable, @off),
    send(Group, append, ButtonGroup, right),
    send(ButtonGroup, append, new(_, button('Run', message(@prolog, run_editor_code, Editor, Output)))),
    send(ButtonGroup, append, new(_, button('Remove', message(@prolog, remove_group, Dialog, Group))), below),
    send(Dialog, append, Group, below),
    get(Dialog, size, DialogSize),
    get(DialogSize, width, W),
    get(DialogSize, height, H),
    send(Dialog, size, size(W, H+120)),
    send(Dialog, layout),
    send(Dialog, compute), 
    send(Dialog, fit).

% ?- manpce(dialog).

save_page(Frame, Pagename) :-
    get(Frame, member, 'Window', Window),
    get(Window, member, 'Dialog', Dialog),
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

load_page(Frame, Pagename) :-
    get(Frame, member, 'Window', Window),
    get(Window, member, 'Dialog', Dialog),
    get(Dialog, members, Chain),
    chain_list(Chain, L),
    forall((member(Obj, L),
            (get(Obj, name, 'Text') -> true; get(Obj, name, 'Code'))),
           send(Dialog, delete, Obj)
          ),
    send(Dialog, size, size(1800, 600)),
    page(Pagename, Entries),
    forall(member(Type-Text, Entries),
           (Type == text
           -> add_text(Frame, Text)
           ; Type == code, add_code(Frame, Text))),

    send(Dialog, layout),
    send(Dialog, compute),
    send(Dialog, fit).

planit_scratch :-
    new(Frame, frame("Page Editor")),
    send(Frame, size, size(1800, 1200)),
    
    new(W, window),
    send(W, size, size(1800, 1200)),
    send(W, name, 'Window'),
    send(W, scrollbars, both),
    send(Frame, append, W),
    new(Dialog, dialog),
    send(Dialog, size, size(1800, 600)),
    send(Dialog, name, 'Dialog'),
    
    new(ButtonGroup, dialog_group('Buttons')),
    send(ButtonGroup, append, new(TI, text_item('page name')), below),
    send(ButtonGroup, append,
         new(_,
             button('+ Text',
                    message(@prolog, add_text, Frame)))),
    send(ButtonGroup, append,
         new(_,
             button('+ Code',
                    message(@prolog, add_code, Frame)))),
    send(ButtonGroup, append,
         new(_,
             button('Save',
                    message(@prolog, save_page, Frame, TI?selection)))),
    send(ButtonGroup, append,
         new(_,
             button('Load',
                    message(@prolog, load_page, Frame, TI?selection)))),
    send(Dialog, append, ButtonGroup, below),
    
    send(Dialog, layout),
    send(Dialog, compute),
    send(Dialog, fit),
    
    send(W, display, Dialog, point(0, 0)),
    send(Frame, open).

:- multifile snap:app/2.
:- discontiguous snap:app/2.

snap:app("REPL", planit_repl).
snap:app("Page Editor", planit_scratch).
