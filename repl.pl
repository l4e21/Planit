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


group('text').
group('code').


add('code', Frame) :-
    get(Frame, member, 'Window', Window),
    get(Window, member, 'Dialog', Dialog),
    new(Group, dialog_group('code')),
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
    send(Editor, recogniser, new(K1, key_binding)),
    send(K1, function, '\\C-c', message(Editor?selection, copy)),
    send(K1, function, '\\C-v', message(Editor, paste)),
    send(Output, recogniser, new(K2, key_binding)),
    send(K2, function, '\\C-c', message(Output?selection, copy)),
    send(K2, function, '\\C-v', message(Output, paste)),
    
    send(Dialog, layout),
    send(Dialog, compute), 
    send(Dialog, fit).

add('text', Frame) :-
    get(Frame, member, 'Window', Window),
    get(Window, member, 'Dialog', Dialog),
    new(Group, dialog_group('text')),
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
    send(Editor, recogniser, new(K, key_binding)),
    send(K, function, '\\C-c', message(Editor?selection, copy)),
    send(K, function, '\\C-v', message(Editor, paste)),

    send(Dialog, layout),
    send(Dialog, compute),
    send(Dialog, fit).
    
add('code', Frame, PreText) :-
    get(Frame, member, 'Window', Window),
    get(Window, member, 'Dialog', Dialog),
    new(Group, dialog_group('code')),
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
    send(Editor, recogniser, new(K1, key_binding)),
    send(K1, function, '\\C-c', message(Editor?selection, copy)),
    send(K1, function, '\\C-v', message(Editor, paste)),
    send(Output, recogniser, new(K2, key_binding)),
    send(K2, function, '\\C-c', message(Output?selection, copy)),
    send(K2, function, '\\C-v', message(Output, paste)),
    
    get(Dialog, size, DialogSize),
    get(DialogSize, width, W),
    get(DialogSize, height, H),
    send(Dialog, size, size(W, H+120)),
    send(Dialog, layout),
    send(Dialog, compute),
    send(Dialog, fit).

add('text', Frame, PreText) :-
    get(Frame, member, 'Window', Window),
    get(Window, member, 'Dialog', Dialog),
    new(Group, dialog_group('text')),
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
    send(Editor, recogniser, new(K, key_binding)),
    send(K, function, '\\C-c', message(Editor?selection, copy)),
    send(K, function, '\\C-v', message(Editor, paste)),
    
    send(Dialog, layout),
    send(Dialog, compute),
    send(Dialog, fit).

content('text', Group, Content) :-
    get(Group, member, 'Editor', Editor),
    get(Editor, contents, string(Content)).
content('code', Group, Content) :-
    get(Group, member, 'Editor', Editor),
    get(Editor, contents, string(Content)).

% ?- manpce(chain).

%% Any kind of destructive update will require something along these lines because XPCE dialogs suck lol
remove_group(Dialog, Group) :-
    get(Dialog, size, Size),
    get(Size, width, W),
    get(Size, height, H),
    send(Dialog, size, size(W, H-120)),
    get(Dialog, members, Chain),
    send(Chain, delete, Group),
    send(Group, for_all, message(Group, delete, @arg1)),
    chain_list(Chain, L),
    send(Dialog, clear),
    send(Chain, clear),
    %% send(Chain, for_all, message(@prolog, format, '~w\n', @arg1)),
    forall(member(X, L),
            send(Dialog, append, X)),
    send(Dialog, compute),
    send(Dialog, fit),
    send(Dialog, layout).

save_page(Frame, Pagename) :-
    get(Frame, member, 'Window', Window),
    get(Window, member, 'Dialog', Dialog),
    get(Dialog, members, Chain),
    chain_list(Chain, L),
    findall(GroupName-Content,
            (
                member(Obj, L),
                get(Obj, name, GroupName),
                content(GroupName, Obj, Content)
            ),
            Entries),
    retractall(page(Pagename, _)),
    assertz(page(Pagename, Entries)),
    send(@display, inform, "Done").

% ?- manpce(chain).

load_page(Frame, Pagename) :-
    get(Frame, member, 'Window', Window),
    get(Window, member, 'Dialog', Dialog),
    
    get(Dialog, members, Chain),
    chain_list(Chain, L),

    forall(group(GroupName),           
           forall((member(Obj, L), get(Obj, name, GroupName)),
                  remove_group(Dialog, Obj))),
    
    get(@display, size, Size),
    send(Dialog, size, Size),
    
    page(Pagename, Entries),
    forall(member(GroupName-Content, Entries),
           add(GroupName, Frame, Content)),
    send(Dialog, layout),
    send(Dialog, compute),
    send(Dialog, fit).

planit_scratch :-
    new(Frame, frame("Page Editor")),
    get(@display, size, Size),
    send(Frame, size, Size),
    
    new(W, window),
    send(W, size, Size),
    send(W, name, 'Window'),
    send(W, scrollbars, both),
    send(Frame, append, W),
    new(Dialog, dialog),
    send(Dialog, size, Size),
    send(Dialog, name, 'Dialog'),
    
    new(ButtonGroup, dialog_group('Buttons')),
    send(ButtonGroup, append, new(TI, text_item('page name')), below),
    forall(group(GroupName),
           (format(string(AddGroupName), "+ ~w", [GroupName]),
            send(ButtonGroup, append,
                 new(_,
                     button(AddGroupName,
                            message(@prolog, add, GroupName, Frame)))))),
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
