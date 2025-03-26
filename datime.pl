:- module(datime, [monthly_calendar/0, daily_calendar/0, make_entry/4, entry/3, now/2, time_delta/3, same_time/2, before/2, after/2]).

:- use_module(library(pce)).
:- use_module(library(clpfd)).

:- multifile snap:to_save/2.

snap:to_save(datime,
             [use_module(library(pce)),
              use_module(library(clpfd))]).

now(Yr/Month/Day, Hr:Min) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(year, DateTime, Yr),
    date_time_value(month, DateTime, Month),
    date_time_value(day, DateTime, Day),
    date_time_value(hour, DateTime, Hr),
    date_time_value(minute, DateTime, Min).

:- dynamic entry/3.

month_name(1, "January").
month_name(2, "February").
month_name(3, "March").
month_name(4, "April").
month_name(5, "May").
month_name(6, "June").
month_name(7, "July").
month_name(8, "August").
month_name(9, "September").
month_name(10, "October").
month_name(11, "November").
month_name(12, "December").

days_in_month(1, 31).
days_in_month(2, 28).
days_in_month(3, 31).
days_in_month(4, 30).
days_in_month(5, 31).
days_in_month(6, 30).
days_in_month(7, 31).
days_in_month(8, 31).
days_in_month(9, 30).
days_in_month(10, 31).
days_in_month(11, 30).
days_in_month(12, 31).

valid_time(Hr:Min) :-
    Hr in 0..23,
    Min in 0..59.    

valid_date(Yr/Month/Day) :-
    Yr in 0..4000,
    Month in 1..12,
    days_in_month(Month, Days),
    Day in 1..Days.

before(Hr1:Min1, Hr2: Min2) :-
    Hr1 * 60 + Min1 #< Hr2 * 60 + Min2.

after(Hr1:Min1, Hr2: Min2) :-
    Hr1 * 60 + Min1 #> Hr2 * 60 + Min2.

same_time(Hr:Min, Hr: Min).

time_delta(Hr1:Min1, Hr2:Min2, DeltaHr:DeltaMin) :-
    valid_time(Hr1:Min1),
    valid_time(Hr2:Min2),
    valid_time(DeltaHr:DeltaMin),
    
    Minutes #= Min1 + DeltaMin,
    Minutes #>= 60 #<==> Carryover,
    Hr2 #= Hr1 + DeltaHr + Carryover,
    Min2 #= Minutes - 60*Carryover.
    

overlapping_times(StartHr1:StartMin1, EndHr1:EndMin1, StartHr2:StartMin2, EndHr2:EndMin2) :-
    (after(EndHr1:EndMin1, StartHr2:StartMin2), before(StartHr1:StartMin1, EndHr2:EndMin2)
    -> true
    ; before(StartHr1:StartMin1, EndHr2:EndMin2), after(EndHr1:EndMin1, StartHr2:StartMin2)).

entry_time(entry(_Y/_M/_D, Hr:Min, _Info), Hr:Min).
entry_date(entry(Y/M/D, _Hr:_Min, _Info), Y/M/D).
entry_info(entry(_Y/_M/_D, _Hr:_Min, Info), Info).

make_entry(Y/M/D, Hr:Min, Info, Conditions) :-
    assertz(entry(Y/M/D, Hr:Min, Info) :- (valid_time(Hr:Min), valid_date(Y/M/D), Conditions)).


% ?- use_module(library(clpfd)).

init_buttons(Yr, Month, Frame) :-
    get(Frame, member, "Dialog", Dialog),
    %% get(Frame, member, "Picture", Picture),

    send(Dialog, append,
         new(_,
             button('Last Month',
                    message(@prolog, last_monthly_cal, Yr, Month, Frame))),
         below),

    %% send(LastButton, name, "LastButton"),
    send(Dialog, append,
         new(_,
             button('Next Month',
                    message(@prolog, next_monthly_cal, Yr, Month, Frame))),
         below).

    %% update_buttons(Yr, Month, Frame)

    %% send(NextButton, label, "NextButton"),
    %% send(Picture, recogniser, new(@e, key_binding)),
    %% send(@e, function, 'e', message(@prolog, next_monthly_cal, Yr, Month, Frame)).
    %% send(@e, free),
    %% send(Picture, recogniser, new(@e, key_binding)),
    %% send(@e, function, 'e', message(NextButton, execute)).

    %% send(Picture, recogniser, new(LastK, key_binding)),
%% send(LastK, function, 'b', message(LastButton, execute)).

update_buttons(Yr, Month, Frame) :-
    get(Frame, member, "Dialog", Dialog),
    
    send(Dialog, clear),
    init_buttons(Yr, Month, Frame),
    send(Dialog, layout).

next_monthly_cal(Yr, Month, Frame) :-
    Month #= 12 #<==> Carryover,
    NextMonth #= Month mod 12 + 1,
    NextYr #= Yr + Carryover,
    
    update_calendar(NextYr, NextMonth, Frame),
    update_buttons(NextYr, NextMonth, Frame).
   
 
last_monthly_cal(Yr, Month, Frame) :-
    Month #= 1 #<==> Carryover,
    LastMonth #= Month - 1 + Carryover*12,
    LastYr #= Yr - Carryover,

    update_calendar(LastYr, LastMonth, Frame),
    update_buttons(LastYr, LastMonth, Frame).

update_calendar(Yr, Month, Frame) :-
    get(Frame, member, "Picture", Picture),
     
    get(Picture, member, "Calendar", C),
    get(Picture, member, "Header", H),
    send(C, free),
    send(H, free),

    init_calendar(Yr, Month, Picture),
    send(Frame, keyboard_focus, Picture).

init_calendar(Yr, Month, Picture) :-    
    Month in 1..12,
    days_in_month(Month, Days),
    LineLength is 6,
    
    PicWidth is 1200,
    PicHeight is 1200,

    send(Picture, display,  new(DeviceHeader, device)),
    send(Picture, display, new(DeviceCalendar, device), point(0, 100)),
    
    send(DeviceHeader, name, "Header"),
    send(DeviceCalendar, name, "Calendar"),

        send(DeviceHeader, display, new(BoxHeader, box(PicWidth/1.2 + 50, 50)), point((PicWidth - PicWidth/1.2)/2 + 50, 50)),
    send(BoxHeader, fill_pattern, colour(white)),
    send(BoxHeader, pen, 2),
    month_name(Month, MonthName),
    format(string(HeaderStr), "~s ~w", [MonthName, Yr]),
    send(DeviceHeader, display, new(_, text(HeaderStr)), point(PicWidth/2 + 50, 10 + 50)),

    forall(between(1, Days, Day),
           (
               Y is Day // LineLength,
               X is Day mod LineLength,
               send(DeviceCalendar, display, new(Box, box(PicWidth/LineLength - 20, PicHeight/LineLength - 20)), point(X*(PicWidth/LineLength) + 50, Y*(PicHeight/LineLength) + 50)),
               send(Box, fill_pattern, colour(white)),
               send(Box, pen, 2),
               send(DeviceCalendar, display, new(_, text(Day)), point(X*(PicWidth/LineLength) + 5 + 50, Y*(PicHeight/LineLength) + 5 + 50)),
               send(Box, recogniser, click_gesture(left, '', single, message(@prolog, daily_calendar, Yr, Month, Day))))).


monthly_calendar :-
    new(Frame, frame("Monthly Calendar")),
    send(Frame, append, new(Picture, picture)),
    send(new(Dialog, dialog), below(Picture)),

    send(Dialog, name, "Dialog"),
    send(Picture, name, "Picture"),
    send(Frame, keyboard_focus, Picture),
    
    now(Yr/Month/_Day, _Hr:_Min),

    init_calendar(Yr, Month, Picture),
    init_buttons(Yr, Month, Frame),
    send(Frame, open).

% ?- datime:monthly_calendar(F), send(F, open).

% ?- manpce.

% ?- open_daily.

% ?- manpce(key_binding).

% ?- manpce(event).

% ?- new(F, frame), send(F, append, new(P, picture)), send(P, name, "E"), get(F, member, "E", P).

% ?- new(P, picture), send(P, recogniser, new(K, key_binding)), get(P, recogniser, A).
    

daily_calendar(Yr, Month, Day, Picture) :-
    Month in 1..12,
    days_in_month(Month, Days),
    Day in 1..Days,

    PicWidth is 1800,
    PicHeight is 1200,

    VerticalScaleFactor is 2,
    ColumnWidth is 400,
    CalendarOffset is 100,

    send(Picture, size, size(PicWidth, PicHeight)),
    send(Picture, display,  new(DeviceHeader, device)),
    send(DeviceHeader, display, new(BoxHeader, box(PicWidth/1.2, 50)), point((PicWidth - PicWidth/1.2)/2, 0)),
    send(BoxHeader, fill_pattern, colour(white)),
    send(BoxHeader, pen, 2),
    month_name(Month, MonthName),
    format(string(DateStr), "~w ~s", [Day, MonthName]),
    send(DeviceHeader, display, new(_, text(DateStr)), point(PicWidth/2, 10)),
    send(Picture, display, new(DeviceCalendar, device), point(0, 100)),
    forall(between(0, 23, Hour),
           (
               send(DeviceCalendar, display, new(_, line(0, Hour*60*VerticalScaleFactor + CalendarOffset, 1800, Hour * 60 * VerticalScaleFactor + CalendarOffset))),
               send(DeviceCalendar, display, new(_, text(Hour)), point(0, Hour*60*VerticalScaleFactor + 5 + CalendarOffset))
           )),
    findall(Info, entry(Yr/Month/Day, _, Info), AllInfos),
    list_to_set(AllInfos, InfoSet),
    forall(nth0(Col, InfoSet, Info),
          send(DeviceCalendar, display, new(_, text(Info)), point(100 + Col*405 + ColumnWidth/2.5, 0))),
    
    findall(entry(Yr/Month/Day, Hr:Min, Info), (valid_time(Hr:Min), label([Hr, Min]), entry(Yr/Month/Day, Hr:Min, Info)), AllEntries),
    forall(member(entry(Yr/Month/Day, Hr:Min, Info), AllEntries),
           (nth0(Col, InfoSet, Info),
            send(DeviceCalendar, display, new(Box, box(ColumnWidth, VerticalScaleFactor)), point(100 + Col*(ColumnWidth + 5), Hr*60*VerticalScaleFactor + Min*VerticalScaleFactor + CalendarOffset)),
            send(Box, pen, 0),
            send(Box, fill_pattern, colour(cyan)))).

% ?- manpce(colour).
daily_calendar :-
    new(Frame, frame("Daily Calendar")),
    send(Frame, append, new(Picture, picture)),
    now(Yr/Month/Day, _Hr:_Min),
    daily_calendar(Yr, Month, Day, Picture),
    send(Frame, open).

daily_calendar(Yr, Month, Day) :-
    new(Frame, frame("Daily Calendar")),
    send(Frame, append, new(Picture, picture)),
    daily_calendar(Yr, Month, Day, Picture),
    send(Frame, open).
    

:- multifile snap:app/2.
:- discontiguous snap:app/2.

snap:app("Monthly Calendar", monthly_calendar).
snap:app("Daily Calendar", daily_calendar).
