:- module(time, []).

:- use_module(library(pce)).
:- use_module(library(clpfd)).

now(Yr/Month/Day, Hr:Min) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(year, DateTime, Yr),
    date_time_value(month, DateTime, Month),
    date_time_value(day, DateTime, Day),
    date_time_value(hour, DateTime, Hr),
    date_time_value(minute, DateTime, Min).

:- dynamic entry/4.

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


overlapping_entry(entry(Yr/Month/Day, StartHr:StartMin, EndHr:EndMin, _),
                  entry(Yr/Month/Day, StartHr1:StartMin1, EndHr1:EndMin1, Info1)) :-
    entry(Yr/Month/Day, StartHr1:StartMin1, EndHr1:EndMin1, Info1),
    overlapping_times(StartHr:StartMin, EndHr:EndMin, StartHr1:StartMin1, EndHr1:EndMin1).

valid_entry(entry(_Yr/_Month/_Day, StartHr:StartMin, EndHr:EndMin, _Info)) :-
    valid_time(StartHr:StartMin),
    valid_time(EndHr:EndMin),    
    before(StartHr:StartMin, EndHr:EndMin).

make_entry(Entry, PreConditions, PostConditions) :-
    PreConditions,
    valid_entry(Entry),
    assertz(Entry :- PostConditions).

% ?- use_module(library(clpfd)).

% ?- time:make_entry(entry(2025/3/19, 10:0, 11:40, "See Doctor"), true, true).

% ?- time:entry(Date, T1, T2, Info).

%% We have to be very specific about the point at which we're instantiating the clpfd vars

% ?- member(Hr1, [9, 11]), T1 = Hr1:Min1, T2 = Hr2:Min2, time:valid_time(T1), time:valid_time(T2), time:time_delta(T1, T2, 0:30), E = entry(2025/3/19, T1, T2, "Buy Soybeans"), time:valid_entry(E), label([Min1, Min2]), \+ time:overlapping_entry(E, E2).

%% We can also conditionally add conditional events!

% ?- assertz(snap:entry(2025/3/D, 13:50, 17:20, "Board Games") :- rainy(2025/3/D)).

% ?- assertz(snap:rainy(2025/3/18)).

% ?- snap:entry(2025/3/17, S, E, "Board Games").

% ?- snap:entry(2025/3/18, S, E, "Board Games").

% ?- snap:overlapping_entry(entry(_/_/_, 11:35, 12:45, _), E2).

% ?- entry(_, _, S, "b"), entry(_, E, _, "c"), make_entry(_, S, E, "D").

%% Culminating in a beautiful general abstraction

% ?- snap:make_entry(entry(2025/3/D, T1, 17:20, "Board Games"), (valid_time(T1), T1 = H1:M1, label([H1, M1])), rainy(2025/3/D)).


%% Soft constraints example

% ?- valid_time(Hr1:Min1), P1 in 0..1, P2 in 0..2, E = entry(2025/3/19, Hr1:Min1, 23:59, "Blah"), Hr1 in 7..15 #\/ P1 #= 1, Min1 in 0..10 #\/ P2 #= P1 + 1, P2 #< 2, label([Hr1, Min1]).


%% Ideas for a GUI
%% - Calendar Page
%% - Daily Page
%% - Make an entry page
%% - Specifics

:- pce_begin_class(monthly_calendar, dialog).
initialise(Dialog, _Yr:int, Month:int) :->
send_super(Dialog, initialise),

LineLength is 6,
PicWidth is 1200,
PicHeight is 1200,

send(Dialog, append, new(Picture, picture)),
send(Picture, size, size(PicWidth, PicHeight)),
send(Picture, display,  new(DeviceHeader, device)),
send(DeviceHeader, display, new(BoxHeader, box(PicWidth/1.2, 50)), point((PicWidth - PicWidth/1.2)/2, 0)),
send(BoxHeader, fill_pattern, colour(white)),
send(BoxHeader, pen, 2),
month_name(Month, MonthName),
send(DeviceHeader, display, new(_, text(MonthName)), point(PicWidth/2, 10)),
send(Picture, display, new(DeviceCalendar, device), point(0, 100)),
days_in_month(Month, Days),
forall(between(1, Days, Day),
       (
           Y is Day // LineLength,
           X is Day mod LineLength,
           send(DeviceCalendar, display, new(Box, box(PicWidth/LineLength - 20, PicHeight/LineLength - 20)), point(X*(PicWidth/LineLength), Y*(PicHeight/LineLength))),
           send(Box, fill_pattern, colour(white)),
           send(Box, pen, 2),
           send(DeviceCalendar, display, new(_, text(Day)), point(X*(PicWidth/LineLength) + 5, Y*(PicHeight/LineLength) + 5))
       )).
:- pce_end_class.

% ?- new(MonthlyCal, time:monthly_calendar(1, 1)), send(MonthlyCal, open).

:- pce_begin_class(daily_calendar, dialog).
initialise(Dialog, Yr:int, Month:int, Day:int) :->
send_super(Dialog, initialise),
PicWidth is 1800,
PicHeight is 1200,

send(Dialog, append, new(Picture, picture)),
send(Picture, size, size(PicWidth, PicHeight)),
send(Picture, display,  new(DeviceHeader, device)),
send(DeviceHeader, display, new(BoxHeader, box(PicWidth/1.2, 50)), point((PicWidth - PicWidth/1.2)/2, 0)),
send(BoxHeader, fill_pattern, colour(white)),
send(BoxHeader, pen, 2),
send(DeviceHeader, display, new(_, text(Day)), point(PicWidth/2, 10)),
send(Picture, display, new(DeviceCalendar, device), point(0, 100)),
forall(between(1, 24, Hour),
       send(DeviceCalendar, display, new(_, text(Hour)), point(0, Hour*120))),
forall(entry(Yr/Month/Day, StartHr:StartMin, EndHr:EndMin, Info),
       (time_delta(StartHr:StartMin, EndHr:EndMin, HrDiff:MinDiff),
        send(DeviceCalendar, display, new(_, box(600, HrDiff*120 + MinDiff)), point(100, StartHr*120)),
        send(DeviceCalendar, display, new(_, text(Info)), point(105, StartHr*120 + 5)))).
%% send(Dialog, append, button(next, message(@prolog, daily_calendar, Yr, Month, Day))),
:- pce_end_class.

% ?- assertz(time:entry(1/1/1, 10:0, 12:0, "Buy soybeans")).

% ?- new(DailyCal, time:daily_calendar(1, 1, 1)), send(DailyCal, open).
