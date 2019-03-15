%Partner: Sergey Gasparyan (sgaspary@ucsc.edu)
%Partner: Samuel Guyette (sguyette@ucsc.edu)
% Prolog not
not(X) :- X,!,fail.
not(_).

calc_time(StartLoc, FinishLoc, ReturnTime) :-

    airport(StartLoc, _, degmin(Degree1, Degree2),
    degmin(Degree3, Degree4)),
    airport(FinishLoc, _, degmin(DegreeA, DegreeB),
    degmin(DegreeC, DegreeD)),

    StartLat is (Degree1 + (Degree2/60)) * pi/180,
    StartLon is (Degree3 + (Degree4/60)) * pi/180,
    FinishLat is (DegreeA + (DegreeB/60)) * pi/180,
    FinishLon is (DegreeC + (DegreeD/60)) * pi/180,

    LatDistance is FinishLat - StartLat,
    LonDistance is FinishLon - StartLon,

    %convert to Haversine Radians
    Temp is sin(LatDistance/2) ** 2 + cos(StartLat) *
        cos(FinishLat) * sin(LonDistance / 2) ** 2,%** turns to float


    TempDistance is 2 * atan2(sqrt(Temp), sqrt(1 - Temp)),
    RealDistance is TempDistance*3959,

    ReturnTime is (RealDistance/500).


to_upper( Lower, Upper) :-
   atom_chars( Lower, Lowerlist),
   maplist( lower_upper, Lowerlist, Upperlist),
   atom_chars( Upper, Upperlist).


print_trip( Action, Code, Name, time( Hour, Minute)) :-
    to_upper( Code, Upper_code),
    format( "%-6s  %3s  %-16s  %02d:%02d",
        [Action, Upper_code, Name, Hour, Minute]),nl.

printtpath([]) :-
    nl,!.


printtpath([flight(Start, NextDes, time(Hours,Min)) |
    ListWithoutFront]) :-
    airport(Start, LeavingName, _, _),
    airport(NextDes, ArrivingName, _, _),
    to_upper(Start, StartUpper),
    to_upper(NextDes, NextDesCap),

    print_trip(depart,StartUpper,LeavingName,time(Hours,Min)),

    calc_time(Start,NextDes,ReturnTime),

    Realtime is ReturnTime + Hours + (Min/60),
    RealHours is floor(Realtime),
    RealMin is round((Realtime - (floor(Realtime)))*60.0),

    print_trip(arrive, NextDesCap, ArrivingName,
        time(RealHours,RealMin)),

    printtpath(ListWithoutFront).




calc_arival_time(Hour, Min, TravelTime, RetrunArivalTime) :-
    RetrunArivalTime is (TravelTime + Hour + (Min/60) + 0.5).


calc_depart_time(Hour, Min, RetrunDepartureTime) :-
    RetrunDepartureTime is (Hour + (Min/60)).



findpath(Start, Finish, [flight(Start,NextStop,NextTime)| PathList]) :-
    flight(Start, NextStop, NextTime),
    findpath(NextStop, Finish, [flight(Start,NextStop,NextTime)],
        PathList).


findpath(Start, Start, _, []).  %error start and finish the same.


findpath(Start, Finish,
    [flight(PrevOne,PrevTwo, time(HourOne, MinOne)) | PrevPath],
    [flight(Start, NextStop, time(HourTwo, MinTwo))| NextPath]) :-

    not(Start = Finish),
    %flight(PrevOne, PrevTwo, time(HourOne, MinOne)),
    flight(Start,NextStop, time(HourTwo, MinTwo)),

    calc_time(PrevOne, PrevTwo, TravelTimeA),
    calc_time(Start, NextStop, TravelTimeB),

    %arival time adds 30 min for layover
    calc_arival_time(HourOne, MinOne, TravelTimeA, ArivalTime),
    calc_depart_time(HourTwo, MinTwo, DepartTime),

    ArivalTime =< DepartTime,

    %check if adding next flight exceeds 24 hour cap
    DepartTime + TravelTimeB < 24,

    %if all of the above conditions pass add flight patern to path list.
    NewPrevPath = append([flight(PrevOne, PrevTwo,
        time(HourOne, MinOne))], PrevPath),

    %add checking conditions for already existing routes *****
    not(member(NextStop, NewPrevPath)),
    not(NextStop = PrevTwo),
    not(PrevOne = NextStop),
    %call recursivly
    findpath(NextStop, Finish, [flight(Start,NextStop,
        time(HourTwo, MinTwo)) | NewPrevPath], NextPath).




%Main
%Fails if Start and Finish are the same Airports
fly(Start,Start) :-
    nl,
    !,fail.

%Runs search algo
fly(Start, Finish) :-
    findpath(Start, Finish, PathList), nl,
    printtpath(PathList).

%Fails if airport is not in Database
fly(Start, Finish) :-
    airport(Start, _, _, _),
    airport(Finish, _, _, _),
    write('Error: flight path is not possible'),
    !, fail.

fly(_,_) :-
    write('Error: Not a real Airport.'),
    nl,
    !, fail.
