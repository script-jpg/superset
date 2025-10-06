% Simple in-memory chat room system
% Supports DMs and Group Chats
% ---

% :- module(chat_room, [
%         create_room/1,          % +RoomName
%         send_to_room/3,         % +From, +Room, +Text
%         get_room_log/2,         % +Room, -Messages
%         room_log_text/3         % +Room, +UserId, -Text
% ]).

:- dynamic room/3.            % room(RoomName, [(From,Text),...])

create_room(Name, Description, CreatorId, Members) :-
    must_be(string, Name),
    \+ room(Name, _, _), % fail if room already exists
    % Add CreatorId to Members if not already present
    (   member(CreatorId, Members)
    ->  true
    ;   Members = [CreatorId|Members]
    ),
    asserta(room(Name, [], Members)),
    % Check if Description is a string and not empty
    (   string(Description), Description \= ""
    ->  % Send message to room log
        send_to_room(CreatorId, Name, Description)
    ;   true
    ).


send_to_room(From, Room, Text) :-
    ( atom(From) -> atom_string(From, FromStr) ; FromStr = From ),
    must_be(string, FromStr),
    must_be(string, Room),
    must_be(string, Text),
    retract(room(Room, Old, Members)),
    append(Old, [(FromStr,Text)], New),
    asserta(room(Room, New, Members)).

room_visible_to_user(Room, UserId) :-
    room(Room, _, Members),
    (   Members = []             % public room
    ;   member(UserId, Members)  % private room with user
    ).

get_room_log(Room, Ms) :-
    ( room(Room, Ms, _) -> true ; Ms = [] ).

room_log_text(Room, UserId, Text) :-
    get_room_log(Room, Ms),
    maplist(message_line(UserId), Ms, Lines),
    atomic_list_concat(Lines, "\n", Text).

message_line(_UserId, (From, Msg), Line) :-
    format(string(Line), "~w: ~w", [From, Msg]).