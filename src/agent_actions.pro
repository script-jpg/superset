% Agent Actions Module
% ---------------------
% This module contains all the action execution logic for the Ollama agent
% It provides a clean interface for executing different types of agent actions

% Include necessary files
:- include('types.pro').
:- include('helpers.pro').
:- include('generation.pro').
:- include('ollama_agent.pro').
:- include('chat_room.pro').

% Load library for string manipulation
:- use_module(library(strings)).
:- use_module(library(lists)).

% Main predicate to execute agent actions
% Input: ActionString in format "ACTION|DETAILS|REFERENCES"
% UserId: The user performing the action
execute_agent_action(ActionString, UserId) :-
    format('Agent: Executing action "~w" as user ~w~n', [ActionString, UserId]),
    handle_action(ActionString, UserId, Reply),
    format('Action result: ~w~n', [Reply]),
    !.  % Green cut: commit to this action handler

% Handle the action string by parsing and dispatching to appropriate command
handle_action(ActionString, UserId, Reply) :-
    split_string(ActionString, "|", "", [Action|Args]),
    command(Action, Args, UserId, Reply).

% ---------- Command Table ---------- %
% Handle post action: POST|Content|References
command("POST", [Content, TagPart, RefPart], UserId, reply(String)) :-
    % decode the reference part
    parseList(RefPart, References, _),
    parseList(TagPart, _, Tags),
    add_post(Content, UserId, Tags, References, _PostId),
    format(string(String), "Posted '~s' by ~s", [Content, UserId]).

% Handle react action: REACT|PostId|Reaction
command("REACT", [PostId, Reaction], UserId, reply(String)) :-
    add_reaction(UserId, PostId, Reaction),
    format(string(String), "~s reacted ~s to post ~s", [UserId, Reaction, PostId]).

% Handle send message action: SEND|Room|Message
command("ROOM_MESSAGE", [Room, Message], UserId, reply(String)) :-
    send_to_room(UserId, Room, Message),
    persist_all,
    format(string(String), "~s sent message to room ~s", [UserId, Room]).

% Handle create room action: CREATE_ROOM|RoomName|RoomDesc|MemberPart
command("CREATE_ROOM", [RoomName,RoomDesc, MemberPart], UserId, reply(String)) :-
    parseList(MemberPart, AllowedMembers, _),
    create_room(RoomName, RoomDesc, UserId, AllowedMembers),
    % Persist the room data
    persist_all,
    format(string(String), '~s created room ~s', [UserId, RoomName]).

% Handle unknown commands
command(Cmd, _, _UserId, error(String)) :-
    format(string(String), "Unknown command: ~s", [Cmd]).

% Helper predicates
parseList(CSV, Atoms, Strs) :-
    string_chars(CSV, Chs),
    (   maplist([C]>>char_type(C,space), Chs)   % every char is whitespace
    ->  Atoms = [],
        Strs   = []
    ;   split_string(CSV, ",", " ", Strs0),
        exclude(==(""), Strs0, Strs),
        maplist(atom_string, Atoms, Strs)
    ).