% Generation utilities
% -------------------------------
:- include('types.pro').
:- include('chat_room.pro').

% Generate raw ID string with optional prefix
generate_raw_id(Prefix, Id) :-
    get_time(TimeStamp),
    format_time(string(TimeStr), '%Y%m%d%H%M%S', TimeStamp),
    random_between(1000, 9999, RandomSuffix),
    format(string(Id), '~w~w_~w', [Prefix, TimeStr, RandomSuffix]).

% Generate unique post ID as atom directly
generate_post_id(PostId) :-
    generate_raw_id('p', RawId),
    atom_string(PostId, RawId).

% Generate unique reaction ID as atom directly
generate_reaction_id(ReactionId) :-
    generate_raw_id('r', RawId),
    atom_string(ReactionId, RawId).

% Add a new post and assert it, also persist to file
add_post(Content, Author, Tags, References, PostId) :-
    generate_post_id(PostId),
    get_time(TimeStamp),
    format_time(string(CreatedAt), '%Y-%m-%dT%H:%M:%SZ', TimeStamp),

    % Directly assert without filtering (filter_content moved to ollama_agent)
    assertz(post(PostId, Content, Author, CreatedAt, Tags, References)),
    persist_all.

% Add a new reaction and assert it, also persist ALL facts to file
add_reaction(UserId, PostId, ReactionTag) :-
    generate_reaction_id(ReactionId),
    get_time(TimeStamp),
    format_time(string(CreatedAt), '%Y-%m-%dT%H:%M:%SZ', TimeStamp),
    
    assertz(reaction(ReactionId, UserId, PostId, ReactionTag, CreatedAt)),
    persist_all.

% Persist ALL facts to a single file for durability (absolute path)
persist_all :-
    % Get the directory of the current source file and construct the data path
    source_file(persist_all, ThisFile),
    file_directory_name(ThisFile, SrcDir),
    atomic_list_concat([SrcDir, '/../data/persisted_data.pro'], DataPath),
    absolute_file_name(DataPath, AbsolutePath),
    open(AbsolutePath, write, Stream),
    forall(post(P, C, A, Ca, T, R),
           write_term(Stream, post(P, C, A, Ca, T, R), [fullstop(true), nl(true), quoted(true)])),
    forall(reaction(Ri, U, P, Rt, Ca),
           write_term(Stream, reaction(Ri, U, P, Rt, Ca), [fullstop(true), nl(true), quoted(true)])),
    % Also persist room data
    forall(room(RoomName, Messages, Members),
           write_term(Stream, room(RoomName, Messages, Members), [fullstop(true), nl(true), quoted(true)])),
    close(Stream).
