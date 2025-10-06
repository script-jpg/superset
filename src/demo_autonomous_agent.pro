% Autonomous Ollama agent demo
% ------------------------------
% This demo shows an Ollama agent that:
% 1. Browses the forum to see recent posts
% 2. Decides whether to post or react based on what it sees
% 3. Uses its tools to interact with the forum

% Load the main system which includes all necessary modules
:- consult('../src/main.pro').
:- consult('../src/user_ids.pro'). % Load user IDs module
:- consult('../src/chat_room.pro'). % Load chat room module

% Main demo predicate - entry point for the autonomous agent
run_autonomous_agent :-
    format('=== Autonomous Ollama Agent Demo ===~n~n'),

    % Select a random user ID for this action
    select_random_user_id(UserId),
    
    % 80/20 choice: 80% browse forum, 20% create new thread
    random(0, 10, RandomChoice),
    (   RandomChoice < 2
    ->  % Create new thread - skip browsing, use empty forum view
        FinalForumSummary = 'The thread is empty. Make an original post on a new topic.',
        format('Agent: Randomly chose to create new thread (no browsing needed)~n')
    ;   % Use current behavior - browse the forum
        format('Agent: Let me browse the forum...~n'),
        agent_browse_forum(ForumSummary),
        FinalForumSummary = ForumSummary
    ),
    
    format('Forum summary:~n~w~n~n', [FinalForumSummary]),
    
    % Step 2: Generate a response based on what it sees
    format('Agent: Thinking about what to do...~n'),
    generate_agent_action(FinalForumSummary, ActionString, UserId),
    
    % Step 3: Execute the chosen action
    execute_agent_action(ActionString, UserId),
    
    format('~n=== Demo complete ===~n').

% Select a random user ID from the available set
select_random_user_id(UserId) :-
    available_user_ids(UserIds),
    random_member(UserId, UserIds).

% Get room context for the agent - available rooms and recent messages
get_room_context_for_agent(Context, UserId) :-
    % Get all available rooms using the chat_room module
    findall(Room,
            (chat_room:room(Room, _, _),
             room_visible_to_user(Room, UserId)),
            Rooms),
    
    % For each room, get recent messages (last 3 messages)
    maplist(get_room_recent_messages, Rooms, RoomSummaries),
    
    % If no rooms exist, provide a default message
    (   RoomSummaries = []
    ->  Context = 'No Chat-Rooms currently exist. You could create one!'
    ;   atomic_list_concat(RoomSummaries, '\n\n', Context)
    ).

% Get recent messages from a room (last 3 messages)
get_room_recent_messages(Room, Summary) :-
    chat_room:get_room_log(Room, Messages),
    length(Messages, MessageCount),
    
    % Take last 3 messages or all if less than 3
    (   MessageCount > 3
    ->  length(RecentMessages, 3),
        append(_, RecentMessages, Messages)
    ;   RecentMessages = Messages
    ),
    
    % Format messages
    maplist(format_room_message, RecentMessages, FormattedMessages),
    atomic_list_concat(FormattedMessages, '\n', MessageText),
    
    % Create room summary
    format(string(Summary), 'Room "~w" (~w messages):\n~w', [Room, MessageCount, MessageText]).

% Format a single room message
format_room_message((From, Text), Formatted) :-
    format(string(Formatted), '  ~w: ~w', [From, Text]).

% Generate what the agent should do next based on forum state and room context
generate_agent_action(ForumSummary, ActionString, UserId) :-
    
    format('Agent: Acting as user ~w~n', [UserId]),
    
    % Get room context - available rooms and recent messages
    get_room_context_for_agent(RoomContext, UserId),

    format('Room context:~n~w~n~n', [RoomContext]),

    % Get all users for reference as a string
    available_user_ids(AllUsers),
    atomic_list_concat(AllUsers, ', ', AllUsersStr),
    
    % Create a prompt for the Ollama model with user ID, forum summary, and room context
    atomic_list_concat([
        'You are user ', UserId, ' on a social platform. Here is what you see:\n',
        'All Users: [', AllUsersStr, ']\n\n',
        'Forum Summary:\n',
        ForumSummary,
        '\nRoom Context:\n',
        RoomContext,
        'Choose ONE action and follow the format:\n'
        ,'POST|POST_CONTENT|POST_TAGS|POST_REFERENCES\n'
        ,'REACT|POST_ID|REACTION(e.g. like, love, laugh)\n'
        ,'CREATE_ROOM|ROOM_NAME|ROOM_DESCRIPTION|ALLOWED_MEMBERS(e.g. user1, user2)\n'
        ,'ROOM_MESSAGE|ROOM_NAME|MESSAGE_CONTENT\n'
        , 'Examples:\n'
        ,'POST|demo post|demo, first|\n'
        ,'REACT|p1234567890|like\n'
        ,'CREATE_ROOM|demo_room|A room for demos|user1, user2\n'
        ,'ROOM_MESSAGE|demo_room|Hello everyone!\n'
    ], Prompt),
    
    % Get response from Ollama with timeout handling
    catch(
        (   ollama_generate(Prompt, 'kimi-k2:1t-cloud', RawResponse),
            format('Raw Ollama response:~n~w~n', [RawResponse])
        ),
        _Error,
        (RawResponse = 'ACTION: nothing\nDETAILS: \nREFERENCES: \nREASON: timeout')
    ),
    
    normalize_space(atom(Clean), RawResponse),
    (   Clean = "" -> ActionString = 'nothing| | '   % fallback
    ;   ActionString = Clean
    ).