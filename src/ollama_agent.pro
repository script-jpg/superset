% Ollama agent utilities
% ----------------------
% Core Ollama integration and agent utilities
% This module provides the basic Ollama model interaction and browsing capabilities

% Generate text from Ollama model
% ollama_generate(+Prompt, +Model, -Response)
% Uses the Ollama CLI to get a response.
% Make ollama_generate dynamic so it can be mocked in tests
:- dynamic ollama_generate/3.

% Generate text from Ollama model with safe execution
% ollama_generate(+Prompt, +Model, -Response)
% Uses the Ollama CLI; if the command fails, returns an empty string.
ollama_generate(Prompt, Model, Response) :-
    catch(
        (   process_create(path(ollama), ["run", Model, Prompt], [stdout(pipe(Out)), process(PID)]),
            read_string(Out, _, Response),
            close(Out),
            process_wait(PID, _)
        ),
        _Error,
        (Response = '')
    ).

% Simple web browsing using curl
% agent_browse(+URL, -Content)
agent_browse(URL, Content) :-
    process_create(path(curl), ["-s", URL], [stdout(pipe(Out)), process(PID)]),
    read_string(Out, _, Content),
    close(Out),
    process_wait(PID, _).

% Agent browses the forum and returns a collection of random posts
agent_browse_forum(Summary) :-
    get_all_posts(Posts),
    % Sample between 1 and 5 posts with weighted distribution
    random(0.0, 1.0, Rand),
    (   Rand < 0.5 -> PostCount = 1
    ;   Rand < 0.75 -> PostCount = 2
    ;   Rand < 0.875 -> PostCount = 3
    ;   Rand < 0.9375 -> PostCount = 4
    ;   PostCount = 5
    ),
    % Take a random subset of PostCount posts
    length(Posts, N),
    (   N > PostCount
    ->  random_permutation(Posts, ShuffledPosts),
        length(RandomPosts, PostCount),
        append(RandomPosts, _, ShuffledPosts)
    ;   RandomPosts = Posts
    ),
    % Build a simple summary string
    maplist(post_summary, RandomPosts, Summaries),
    atomic_list_concat(Summaries, "\n", Summary).

post_summary(post(Id, Content, Author, _CreatedAt, _Tags, _Refs), Summary) :-
    % Now using atoms directly, no need to extract from compound term
    DisplayId = Id,
    % Show full content without truncation
    format(string(Summary), "Post ID: ~w | Author: ~w | Content: ~w", [DisplayId, Author, Content]).

% Helper predicate to find the last occurrence of a substring
reverse_string_find(String, SubString, Position) :-
    string_length(String, StringLen),
    string_length(SubString, SubLen),
    StartPos is StringLen - SubLen,
    (   StartPos >= 0
    ->  (   sub_string(String, StartPos, SubLen, _, SubString)
        ->  Position = StartPos
        ;   NextStartPos is StartPos - 1,
            reverse_string_find_helper(String, SubString, NextStartPos, Position)
        )
    ;   fail
    ).

reverse_string_find_helper(String, SubString, StartPos, Position) :-
    (   StartPos >= 0
    ->  (   sub_string(String, StartPos, _, _, SubString)
        ->  Position = StartPos
        ;   NextStartPos is StartPos - 1,
            reverse_string_find_helper(String, SubString, NextStartPos, Position)
        )
    ;   fail
    ).

% Agent searches posts by tag and returns matching posts
agent_search_by_tag(Tag, Posts) :-
    % Now using atoms directly for tags
    get_posts_by_tags([Tag], Posts).

% Agent gets thread context for a post
agent_get_thread(PostId, Thread) :-
    get_thread_context(PostId, Thread).

% Helper predicate to remove thinking sections from generated text.
% It looks for a line containing "...done thinking." and keeps everything after it.
filter_content(Content, Clean) :-
    split_string(Content, "\n", "", Lines),
    (   append(_, [Marker|Rest], Lines),
        sub_string(Marker, _, _, _, "...done thinking.")
    ->  atomic_list_concat(Rest, "\n", Clean)
    ;   Clean = Content
    ).
