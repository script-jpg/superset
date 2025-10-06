% Declarations for dynamic and discontiguous predicates
:- dynamic post/6.
:- dynamic reaction/5.
:- dynamic user/3.
:- dynamic llm_fields/3.

:- discontiguous user/3.
:- discontiguous llm_fields/3.
:- discontiguous post/6.
:- discontiguous reaction/5.

:- discontiguous user_id/1.
:- discontiguous post_id/1.
:- discontiguous reaction_id/1.
:- discontiguous post_tag/1.
:- discontiguous reaction_tag/1.
:- discontiguous user_type/1.
:- discontiguous datetime/1.

% Types and validation predicates
% -------------------------------
% Basic type validation predicates - now using atoms instead of compound terms
user_id(Id) :- atom(Id).
post_id(Id) :- atom(Id).
reaction_id(Id) :- atom(Id).
post_tag(Tag) :- atom(Tag).

% User types
user_type(human).
user_type(llm).

% LLM fields - direct and clean
llm_fields(UserId, Model, Prompt) :-
    user_id(UserId),
    string(Model),
    string(Prompt).

% User predicate - direct structure
user(UserId, CreatedAt, UserType) :-
    user_id(UserId),
    datetime(CreatedAt),
    user_type(UserType).

% Post record - now using atoms directly
post(PostId, Content, Author, CreatedAt, Tags, References) :-
    post_id(PostId),
    string(Content),
    user_id(Author),
    (CreatedAt = nothing ; datetime(CreatedAt)),
    is_list(Tags), maplist(post_tag, Tags),
    is_list(References), maplist(post_id, References).

% Reaction record - now using atoms directly
reaction(ReactionId, UserId, PostId, ReactionTag, CreatedAt) :-
    reaction_id(ReactionId),
    user_id(UserId),
    post_id(PostId),
    reaction_tag(ReactionTag),
    datetime(CreatedAt).