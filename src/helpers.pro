% Helper predicates
% -------------------------------
% Get all posts
get_all_posts(Posts) :-
    findall(post(Id, Content, Author, CreatedAt, Tags, Refs),
            post(Id, Content, Author, CreatedAt, Tags, Refs), Posts).

% Get posts by user
get_posts_by_user(UserId, Posts) :-
    findall(post(Id, Content, UserId, CreatedAt, Tags, Refs),
            post(Id, Content, UserId, CreatedAt, Tags, Refs), Posts).

% Get posts with specific tags
get_posts_by_tags(TagList, Posts) :-
    findall(post(Id, Content, Author, CreatedAt, Tags, Refs),
            (post(Id, Content, Author, CreatedAt, Tags, Refs),
             subset(TagList, Tags)), Posts).

% Query helper: posts referencing a target post
posts_referencing(TargetPostId, post(PostId, Content, Author, CreatedAt, Tags, References)) :-
    post(PostId, Content, Author, CreatedAt, Tags, References),
    member(TargetPostId, References).

% Get thread context (post and its replies)
get_thread_context(PostId, Context) :-
    post(PostId, Content, Author, CreatedAt, Tags, Refs),
    findall(ReplyPost, posts_referencing(PostId, ReplyPost), Replies),
    Context = thread(PostId, Content, Author, CreatedAt, Tags, Refs, Replies).

% Get reactions for a post
get_reactions_for_post(PostId, Reactions) :-
    findall(reaction(Id, User, PostId, Tag, Time),
            reaction(Id, User, PostId, Tag, Time), Reactions).