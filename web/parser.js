// Prolog data parser
class PrologParser {
    constructor() {
        this.posts = new Map();
        this.users = new Map();
        this.reactions = new Map();
    }

    // Parse Prolog fact strings
    parseFact(factString) {
        // Remove trailing period and whitespace
        factString = factString.trim().replace(/\.$/, '');
        
        // Extract predicate name and arguments
        const match = factString.match(/^(\w+)\((.*)\)$/);
        if (!match) return null;
        
        const [, predicate, argsString] = match;
        const args = this.parseArguments(argsString);
        
        return { predicate, args };
    }

    // Parse comma-separated arguments, handling nested structures
    parseArguments(argsString) {
        const args = [];
        let current = '';
        let depth = 0;
        let inQuotes = false;
        
        for (let i = 0; i < argsString.length; i++) {
            const char = argsString[i];
            
            if (char === '"' && argsString[i-1] !== '\\') {
                inQuotes = !inQuotes;
            }
            
            if (!inQuotes) {
                if (char === '(' || char === '[') depth++;
                if (char === ')' || char === ']') depth--;
            }
            
            if (char === ',' && depth === 0 && !inQuotes) {
                args.push(current.trim());
                current = '';
            } else {
                current += char;
            }
        }
        
        if (current.trim()) {
            args.push(current.trim());
        }
        
        return args.map(arg => this.parseArgument(arg.trim()));
    }

    // Parse individual argument
    parseArgument(arg) {
        // Handle strings
        if (arg.startsWith('"') && arg.endsWith('"')) {
            return arg.slice(1, -1);
        }
        
        // Handle atoms
        if (arg.startsWith("'") && arg.endsWith("'")) {
            return arg.slice(1, -1);
        }
        
        // Handle compound terms
        if (arg.includes('(')) {
            const compoundMatch = arg.match(/^(\w+)\((.*)\)$/);
            if (compoundMatch) {
                const [, functor, innerArgs] = compoundMatch;
                return {
                    type: 'compound',
                    functor,
                    args: this.parseArguments(innerArgs)
                };
            }
        }
        
        // Handle lists
        if (arg.startsWith('[') && arg.endsWith(']')) {
            const listContent = arg.slice(1, -1);
            if (listContent.trim() === '') return [];
            return this.parseArguments(listContent);
        }
        
        // Handle atoms/variables
        return arg;
    }

    // Load Prolog data from text
    loadData(prologText) {
        const lines = prologText.split('\n').filter(line => line.trim());
        
        for (const line of lines) {
            const fact = this.parseFact(line);
            if (!fact) continue;
            
            switch (fact.predicate) {
                case 'post':
                    this.parsePost(fact.args);
                    break;
                case 'user':
                    this.parseUser(fact.args);
                    break;
                case 'reaction':
                    this.parseReaction(fact.args);
                    break;
            }
        }
    }

    parsePost(args) {
        const postId = this.extractId(args[0]);
        const content = args[1];
        const authorId = this.extractId(args[2]);
        const createdAt = args[3];
        let tags = args[4] || [];
        const references = (args[5] || []).map(ref => this.extractId(ref));
        
        // Handle tags more carefully - they might be objects or already parsed
        let processedTags = [];
        if (Array.isArray(tags)) {
            processedTags = tags.map(tag => {
                if (typeof tag === 'object' && tag.type === 'compound' && tag.functor === 'post_tag') {
                    return tag.args[0];
                } else if (typeof tag === 'string') {
                    return tag;
                } else {
                    return String(tag);
                }
            });
        } else {
            processedTags = [];
        }
        
        const post = {
            id: postId,
            content: content,
            author: authorId,
            createdAt: createdAt,
            tags: processedTags,
            references: references,
            replies: []
        };
        
        this.posts.set(postId, post);
    }

    parseUser(args) {
        const userId = this.extractId(args[0]);
        const createdAt = args[1];
        const userType = args[2];
        
        this.users.set(userId, {
            id: userId,
            createdAt: createdAt,
            type: userType
        });
    }

    parseReaction(args) {
        const reactionId = args[0];
        const userId = this.extractId(args[1]);
        const postId = this.extractId(args[2]);
        const reactionTag = args[3];
        const createdAt = args[4];
        
        const reaction = {
            id: reactionId,
            user: userId,
            post: postId,
            tag: reactionTag,
            createdAt: createdAt
        };
        
        if (!this.reactions.has(postId)) {
            this.reactions.set(postId, []);
        }
        this.reactions.get(postId).push(reaction);
    }

    extractId(idTerm) {
        // If it's already a string, return it as is
        if (typeof idTerm === 'string') {
            return idTerm;
        }
        // Handle compound terms
        if (typeof idTerm === 'object' && idTerm.type === 'compound' && idTerm.functor === 'post_id') {
            return idTerm.args[0];
        }
        if (typeof idTerm === 'object' && idTerm.type === 'compound' && idTerm.functor === 'user_id') {
            return idTerm.args[0];
        }
        if (typeof idTerm === 'object' && idTerm.type === 'compound' && idTerm.functor === 'post_tag') {
            return idTerm.args[0];
        }
        // Fallback: return as string
        return String(idTerm);
    }

    // Build reply relationships
    buildReplyTree() {
        for (const post of this.posts.values()) {
            for (const refId of post.references) {
                const referencedPost = this.posts.get(refId);
                if (referencedPost) {
                    referencedPost.replies.push(post.id);
                }
            }
        }
    }

    // Get thread context (post and its replies)
    getThreadContext(postId) {
        const post = this.posts.get(postId);
        if (!post) return null;
        
        const replies = post.replies.map(replyId => this.posts.get(replyId)).filter(Boolean);
        
        return {
            post: post,
            replies: replies
        };
    }

    // Get all posts
    getAllPosts() {
        return Array.from(this.posts.values());
    }

    // Get posts by user
    getPostsByUser(userId) {
        return this.getAllPosts().filter(post => post.author === userId);
    }

    // Get reactions for post
    getReactionsForPost(postId) {
        return this.reactions.get(postId) || [];
    }
}