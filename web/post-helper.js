// Shared helper functions for creating post elements
// This module provides common functionality for rendering posts

class PostHelper {
    constructor(parser) {
        this.parser = parser;
    }

    /**
     * Creates a post element with the given post data
     * @param {Object} post - The post object containing id, author, content, etc.
     * @param {boolean} isReply - Whether this post is a reply
     * @param {number} depth - The depth level for indentation
     * @param {string} elementIdPrefix - Prefix for the element ID (default: 'post')
     * @returns {HTMLElement} The created post element
     */
    createPostElement(post, isReply = false, depth = 0, elementIdPrefix = 'post') {
        const postDiv = document.createElement('div');
        postDiv.className = 'post';
        postDiv.id = `${elementIdPrefix}-${post.id}`;
        if (isReply) {
            postDiv.classList.add('reply');
        }
        
        // Apply indentation based on depth
        if (depth > 0) {
            postDiv.style.marginLeft = `${depth * 40}px`;
        }
        
        const postInfo = document.createElement('div');
        postInfo.className = 'post-info';
        
        const postNum = document.createElement('span');
        postNum.className = 'post-num';
        postNum.textContent = `${post.id}`;
        
        const postAuthor = document.createElement('span');
        postAuthor.className = 'post-author';
        postAuthor.textContent = post.author;
        
        const postTime = document.createElement('span');
        postTime.className = 'post-time';
        postTime.textContent = this.formatDate(post.createdAt);
        
        postInfo.appendChild(postAuthor);
        postInfo.appendChild(document.createTextNode(' '));
        postInfo.appendChild(postTime);
        postInfo.appendChild(document.createTextNode(' '));
        postInfo.appendChild(postNum);
        
        const postContent = document.createElement('div');
        postContent.className = 'post-content';
        postContent.innerHTML = this.formatContent(post.content);
        
        const postTags = document.createElement('div');
        postTags.className = 'post-tags';
        post.tags.forEach(tag => {
            const tagSpan = document.createElement('span');
            tagSpan.className = 'tag';
            tagSpan.textContent = tag;
            postTags.appendChild(tagSpan);
        });
        
        postDiv.appendChild(postInfo);
        postDiv.appendChild(postContent);
        if (post.tags.length > 0) {
            postDiv.appendChild(postTags);
        }
        
        // Add reactions section below post tags
        const reactions = this.parser.getReactionsForPost(post.id);
        if (reactions.length > 0) {
            const reactionsDiv = document.createElement('div');
            reactionsDiv.className = 'post-reactions';
            
            // Group reactions by type
            const reactionCounts = {};
            reactions.forEach(reaction => {
                reactionCounts[reaction.tag] = (reactionCounts[reaction.tag] || 0) + 1;
            });
            
            // Display reaction counts
            Object.entries(reactionCounts).forEach(([tag, count]) => {
                const reactionSpan = document.createElement('span');
                reactionSpan.className = 'reaction';
                reactionSpan.textContent = `${tag} (${count})`;
                reactionsDiv.appendChild(reactionSpan);
            });
            
            postDiv.appendChild(reactionsDiv);
        }
        
        return postDiv;
    }

    /**
     * Formats post content, handling greentext and newlines
     * @param {string} content - The raw post content
     * @returns {string} The formatted HTML content
     */
    formatContent(content) {
        // Replace literal \n with actual newlines first
        content = content.replace(/\\n/g, '\n');
        
        // Handle greentext (>be me)
        return content
            .split('\n')
            .map(line => {
                if (line.startsWith('>')) {
                    return `<span class="greentext">${line}</span>`;
                }
                return line;
            })
            .join('<br>');
    }

    /**
     * Formats a date string into a human-readable format
     * @param {string} dateString - The date string to format
     * @returns {string} The formatted date
     */
    formatDate(dateString) {
        if (dateString === 'nothing') return '';
        try {
            const date = new Date(dateString);
            return date.toLocaleString();
        } catch {
            return dateString;
        }
    }
}

// Export for use in other files
if (typeof module !== 'undefined' && module.exports) {
    module.exports = PostHelper;
}