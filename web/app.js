// Main application
class ChanApp {
    constructor() {
        this.parser = new PrologParser();
        this.postHelper = new PostHelper(this.parser); // Initialize the shared helper
        this.expandedReplies = new Set();
        this.init();
    }

    async init() {
        try {
            // Load the Prolog data from local copy
            const response = await fetch('data.pro');
            const prologText = await response.text();
            
            this.parser.loadData(prologText);
            this.parser.buildReplyTree();
            
            // Display the main thread starting from post p1
            this.displayThread();
        } catch (error) {
            console.error('Failed to load data:', error);
            this.showError('Failed to load board data');
        }
    }

    displayThread() {
        const container = document.getElementById('thread-container');
        container.innerHTML = '';
        
        // Get all posts to display the full board
        const allPosts = this.parser.getAllPosts();
        
        // Find root posts (posts that don't reference other posts)
        const rootPosts = allPosts.filter(post => post.references.length === 0);
        
        // Display each root post and its thread
        rootPosts.forEach(rootPost => {
            const threadSection = document.createElement('div');
            threadSection.className = 'thread-section';
            threadSection.dataset.threadId = rootPost.id; // Add thread ID for context checking
            
            // Display the root post using the shared helper
            const rootPostElement = this.postHelper.createPostElement(rootPost, false, 0);
            threadSection.appendChild(rootPostElement);
            
            // Find all replies to this root post
            const replies = this.findAllReplies(rootPost.id, allPosts);
            if (replies.length > 0) {
                const repliesContainer = document.createElement('div');
                repliesContainer.className = 'replies-container';
                repliesContainer.style.marginLeft = '40px';
                
                const showRepliesBtn = document.createElement('button');
                showRepliesBtn.className = 'show-replies-btn';
                showRepliesBtn.textContent = `Show ${replies.length} replies`;
                showRepliesBtn.onclick = () => this.toggleAllReplies(rootPost.id, repliesContainer, showRepliesBtn, allPosts);
                
                threadSection.appendChild(showRepliesBtn);
                threadSection.appendChild(repliesContainer);
            }
            
            container.appendChild(threadSection);
        });
    }

    // Find all replies to a post (recursive)
    findAllReplies(postId, allPosts) {
        return allPosts.filter(post => post.references.includes(postId));
    }

    // Toggle display of all replies to a post
    toggleAllReplies(postId, container, button, allPosts) {
        if (this.expandedReplies.has(postId)) {
            // Hide replies
            container.innerHTML = '';
            button.textContent = button.textContent.replace('Hide', 'Show');
            this.expandedReplies.delete(postId);
        } else {
            // Show all replies recursively
            container.innerHTML = '';
            this.displayRepliesRecursive(postId, container, allPosts, 0);
            button.textContent = button.textContent.replace('Show', 'Hide');
            this.expandedReplies.add(postId);
        }
    }

    // Display replies recursively - 4chan authentic approach
    displayRepliesRecursive(postId, container, allPosts, depth) {
        const replies = this.findAllReplies(postId, allPosts);
        
        replies.forEach(reply => {
            // Check if this post is already visible in the current thread context
            const existingPost = document.getElementById(`post-${reply.id}`);
            if (existingPost && existingPost.closest('.thread-section') === container.closest('.thread-section')) {
                // Post already exists in this thread, just add a reference marker
                const marker = document.createElement('div');
                marker.className = 'reply-marker';
                marker.style.marginLeft = `${(depth + 1) * 40}px`;
                marker.style.fontSize = '11px';
                marker.style.color = '#666';
                marker.style.padding = '2px 0';
                marker.innerHTML = `<span class="greentext">&gt;&gt;${reply.id} (in thread)</span>`;
                container.appendChild(marker);
                return;
            }
            
            // Post not in this context yet, display it normally
            const replyElement = this.postHelper.createPostElement(reply, true, depth + 1);
            
            // Check if this reply has its own replies
            const nestedReplies = this.findAllReplies(reply.id, allPosts);
            if (nestedReplies.length > 0) {
                const nestedContainer = document.createElement('div');
                nestedContainer.className = 'replies-container';
                
                const nestedShowBtn = document.createElement('button');
                nestedShowBtn.className = 'show-replies-btn';
                nestedShowBtn.textContent = `Show ${nestedReplies.length} replies`;
                nestedShowBtn.onclick = () => this.toggleAllReplies(reply.id, nestedContainer, nestedShowBtn, allPosts);
                
                replyElement.appendChild(nestedShowBtn);
                replyElement.appendChild(nestedContainer);
            }
            
            container.appendChild(replyElement);
        });
    }

    createPostElement(post, isReply = false, depth = 0) {
        // Delegate to the shared helper
        return this.postHelper.createPostElement(post, isReply, depth);
    }

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

    formatDate(dateString) {
        if (dateString === 'nothing') return '';
        try {
            const date = new Date(dateString);
            return date.toLocaleString();
        } catch {
            return dateString;
        }
    }

    showError(message) {
        const container = document.getElementById('thread-container');
        container.innerHTML = `<div class="error">${message}</div>`;
    }
}

// Initialize the app when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    new ChanApp();
});