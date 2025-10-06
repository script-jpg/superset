// Chat-Rooms Application
class ChatRoomsApp {
    constructor() {
        this.parser = new PrologParser();
        this.rooms = [];
        this.currentRoom = null;
        this.prologText = ''; // Store the raw text for manual parsing
        this.init();
    }

    async init() {
        try {
            // Load the Prolog data from the local copy
            const response = await fetch('data.pro');
            this.prologText = await response.text();
            
            console.log('Loaded prolog text length:', this.prologText.length);
            console.log('First few lines:', this.prologText.split('\n').slice(0, 3));
            
            this.parseRooms();
            console.log('Parsed rooms count:', this.rooms.length);
            
            this.displayRooms();
        } catch (error) {
            console.error('Failed to load room data:', error);
            this.showError('Failed to load chat room data: ' + error.message);
        }
    }

    parseRooms() {
        // Parse room data using regex to handle the multi-line format
        const roomPattern = /room\("([^"]+)",\s*\[(.*?)\],\s*\[(.*?)\]\)\./gs;
        const matches = this.prologText.matchAll(roomPattern);
        
        this.rooms = [];
        
        for (const match of matches) {
            const [, name, messagesStr, usersStr] = match;
            
            // Parse messages - handle the tuple format ("author","message")
            const messages = [];
            const messageMatches = messagesStr.match(/\("([^"]+)","([^"]+)"\)/g);
            if (messageMatches) {
                messageMatches.forEach(msgMatch => {
                    const msgContent = msgMatch.match(/\("([^"]+)","([^"]+)"\)/);
                    if (msgContent) {
                        messages.push({
                            author: msgContent[1],
                            content: msgContent[2]
                        });
                    }
                });
            }
            
            // Parse users - handle the list format [user1,user2,user3]
            const users = usersStr.split(',').map(user => user.trim());
            
            this.rooms.push({
                name: name,
                messages: messages,
                users: users,
                messageCount: messages.length,
                userCount: users.length
            });
        }
        
        console.log('Successfully parsed rooms:', this.rooms.length);
        if (this.rooms.length > 0) {
            console.log('First room:', this.rooms[0]);
        }
    }

    displayRooms() {
        const container = document.getElementById('rooms-grid');
        console.log('Displaying rooms, count:', this.rooms.length);
        container.innerHTML = '';
        
        if (this.rooms.length === 0) {
            console.log('No rooms found, showing loading message');
            container.innerHTML = '<div class="loading">No Chat-Rooms found</div>';
            return;
        }
        
        console.log('Creating room cards for', this.rooms.length, 'rooms');
        this.rooms.forEach((room, index) => {
            console.log('Creating card for room:', room.name);
            const roomCard = this.createRoomCard(room, index);
            container.appendChild(roomCard);
        });
    }

    createRoomCard(room, index) {
        const card = document.createElement('div');
        card.className = 'room-card';
        card.dataset.roomIndex = index;
        
        // Room title
        const title = document.createElement('div');
        title.className = 'room-title';
        title.textContent = room.name;
        card.appendChild(title);
        
        // Room description (first message as description)
        if (room.messages.length > 0) {
            const description = document.createElement('div');
            description.className = 'room-description';
            description.textContent = room.messages[0].content.substring(0, 100) + 
                (room.messages[0].content.length > 100 ? '...' : '');
            card.appendChild(description);
        }
        
        // Users
        const usersContainer = document.createElement('div');
        usersContainer.className = 'room-users';
        
        const usersLabel = document.createElement('div');
        usersLabel.style.fontSize = '11px';
        usersLabel.style.color = '#666';
        usersLabel.style.marginBottom = '5px';
        usersLabel.textContent = 'Users:';
        usersContainer.appendChild(usersLabel);
        
        room.users.slice(0, 8).forEach(user => {
            const userTag = document.createElement('span');
            userTag.className = 'user-tag';
            userTag.textContent = user;
            usersContainer.appendChild(userTag);
        });
        
        if (room.users.length > 8) {
            const moreTag = document.createElement('span');
            moreTag.className = 'user-tag';
            moreTag.textContent = `+${room.users.length - 8} more`;
            usersContainer.appendChild(moreTag);
        }
        
        card.appendChild(usersContainer);
        
        // Stats
        const stats = document.createElement('div');
        stats.className = 'room-stats';
        stats.innerHTML = `
            <div>📨 ${room.messageCount} messages</div>
            <div>👥 ${room.userCount} users</div>
        `;
        card.appendChild(stats);
        
        // Click handler
        card.addEventListener('click', () => {
            this.showRoomMessages(index);
        });
        
        return card;
    }

    showRoomMessages(roomIndex) {
        const room = this.rooms[roomIndex];
        if (!room) return;
        
        this.currentRoom = room;
        
        // Hide rooms list, show messages
        document.getElementById('rooms-list').style.display = 'none';
        document.getElementById('room-messages').style.display = 'block';
        
        // Update room title
        document.getElementById('current-room-title').textContent = room.name;
        
        // Update room users
        const usersContainer = document.getElementById('current-room-users');
        usersContainer.innerHTML = '';
        
        const usersLabel = document.createElement('div');
        usersLabel.style.fontSize = '12px';
        usersLabel.style.color = '#666';
        usersLabel.style.marginBottom = '10px';
        usersLabel.textContent = 'Users in this room:';
        usersContainer.appendChild(usersLabel);
        
        room.users.forEach(user => {
            const userTag = document.createElement('span');
            userTag.className = 'user-tag';
            userTag.textContent = user;
            usersContainer.appendChild(userTag);
        });
        
        // Display messages
        this.displayMessages(room.messages);
        
        // Set up back button
        document.getElementById('back-button').onclick = () => {
            this.showRoomsList();
        };
    }

    displayMessages(messages) {
        const container = document.getElementById('messages-container');
        container.innerHTML = '';
        
        if (messages.length === 0) {
            container.innerHTML = '<div class="loading">No messages in this room</div>';
            return;
        }
        
        messages.forEach(message => {
            const messageElement = this.createMessageElement(message);
            container.appendChild(messageElement);
        });
    }

    createMessageElement(message) {
        const messageDiv = document.createElement('div');
        messageDiv.className = 'message';
        
        // Message header
        const header = document.createElement('div');
        header.className = 'message-header';
        
        const author = document.createElement('div');
        author.className = 'message-author';
        author.textContent = message.author;
        header.appendChild(author);
        
        messageDiv.appendChild(header);
        
        // Message content
        const content = document.createElement('div');
        content.className = 'message-content';
        content.innerHTML = this.formatMessageContent(message.content);
        messageDiv.appendChild(content);
        
        return messageDiv;
    }

    formatMessageContent(content) {
        // Handle greentext formatting
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

    showRoomsList() {
        document.getElementById('room-messages').style.display = 'none';
        document.getElementById('rooms-list').style.display = 'block';
        this.currentRoom = null;
    }

    showError(message) {
        const container = document.getElementById('rooms-grid');
        container.innerHTML = `<div class="error">${message}</div>`;
    }
}

// Initialize the app when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    new ChatRoomsApp();
});