// Graph visualization using D3.js for 4chan posts
class GraphView {
    constructor() {
        this.parser = new PrologParser();
        this.postHelper = new PostHelper(this.parser); // Initialize the shared helper
        this.posts = new Map();
        this.nodes = new Map();
        this.connections = [];
        this.showConnections = true;
        this.previewTimeout = null;
        
        // D3.js specific properties
        this.svg = null;
        this.simulation = null;
        this.zoom = null;
        this.width = 0;
        this.height = 0;
        
        this.init();
    }

    async init() {
        try {
            // Load the Prolog data from local copy
            const response = await fetch('data.pro');
            const prologText = await response.text();
            
            this.parser.loadData(prologText);
            this.parser.buildReplyTree();
            
            // Build the graph data
            this.buildGraph();
            
            // Initialize D3.js
            this.initD3();
            
            // Render the graph
            this.renderGraph();
            this.updateStats();
            
        } catch (error) {
            console.error('Failed to load data:', error);
            this.showError('Failed to load graph data');
        }
    }

    buildGraph() {
        const allPosts = this.parser.getAllPosts();
        
        // Create nodes for all posts
        allPosts.forEach(post => {
            this.posts.set(post.id, post);
            
            // Calculate node type based on post characteristics
            const nodeType = this.getNodeType(post);
            
            this.nodes.set(post.id, {
                id: post.id,
                post: post,
                type: nodeType,
                x: 0,
                y: 0,
                connections: []
            });
        });
        
        // Create connections based on references
        allPosts.forEach(post => {
            post.references.forEach(refId => {
                if (this.nodes.has(refId)) {
                    const sourceNode = this.nodes.get(refId);
                    const targetNode = this.nodes.get(post.id);
                    
                    this.connections.push({
                        source: targetNode,
                        target: sourceNode,
                        id: `${refId}-${post.id}`
                    });
                    
                    sourceNode.connections.push(targetNode);
                }
            });
        });
    }

    getNodeType(post) {
        // Color based on whether post has references or not
        if (post.references.length === 0) {
            return 'root'; // No references - root/original post
        } else {
            return 'reply'; // Has references - reply post
        }
    }

    initD3() {
        // Get container dimensions
        const container = document.getElementById('graph-container');
        const containerRect = container.getBoundingClientRect();
        this.width = containerRect.width;
        this.height = containerRect.height;
        
        // Create SVG
        this.svg = d3.select('#graph-svg');
        
        // Set up zoom behavior
        this.zoom = d3.zoom()
            .scaleExtent([0.1, 10])
            .on('zoom', (event) => {
                this.svg.select('#nodes-group').attr('transform', event.transform);
                this.svg.select('#links-group').attr('transform', event.transform);
            });
        
        // Apply zoom to SVG
        this.svg.call(this.zoom);
        
        // Create force simulation with adjusted parameters
        this.simulation = d3.forceSimulation()
            .force('link', d3.forceLink().id(d => d.id).distance(80).strength(0.8))
            .force('charge', d3.forceManyBody().strength(-300)) // Stronger repulsion
            .force('center', d3.forceCenter(this.width / 2, this.height / 2))
            .force('collision', d3.forceCollide().radius(30))
            .force('x', d3.forceX(this.width / 2).strength(0.05)) // Gentle pull to center
            .force('y', d3.forceY(this.height / 2).strength(0.05)) // Gentle pull to center
            .alphaDecay(0.02) // Slower cooling for better final layout
            .velocityDecay(0.3); // Reduce oscillations
    }

    renderGraph() {
        // Prepare data for D3
        const nodes = Array.from(this.nodes.values());
        const links = this.connections.map(conn => ({
            source: conn.source.id,
            target: conn.target.id,
            id: conn.id
        }));
        
        // Position orphan nodes closer to the center initially
        this.positionOrphanNodes(nodes, links);
        
        // Render links
        this.renderLinks(links);
        
        // Render nodes
        this.renderNodes(nodes);
        
        // Set up simulation
        this.simulation.nodes(nodes);
        this.simulation.force('link').links(links);
        
        // Start simulation
        this.simulation.on('tick', () => {
            this.updateLinkPositions();
            this.updateNodePositions();
        });
        
        this.simulation.restart();
    }

    positionOrphanNodes(nodes, links) {
        // Find nodes that have no connections (orphan nodes)
        const connectedNodeIds = new Set();
        links.forEach(link => {
            connectedNodeIds.add(link.source);
            connectedNodeIds.add(link.target);
        });
        
        const orphanNodes = nodes.filter(node => !connectedNodeIds.has(node.id));
        const connectedNodes = nodes.filter(node => connectedNodeIds.has(node.id));
        
        // Position orphan nodes in a circle around the center, closer to connected nodes
        const centerX = this.width / 2;
        const centerY = this.height / 2;
        const radius = Math.min(this.width, this.height) * 0.15; // Closer to center
        
        orphanNodes.forEach((node, index) => {
            const angle = (index / orphanNodes.length) * 2 * Math.PI;
            node.x = centerX + radius * Math.cos(angle);
            node.y = centerY + radius * Math.sin(angle);
            
            // Add some randomness to prevent perfect circle
            node.x += (Math.random() - 0.5) * 50;
            node.y += (Math.random() - 0.5) * 50;
        });
        
        // Position connected nodes randomly near center (they'll be pulled by forces)
        connectedNodes.forEach(node => {
            node.x = centerX + (Math.random() - 0.5) * 100;
            node.y = centerY + (Math.random() - 0.5) * 100;
        });
    }

    renderLinks(links) {
        const linksGroup = this.svg.select('#links-group');
        
        // Remove existing links
        linksGroup.selectAll('.link').remove();
        
        // Create link elements
        const link = linksGroup.selectAll('.link')
            .data(links)
            .enter().append('line')
            .attr('class', 'link')
            .attr('id', d => `link-${d.id}`)
            .style('display', this.showConnections ? 'block' : 'none');
        
        // Add hover events
        // link.on('mouseenter', (event, d) => {
        //     this.showLinkTooltip(event, d);
        // })
        // .on('mouseleave', () => {
        //     this.hideLinkTooltip();
        // });
    }

    renderNodes(nodes) {
        const nodesGroup = this.svg.select('#nodes-group');
        
        // Remove existing nodes
        nodesGroup.selectAll('.node-group').remove();
        
        // Create node groups
        const nodeGroup = nodesGroup.selectAll('.node-group')
            .data(nodes)
            .enter().append('g')
            .attr('class', 'node-group')
            .call(d3.drag()
                .on('start', (event, d) => this.dragstarted(event, d))
                .on('drag', (event, d) => this.dragged(event, d))
                .on('end', (event, d) => this.dragended(event, d)));
        
        // Add circles for nodes
        nodeGroup.append('circle')
            .attr('class', d => `node ${d.type}`)
            .attr('r', 25)
            .attr('id', d => `node-${d.id}`);
        
        // Add labels
        nodeGroup.append('text')
            .attr('class', 'node-label')
            .text(d => {
                const shortId = d.id.length > 8 ? d.id.slice(-8) : d.id;
                return shortId;
            });
        
        // Add hover events
        nodeGroup.on('mouseenter', (event, d) => {
            this.showPostPreview(d.post, event);
        })
        .on('mouseleave', () => {
            this.hidePostPreview();
        })
        .on('click', (event, d) => {
            this.focusNode(d.id);
        });
    }

    updateLinkPositions() {
        this.svg.selectAll('.link')
            .attr('x1', d => d.source.x)
            .attr('y1', d => d.source.y)
            .attr('x2', d => d.target.x)
            .attr('y2', d => d.target.y);
    }

    updateNodePositions() {
        this.svg.selectAll('.node-group')
            .attr('transform', d => `translate(${d.x},${d.y})`);
    }

    // Drag behavior methods
    dragstarted(event, d) {
        if (!event.active) this.simulation.alphaTarget(0.3).restart();
        d.fx = d.x;
        d.fy = d.y;
    }

    dragged(event, d) {
        d.fx = event.x;
        d.fy = event.y;
    }

    dragended(event, d) {
        if (!event.active) this.simulation.alphaTarget(0);
        d.fx = null;
        d.fy = null;
    }

    // Tooltip methods
    showLinkTooltip(event, d) {
        const tooltip = document.getElementById('link-tooltip');
        tooltip.textContent = `${d.target.id} → ${d.source.id}`;
        tooltip.style.left = `${event.pageX + 10}px`;
        tooltip.style.top = `${event.pageY - 10}px`;
        tooltip.style.display = 'block';
    }

    hideLinkTooltip() {
        const tooltip = document.getElementById('link-tooltip');
        tooltip.style.display = 'none';
    }

    // Post preview methods (reused from original)
    showPostPreview(post, event) {
        const preview = document.getElementById('post-preview');
        const postElement = this.postHelper.createPostElement(post, false, 0, 'preview-post');
        
        // Clear previous content and add new post
        preview.innerHTML = '';
        preview.appendChild(postElement);
        
        // Position the preview near the cursor
        const container = document.getElementById('graph-container');
        const containerRect = container.getBoundingClientRect();
        
        let left = event.clientX - containerRect.left + 10;
        let top = event.clientY - containerRect.top + 10;
        
        // Ensure preview stays within container bounds
        const previewRect = preview.getBoundingClientRect();
        if (left + 400 > containerRect.width) left = event.clientX - containerRect.left - 410;
        if (top + 300 > containerRect.height) top = event.clientY - containerRect.top - 310;
        
        preview.style.left = `${left}px`;
        preview.style.top = `${top}px`;
        preview.style.display = 'block';
        
        // Clear any existing timeout
        if (this.previewTimeout) {
            clearTimeout(this.previewTimeout);
        }
    }

    hidePostPreview() {
        // Add a small delay to prevent flickering when moving between nodes
        this.previewTimeout = setTimeout(() => {
            const preview = document.getElementById('post-preview');
            preview.style.display = 'none';
        }, 100);
    }

    // Reuse the shared helper for creating post elements
    createPostElement(post, isReply = false, depth = 0) {
        // Delegate to the shared helper with preview-post prefix for graph view
        return this.postHelper.createPostElement(post, isReply, depth, 'preview-post');
    }

    // Control methods
    resetLayout() {
        // Get current nodes and links
        const nodes = Array.from(this.nodes.values());
        const links = this.connections.map(conn => ({
            source: conn.source.id,
            target: conn.target.id,
            id: conn.id
        }));
        
        // Use the improved positioning logic
        this.positionOrphanNodes(nodes, links);
        
        // Restart simulation with new positions
        this.simulation.nodes(nodes);
        this.simulation.alpha(1).restart();
    }

    toggleConnections() {
        this.showConnections = !this.showConnections;
        this.svg.selectAll('.link')
            .style('display', this.showConnections ? 'block' : 'none');
    }

    centerGraph() {
        // Reset zoom to center
        this.svg.transition()
            .duration(750)
            .call(this.zoom.transform, d3.zoomIdentity);
    }

    focusNode(nodeId) {
        const node = this.nodes.get(nodeId);
        if (node) {
            // Highlight the node
            const nodeElement = d3.select(`#node-${nodeId}`);
            nodeElement.style('fill', '#ffe')
                       .style('stroke', '#800000')
                       .style('stroke-width', '4px');
            
            // Reset after a delay
            setTimeout(() => {
                const originalType = node.type;
                nodeElement.style('fill', originalType === 'root' ? '#ffe' : '#f0e0d6')
                           .style('stroke', originalType === 'root' ? '#800000' : '#d9bfa7')
                           .style('stroke-width', originalType === 'root' ? '3px' : '2px');
            }, 2000);
        }
    }

    updateStats() {
        const stats = document.getElementById('graph-stats');
        stats.innerHTML = `
            Nodes: ${this.nodes.size}<br>
            Connections: ${this.connections.length}<br>
            <small>Drag nodes to rearrange • Scroll to zoom</small>
        `;
    }

    showError(message) {
        const container = document.getElementById('graph-container');
        const error = document.createElement('div');
        error.className = 'error';
        error.style.position = 'absolute';
        error.style.top = '50%';
        error.style.left = '50%';
        error.style.transform = 'translate(-50%, -50%)';
        error.textContent = message;
        container.appendChild(error);
    }
}

// Initialize the graph when DOM is loaded
document.addEventListener('DOMContentLoaded', () => {
    window.graphView = new GraphView();
});