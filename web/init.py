#!/usr/bin/env python3
"""
Initialization script for the web application.
Sets up a Python HTTP server, copies data file, and opens the web interface.
Includes watchdog to monitor for file changes and reload browser.
"""

import http.server
import socketserver
import webbrowser
import shutil
import os
import threading
import time
from pathlib import Path
from datetime import datetime

class FileWatcher:
    """Watchdog class to monitor file changes"""
    def __init__(self, source_path, target_path, check_interval=1.0):
        self.source_path = source_path
        self.target_path = target_path
        self.check_interval = check_interval
        self.last_modified = None
        self.running = False
        self.thread = None
        self.initial_check = True  # Track if this is the initial check
        
    def get_file_modified_time(self):
        """Get the last modified time of the source file"""
        try:
            return os.path.getmtime(self.source_path)
        except (OSError, FileNotFoundError):
            return None
    
    def copy_file(self):
        """Copy the file and update the last modified time"""
        try:
            shutil.copy2(self.source_path, self.target_path)
            self.last_modified = self.get_file_modified_time()
            timestamp = datetime.now().strftime("%H:%M:%S")
            print(f"✓ [{timestamp}] Auto-copied {self.source_path.name} to {self.target_path.name}")
            
            # Only reload browser if this is not the initial check
            if not self.initial_check:
                self.reload_browser()
            
        except Exception as e:
            print(f"✗ Error copying file: {e}")
    
    def reload_browser(self):
        """Reload the browser window"""
        try:
            # Open a new tab/window which will trigger a reload
            url = "http://localhost:8080/index.html"
            webbrowser.open(url, new=0)  # new=0 opens in same window if possible
            timestamp = datetime.now().strftime("%H:%M:%S")
            print(f"✓ [{timestamp}] Reloaded browser: {url}")
        except Exception as e:
            print(f"✗ Error reloading browser: {e}")
    
    def check_for_changes(self):
        """Check if the file has been modified"""
        current_modified = self.get_file_modified_time()
        
        if current_modified is None:
            return
            
        if self.last_modified is None:
            # First time checking, just set the baseline
            self.last_modified = current_modified
            self.copy_file()
            self.initial_check = False  # Mark initial check as complete
        elif current_modified != self.last_modified:
            # File has been modified
            self.copy_file()
    
    def watch_loop(self):
        """Main watch loop that runs in a separate thread"""
        while self.running:
            self.check_for_changes()
            time.sleep(self.check_interval)
    
    def start(self):
        """Start the watchdog"""
        if self.running:
            return
            
        self.running = True
        # Do initial copy/check
        self.check_for_changes()
        # Start the watch thread
        self.thread = threading.Thread(target=self.watch_loop, daemon=True)
        self.thread.start()
        print(f"✓ Started watching {self.source_path.name} for changes")
    
    def stop(self):
        """Stop the watchdog"""
        self.running = False
        if self.thread:
            self.thread.join(timeout=2.0)
        print(f"✓ Stopped watching {self.source_path.name}")

def copy_data_file():
    """Copy persisted_data.pro from ../data/ to web/data.pro"""
    source_path = Path(__file__).parent.parent / "data" / "persisted_data.pro"
    target_path = Path(__file__).parent / "data.pro"
    
    try:
        if source_path.exists():
            shutil.copy2(source_path, target_path)
            print(f"✓ Copied {source_path} to {target_path}")
        else:
            print(f"⚠ Source file not found: {source_path}")
    except Exception as e:
        print(f"✗ Error copying data file: {e}")

def start_server():
    """Start Python HTTP server on port 8080"""
    PORT = 8080
    Handler = http.server.SimpleHTTPRequestHandler
    
    try:
        with socketserver.TCPServer(("", PORT), Handler) as httpd:
            print(f"✓ Server started at http://localhost:{PORT}")
            print("  Press Ctrl+C to stop the server")
            httpd.serve_forever()
    except OSError as e:
        if e.errno == 48:  # Address already in use
            print(f"✗ Port {PORT} is already in use. Try a different port.")
        else:
            print(f"✗ Error starting server: {e}")
    except KeyboardInterrupt:
        print("\n✓ Server stopped by user")
    except Exception as e:
        print(f"✗ Unexpected error: {e}")

def open_browser():
    """Open index.html in the default web browser"""
    try:
        # Wait a moment for server to start
        time.sleep(1)
        url = "http://localhost:8080/index.html"
        webbrowser.open(url)
        print(f"✓ Opening {url} in browser")
    except Exception as e:
        print(f"✗ Error opening browser: {e}")

def main():
    """Main initialization function"""
    print("🚀 Initializing web application...")
    
    # Set up file paths
    source_path = Path(__file__).parent.parent / "data" / "persisted_data.pro"
    target_path = Path(__file__).parent / "data.pro"
    
    # Create and start file watcher
    watcher = FileWatcher(source_path, target_path, check_interval=1.0)
    watcher.start()
    
    # Copy data file (initial copy)
    copy_data_file()
    
    # Start server in a separate thread
    server_thread = threading.Thread(target=start_server, daemon=True)
    server_thread.start()
    
    # Give server a moment to start
    time.sleep(0.5)
    
    # Open browser
    open_browser()
    
    # Keep main thread alive
    try:
        while server_thread.is_alive():
            time.sleep(1)
    except KeyboardInterrupt:
        print("\n✓ Shutting down...")
        watcher.stop()

if __name__ == "__main__":
    main()