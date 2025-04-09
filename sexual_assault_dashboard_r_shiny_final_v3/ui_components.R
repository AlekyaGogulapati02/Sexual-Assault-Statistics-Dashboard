library(shiny)
library(shinydashboard)

# This file contains helper functions for UI components
# and custom UI elements for the application

# Custom CSS
custom_css <- "
.sidebar-login {
  padding: 10px;
  margin-top: 20px;
}

.top-nav {
  background-color: #2c3e50;
  padding: 10px;
  margin-bottom: 20px;
  border-radius: 5px;
}

.nav-button {
  margin-right: 10px;
  background-color: transparent;
  border: 1px solid white;
  color: white;
}

.nav-button:hover {
  background-color: #34495e;
}

.thread-item {
  border: 1px solid #ddd;
  border-radius: 5px;
  padding: 15px;
  margin-bottom: 15px;
  background-color: #f9f9f9;
}

.thread-header {
  margin-bottom: 10px;
}

.thread-meta, .comment-meta {
  color: #666;
  font-size: 0.9em;
}

.thread-preview {
  color: #333;
  margin-bottom: 10px;
}

.thread-content {
  border: 1px solid #ddd;
  border-radius: 5px;
  padding: 20px;
  background-color: white;
  margin-bottom: 20px;
}

.thread-body {
  margin: 20px 0;
  padding-bottom: 20px;
  border-bottom: 1px solid #eee;
}

.comments-section {
  margin-top: 20px;
}

.comment-item {
  border-left: 3px solid #3c8dbc;
  padding: 10px 15px;
  margin-bottom: 15px;
  background-color: #f5f5f5;
}

.comment-body {
  margin-top: 5px;
}

.comment-form {
  margin-top: 20px;
  padding: 15px;
  background-color: #f9f9f9;
  border-radius: 5px;
  border: 1px solid #ddd;
}

.report-form {
  padding: 20px;
  background-color: #f9f9f9;
  border-radius: 5px;
  border: 1px solid #ddd;
}
"

# Create CSS file
create_css_file <- function() {
  # Create www directory if it doesn't exist
  if (!dir.exists("www")) {
    dir.create("www")
  }
  
  # Write CSS to file
  writeLines(custom_css, "www/custom.css")
}