library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(digest)
library(jsonlite)
library(stringr)

# Use UUIDgenerate function directly without uuid package
# This helps avoid the dependency issue
generate_uuid <- function() {
  hex_digits <- c(as.character(0:9), letters[1:6])
  sections <- c(8, 4, 4, 4, 12)
  
  uuid <- paste(
    sapply(sections, function(section) {
      paste(sample(hex_digits, section, replace = TRUE), collapse = "")
    }),
    collapse = "-"
  )
  
  return(uuid)
}

# File paths
USERS_FILE <- "users.json"
DISCUSSIONS_FILE <- "discussions.json"
USER_REPORTS_FILE <- "user_reports.json"
SAMPLE_DATA_FILE <- "sample_data.csv"
VOTES_FILE <- "votes.json"

# Initialize files if they don't exist
initialize_files <- function() {
  # Users file
  if (!file.exists(USERS_FILE)) {
    write_json(list(), USERS_FILE)
  }
  
  # Discussions file
  if (!file.exists(DISCUSSIONS_FILE)) {
    write_json(list(threads = list(), comments = list()), DISCUSSIONS_FILE)
  }
  
  # Create sample data if it doesn't exist
  if (!file.exists(SAMPLE_DATA_FILE)) {
    # Create sample data with Kalamazoo-specific coordinates
    sample_data <- data.frame(
      date = c("2023-01-15", "2023-02-03", "2023-02-20", "2023-03-05", "2023-03-18"),
      type = c("Sexual assault", "Sexual harassment", "Attempted sexual assault", 
              "Sexual assault", "Sexual harassment"),
      location = c("Kalamazoo College", "Downtown Kalamazoo", "Western Michigan University", 
                  "Milwood Neighborhood", "Vine Neighborhood"),
      description = c("Incident reported near dormitory", 
                     "Street harassment incident", 
                     "Attempted assault in parking lot", 
                     "Assault in residential area", 
                     "Harassment at local business"),
      latitude = c(42.2900, 42.2922, 42.2829, 42.2706, 42.2845),
      longitude = c(-85.6041, -85.5872, -85.6159, -85.5728, -85.5941),
      stringsAsFactors = FALSE
    )
    
    # Save sample data
    write.csv(sample_data, SAMPLE_DATA_FILE, row.names = FALSE)
  }
  
  # Create user reports file if it doesn't exist
  if (!file.exists(USER_REPORTS_FILE)) {
    write_json(list(), USER_REPORTS_FILE)
  }
  
  # Create votes file if it doesn't exist
  if (!file.exists(VOTES_FILE)) {
    write_json(list(), VOTES_FILE)
  }
  
  # Create www directory for CSS if it doesn't exist
  if (!dir.exists("www")) {
    dir.create("www")
  }
  
  # Create custom CSS file
  write_css_file()
}

# Custom CSS
write_css_file <- function() {
  custom_css <- "
.sidebar-login {
  padding: 10px;
  margin-top: 20px;
}

.sidebar-login .btn {
  font-size: 12px;
  padding: 5px 10px;
  max-width: 90%;
  margin: 0 auto 5px auto;
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

.incident-card {
  border: 1px solid #ddd;
  border-radius: 5px;
  padding: 15px;
  margin-bottom: 15px;
  background-color: #f9f9f9;
}

.incident-title {
  font-weight: bold;
  margin-bottom: 5px;
}

.incident-details {
  font-size: 0.9em;
  color: #555;
}

.box-header {
  background-color: #3c8dbc;
  color: white;
}

.content-wrapper {
  background-color: #f4f6f9;
}

.map-container {
  border: 1px solid #ddd;
  border-radius: 5px;
  overflow: hidden;
  margin-bottom: 20px;
}

.data-section {
  margin-top: 30px;
}

.vote-container {
  display: flex;
  align-items: center;
  margin-bottom: 10px;
}

.vote-count {
  margin: 0 10px;
  font-weight: bold;
}

.upvote-btn {
  color: #4CAF50;
}

.downvote-btn {
  color: #F44336;
}

.incident-tag {
  display: inline-block;
  background-color: #e74c3c;
  color: white;
  font-size: 0.8em;
  padding: 2px 6px;
  margin-left: 6px;
  border-radius: 3px;
}

.thread-sorting {
  margin-bottom: 15px;
}

.start-thread-btn {
  font-size: 16px;
  padding: 10px 20px;
  margin-bottom: 20px;
  background-color: #3c8dbc;
  border-color: #367fa9;
}

.forum-controls {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 20px;
}

.forum-filter {
  display: flex;
  align-items: center;
}

.filter-label {
  margin-right: 10px;
}
"
  
  # Create www directory if it doesn't exist
  if (!dir.exists("www")) {
    dir.create("www")
  }
  
  # Write CSS to file
  writeLines(custom_css, "www/custom.css")
}

# Hash password with salt
hash_password <- function(password, salt = NULL) {
  if (is.null(salt)) {
    salt <- digest(generate_uuid(), algo = "sha256")
  }
  
  hashed <- digest(paste0(password, salt), algo = "sha256")
  
  return(list(
    hash = hashed,
    salt = salt
  ))
}

# Register a new user
register_user <- function(username, password, contact = NULL, contact_type = NULL) {
  # Read current users
  users <- fromJSON(USERS_FILE)
  users <- if (length(users) == 0) list() else users
  
  # Check if username exists
  usernames <- sapply(users, function(user) user$username)
  if (any(usernames == username)) {
    return(list(
      success = FALSE,
      message = "Username already exists"
    ))
  }
  
  # Generate user ID
  user_id <- generate_uuid()
  
  # Hash password
  pw_data <- hash_password(password)
  
  # Create user object
  user <- list(
    id = user_id,
    username = username,
    password_hash = pw_data$hash,
    password_salt = pw_data$salt,
    contact = contact,
    contact_type = contact_type,
    created_at = as.character(Sys.time())
  )
  
  # Add to users list
  users[[length(users) + 1]] <- user
  
  # Save to file
  write_json(users, USERS_FILE)
  
  return(list(
    success = TRUE,
    user_id = user_id,
    message = "User registered successfully"
  ))
}

# Authenticate a user
authenticate_user <- function(username, password) {
  # Read users
  users <- fromJSON(USERS_FILE)
  users <- if (length(users) == 0) list() else users
  
  # Find user by username
  user <- NULL
  user_index <- NULL
  
  for (i in seq_along(users)) {
    if (users[[i]]$username == username) {
      user <- users[[i]]
      user_index <- i
      break
    }
  }
  
  if (is.null(user)) {
    return(list(
      success = FALSE,
      message = "User not found"
    ))
  }
  
  # Check password
  pw_data <- hash_password(password, user$password_salt)
  
  if (pw_data$hash != user$password_hash) {
    return(list(
      success = FALSE,
      message = "Invalid password"
    ))
  }
  
  return(list(
    success = TRUE,
    user_id = user$id,
    message = "Authentication successful"
  ))
}

# Create a new discussion thread
create_thread <- function(user_id, title, content, report_id = NULL) {
  # Read discussions
  discussions <- fromJSON(DISCUSSIONS_FILE)
  if (is.null(discussions$threads)) {
    discussions$threads <- list()
  }
  if (is.null(discussions$comments)) {
    discussions$comments <- list()
  }
  
  # Generate thread ID
  thread_id <- generate_uuid()
  
  # Create thread object
  thread <- list(
    id = thread_id,
    user_id = user_id,
    title = title,
    content = content,
    report_id = report_id,
    created_at = as.character(Sys.time()),
    votes = 0
  )
  
  # Add to threads list
  discussions$threads[[length(discussions$threads) + 1]] <- thread
  
  # Save to file
  write_json(discussions, DISCUSSIONS_FILE)
  
  return(list(
    success = TRUE,
    thread_id = thread_id,
    message = "Thread created successfully"
  ))
}

# Add a comment to a thread
add_comment <- function(user_id, thread_id, content) {
  # Read discussions
  discussions <- fromJSON(DISCUSSIONS_FILE)
  if (is.null(discussions$comments)) {
    discussions$comments <- list()
  }
  
  # Generate comment ID
  comment_id <- generate_uuid()
  
  # Create comment object
  comment <- list(
    id = comment_id,
    thread_id = thread_id,
    user_id = user_id,
    content = content,
    created_at = as.character(Sys.time()),
    votes = 0
  )
  
  # Add to comments list
  discussions$comments[[length(discussions$comments) + 1]] <- comment
  
  # Save to file
  write_json(discussions, DISCUSSIONS_FILE)
  
  return(list(
    success = TRUE,
    comment_id = comment_id,
    message = "Comment added successfully"
  ))
}

# Vote on a thread or comment
add_vote <- function(user_id, item_id, vote_type) {
  # Read votes
  votes <- fromJSON(VOTES_FILE)
  votes <- if (length(votes) == 0) list() else votes
  
  # Check if user has already voted
  user_vote <- NULL
  user_vote_index <- NULL
  
  for (i in seq_along(votes)) {
    if (votes[[i]]$user_id == user_id && votes[[i]]$item_id == item_id) {
      user_vote <- votes[[i]]
      user_vote_index <- i
      break
    }
  }
  
  # Read discussions to update vote count
  discussions <- fromJSON(DISCUSSIONS_FILE)
  
  # Check if item is a thread or comment
  is_thread <- FALSE
  is_comment <- FALSE
  thread_index <- NULL
  comment_index <- NULL
  
  # Check threads
  for (i in seq_along(discussions$threads)) {
    if (discussions$threads[[i]]$id == item_id) {
      is_thread <- TRUE
      thread_index <- i
      break
    }
  }
  
  # Check comments if not a thread
  if (!is_thread) {
    for (i in seq_along(discussions$comments)) {
      if (discussions$comments[[i]]$id == item_id) {
        is_comment <- TRUE
        comment_index <- i
        break
      }
    }
  }
  
  # If neither thread nor comment, return error
  if (!is_thread && !is_comment) {
    return(list(
      success = FALSE,
      message = "Item not found"
    ))
  }
  
  # Process vote
  if (is.null(user_vote)) {
    # New vote
    vote <- list(
      user_id = user_id,
      item_id = item_id,
      vote_type = vote_type,
      created_at = as.character(Sys.time())
    )
    
    # Add to votes list
    votes[[length(votes) + 1]] <- vote
    
    # Update vote count
    if (is_thread) {
      if (is.null(discussions$threads[[thread_index]]$votes)) {
        discussions$threads[[thread_index]]$votes <- 0
      }
      if (vote_type == "upvote") {
        discussions$threads[[thread_index]]$votes <- discussions$threads[[thread_index]]$votes + 1
      } else {
        discussions$threads[[thread_index]]$votes <- discussions$threads[[thread_index]]$votes - 1
      }
    } else {
      if (is.null(discussions$comments[[comment_index]]$votes)) {
        discussions$comments[[comment_index]]$votes <- 0
      }
      if (vote_type == "upvote") {
        discussions$comments[[comment_index]]$votes <- discussions$comments[[comment_index]]$votes + 1
      } else {
        discussions$comments[[comment_index]]$votes <- discussions$comments[[comment_index]]$votes - 1
      }
    }
  } else {
    # Update existing vote
    old_vote_type <- votes[[user_vote_index]]$vote_type
    
    # If same vote type, remove vote
    if (old_vote_type == vote_type) {
      # Remove vote
      votes <- votes[-user_vote_index]
      
      # Update vote count
      if (is_thread) {
        if (vote_type == "upvote") {
          discussions$threads[[thread_index]]$votes <- discussions$threads[[thread_index]]$votes - 1
        } else {
          discussions$threads[[thread_index]]$votes <- discussions$threads[[thread_index]]$votes + 1
        }
      } else {
        if (vote_type == "upvote") {
          discussions$comments[[comment_index]]$votes <- discussions$comments[[comment_index]]$votes - 1
        } else {
          discussions$comments[[comment_index]]$votes <- discussions$comments[[comment_index]]$votes + 1
        }
      }
    } else {
      # Change vote
      votes[[user_vote_index]]$vote_type <- vote_type
      votes[[user_vote_index]]$created_at <- as.character(Sys.time())
      
      # Update vote count (double effect since changing from up to down or vice versa)
      if (is_thread) {
        if (vote_type == "upvote") {
          discussions$threads[[thread_index]]$votes <- discussions$threads[[thread_index]]$votes + 2
        } else {
          discussions$threads[[thread_index]]$votes <- discussions$threads[[thread_index]]$votes - 2
        }
      } else {
        if (vote_type == "upvote") {
          discussions$comments[[comment_index]]$votes <- discussions$comments[[comment_index]]$votes + 2
        } else {
          discussions$comments[[comment_index]]$votes <- discussions$comments[[comment_index]]$votes - 2
        }
      }
    }
  }
  
  # Save to files
  write_json(votes, VOTES_FILE)
  write_json(discussions, DISCUSSIONS_FILE)
  
  return(list(
    success = TRUE,
    message = "Vote recorded successfully"
  ))
}

# Check user's vote on an item
check_user_vote <- function(user_id, item_id) {
  # Read votes
  votes <- fromJSON(VOTES_FILE)
  votes <- if (length(votes) == 0) list() else votes
  
  # Find user's vote on this item
  for (vote in votes) {
    if (vote$user_id == user_id && vote$item_id == item_id) {
      return(vote$vote_type)
    }
  }
  
  return(NULL)
}

# Get threads list
get_threads <- function(limit = 10, offset = 0, sort_by = "newest", filter_reports = FALSE) {
  # Read discussions
  discussions <- fromJSON(DISCUSSIONS_FILE)
  
  # Check if discussions file is valid and has threads
  if (is.null(discussions) || is.null(discussions$threads) || length(discussions$threads) == 0) {
    return(list())
  }
  
  threads <- discussions$threads
  
  # Filter report threads if needed
  if (filter_reports) {
    filtered_threads <- list()
    for (thread in threads) {
      if (!is.null(thread$report_id)) {
        filtered_threads[[length(filtered_threads) + 1]] <- thread
      }
    }
    threads <- filtered_threads
  }
  
  # If no threads after filtering, return empty list
  if (length(threads) == 0) {
    return(list())
  }
  
  # Sort threads
  if (sort_by == "newest") {
    # Sort by created_at (newest first)
    created_times <- sapply(threads, function(t) as.POSIXct(t$created_at))
    ordered_indices <- order(created_times, decreasing = TRUE)
    threads <- threads[ordered_indices]
  } else if (sort_by == "top") {
    # Sort by votes (highest first)
    votes <- sapply(threads, function(t) {
      if (is.null(t$votes)) 0 else t$votes
    })
    ordered_indices <- order(votes, decreasing = TRUE)
    threads <- threads[ordered_indices]
  }
  
  # Apply limit and offset
  start_idx <- offset + 1
  end_idx <- min(offset + limit, length(threads))
  
  if (start_idx <= end_idx) {
    threads <- threads[start_idx:end_idx]
  } else {
    threads <- list()
  }
  
  return(threads)
}

# Get comments for a thread
get_thread_comments <- function(thread_id, sort_by = "oldest") {
  # Read discussions
  discussions <- fromJSON(DISCUSSIONS_FILE)
  
  # Check if discussions file is valid and has comments
  if (is.null(discussions) || is.null(discussions$comments) || length(discussions$comments) == 0) {
    return(list())
  }
  
  all_comments <- discussions$comments
  
  # Filter comments by thread_id
  thread_comments <- list()
  
  for (i in seq_along(all_comments)) {
    if (all_comments[[i]]$thread_id == thread_id) {
      thread_comments[[length(thread_comments) + 1]] <- all_comments[[i]]
    }
  }
  
  # If no comments for this thread, return empty list
  if (length(thread_comments) == 0) {
    return(list())
  }
  
  # Sort comments
  if (sort_by == "oldest") {
    # Sort by created_at (oldest first)
    created_times <- sapply(thread_comments, function(c) as.POSIXct(c$created_at))
    ordered_indices <- order(created_times)
    thread_comments <- thread_comments[ordered_indices]
  } else if (sort_by == "newest") {
    # Sort by created_at (newest first)
    created_times <- sapply(thread_comments, function(c) as.POSIXct(c$created_at))
    ordered_indices <- order(created_times, decreasing = TRUE)
    thread_comments <- thread_comments[ordered_indices]
  } else if (sort_by == "top") {
    # Sort by votes (highest first)
    votes <- sapply(thread_comments, function(c) {
      if (is.null(c$votes)) 0 else c$votes
    })
    ordered_indices <- order(votes, decreasing = TRUE)
    thread_comments <- thread_comments[ordered_indices]
  }
  
  return(thread_comments)
}

# Get user by ID
get_user_by_id <- function(user_id) {
  # Read users
  users <- fromJSON(USERS_FILE)
  users <- if (length(users) == 0) list() else users
  
  # Find user by ID
  for (user in users) {
    if (user$id == user_id) {
      return(user)
    }
  }
  
  return(NULL)
}

# Load data from both sample data and user reports
load_data <- function() {
  # Load sample data
  sample_data <- read.csv(SAMPLE_DATA_FILE, stringsAsFactors = FALSE)
  
  # Load user reports
  user_reports <- fromJSON(USER_REPORTS_FILE)
  
  # If user reports is empty, initialize as empty data frame
  if (length(user_reports) == 0) {
    user_reports <- data.frame(
      date = character(),
      type = character(),
      location = character(),
      description = character(),
      latitude = numeric(),
      longitude = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    # Convert list to data frame
    user_reports_list <- list()
    
    for (i in seq_along(user_reports)) {
      report <- user_reports[[i]]
      # Only include reports that should be on the map
      if (!is.null(report$include_on_map) && report$include_on_map) {
        user_reports_list[[length(user_reports_list) + 1]] <- data.frame(
          date = report$date,
          type = report$type,
          location = report$location,
          description = report$description,
          latitude = as.numeric(report$latitude),
          longitude = as.numeric(report$longitude),
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Combine all user reports into one data frame
    if (length(user_reports_list) > 0) {
      user_reports <- do.call(rbind, user_reports_list)
    } else {
      user_reports <- data.frame(
        date = character(),
        type = character(),
        location = character(),
        description = character(),
        latitude = numeric(),
        longitude = numeric(),
        stringsAsFactors = FALSE
      )
    }
  }
  
  # Combine data
  if (nrow(user_reports) > 0) {
    # Check if columns match
    missing_cols <- setdiff(names(sample_data), names(user_reports))
    for (col in missing_cols) {
      user_reports[[col]] <- NA
    }
    missing_cols <- setdiff(names(user_reports), names(sample_data))
    for (col in missing_cols) {
      sample_data[[col]] <- NA
    }
    
    # Ensure column order matches
    user_reports <- user_reports[, names(sample_data)]
    
    # Now combine the data
    combined_data <- rbind(sample_data, user_reports)
  } else {
    combined_data <- sample_data
  }
  
  return(combined_data)
}

# Load all user reports (for admin purposes)
load_user_reports <- function() {
  # Create file if it doesn't exist
  if (!file.exists(USER_REPORTS_FILE)) {
    write_json(list(), USER_REPORTS_FILE)
  }
  
  # Load reports
  reports <- fromJSON(USER_REPORTS_FILE)
  
  return(reports)
}

# Get report by ID
get_report_by_id <- function(report_id) {
  reports <- load_user_reports()
  
  if (length(reports) == 0) {
    return(NULL)
  }
  
  for (i in seq_along(reports)) {
    if (reports[[i]]$id == report_id) {
      return(reports[[i]])
    }
  }
  
  return(NULL)
}

# Save a new user report
save_user_report <- function(report) {
  # Generate report ID if not provided
  if (is.null(report$id)) {
    report$id <- generate_uuid()
  }
  
  # Load existing reports
  reports <- load_user_reports()
  
  # Add new report
  reports[[length(reports) + 1]] <- report
  
  # Save to file
  write_json(reports, USER_REPORTS_FILE)
  
  return(report$id)
}

# Search address to coordinates
# This is a simplified version that would be replaced with proper geocoding
search_address_to_coordinates <- function(address) {
  # Simplified version - in a real app, use a geocoding service
  # Various fixed locations in Kalamazoo based on common keywords
  
  # Default to Kalamazoo center
  lat <- 42.2917
  lng <- -85.5872
  
  # Known locations
  if (grepl("downtown", tolower(address)) || grepl("mall", tolower(address))) {
    lat <- 42.2922
    lng <- -85.5872
  } else if (grepl("college", tolower(address)) || grepl("kalamazoo college", tolower(address))) {
    lat <- 42.2900
    lng <- -85.6041
  } else if (grepl("western", tolower(address)) || grepl("wmu", tolower(address)) || grepl("western michigan", tolower(address))) {
    lat <- 42.2829
    lng <- -85.6159
  } else if (grepl("milwood", tolower(address))) {
    lat <- 42.2706
    lng <- -85.5728
  } else if (grepl("vine", tolower(address))) {
    lat <- 42.2845
    lng <- -85.5941
  } else {
    # Add small random offset to simulate different locations
    lat_offset <- runif(1, -0.02, 0.02)
    lng_offset <- runif(1, -0.02, 0.02)
    
    lat <- lat + lat_offset
    lng <- lng + lng_offset
  }
  
  return(list(
    lat = lat,
    lng = lng
  ))
}

# Check for map command in forum post or comment
check_for_map_command <- function(text) {
  # Look for pattern like /map [location]
  match <- regexpr("/map\\s+([^\\n]+)", text)
  
  if (match > 0) {
    # Extract the location
    location <- regmatches(text, match)
    location <- sub("/map\\s+", "", location)
    return(location)
  }
  
  return(NULL)
}

# Initialize files on startup
initialize_files()

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Sexual Assault Statistics - Kalamazoo"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Statistics", tabName = "statistics", icon = icon("chart-bar")),
      menuItem("Report an Incident", tabName = "report", icon = icon("exclamation-triangle")),
      menuItem("Discussion Forum", tabName = "forum", icon = icon("comments"))
    ),
    
    # Login UI
    uiOutput("login_ui")
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
    ),
    
    # Top navigation
    fluidRow(
      column(
        width = 12,
        div(
          class = "top-nav",
          actionButton("home_button", "Home", class = "nav-button"),
          actionButton("statistics_button", "Statistics", class = "nav-button"),
          actionButton("report_button", "Report an Incident", class = "nav-button"),
          actionButton("forum_button", "Discussion Forum", class = "nav-button")
        )
      )
    ),
    
    # Main content area
    tabItems(
      # Home tab
      tabItem(tabName = "home",
        fluidRow(
          column(
            width = 12,
            h2("Sexual Assault Statistics Dashboard - Kalamazoo, MI"),
            p("This application provides information on sexual assault incidents in Kalamazoo, Michigan. Users can view statistics, report incidents anonymously, and participate in community discussions.")
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            # Map search bar
            div(
              class = "map-container",
              h3("Incident Map"),
              textInput("home_address_search", "Search location:", placeholder = "Enter an address in Kalamazoo"),
              actionButton("home_search_button", "Search", class = "btn-primary"),
              leafletOutput("home_map", height = "500px")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            div(
              class = "data-section",
              h3("Recent Incidents"),
              dataTableOutput("recent_incidents_table")
            )
          )
        ),
        
        fluidRow(
          column(
            width = 12,
            h3("About This Platform"),
            p("This platform aims to increase awareness about sexual assault in Kalamazoo, provide a place for anonymous reporting, and enable community discussion around this important topic.")
          )
        )
      ),
      
      # Statistics tab
      tabItem(tabName = "statistics",
        fluidRow(
          column(
            width = 12,
            h2("Statistical Analysis"),
            
            # Trend visualization
            h3("Trends Over Time"),
            plotlyOutput("trends_plot"),
            
            # Statistics tables
            h3("Incident Distribution"),
            plotlyOutput("statistics_plot")
          )
        )
      ),
      
      # Report tab
      tabItem(tabName = "report",
        fluidRow(
          column(
            width = 12,
            h2("Report an Incident"),
            p("Your report will be anonymous. Please provide as much information as you're comfortable sharing.")
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            # Report form
            div(
              class = "report-form",
              dateInput("incident_date", "Date of incident:"),
              textInput("incident_location", "Location (address or intersection):", placeholder = "Enter location in Kalamazoo"),
              actionButton("location_search_button", "Search Location", class = "btn-info"),
              selectInput("incident_type", "Type of incident:",
                          choices = c("Choose type..." = "",
                                    "Sexual assault" = "Sexual assault",
                                    "Attempted sexual assault" = "Attempted sexual assault",
                                    "Sexual harassment" = "Sexual harassment",
                                    "Other" = "Other")),
              conditionalPanel(
                condition = "input.incident_type == 'Other'",
                textInput("other_type", "Please specify:")
              ),
              textAreaInput("incident_description", "Description (optional):", 
                            rows = 5,
                            placeholder = "Any details you feel comfortable sharing..."),
              checkboxInput("include_on_map", "Include this incident on the public map", value = TRUE),
              checkboxInput("post_to_forum", "Create a discussion thread about this incident", value = TRUE),
              conditionalPanel(
                condition = "input.post_to_forum",
                textInput("thread_title", "Discussion thread title:", 
                          placeholder = "Enter a title for the discussion thread")
              ),
              actionButton("submit_report", "Submit Report", class = "btn-primary")
            )
          ),
          
          column(
            width = 6,
            # Map for selecting location
            div(
              class = "map-container",
              h4("Preview Location on Map"),
              p("Search for a location or click on the map to select a specific point"),
              leafletOutput("report_map", height = "400px"),
              br(),
              p(strong("Selected Coordinates:")),
              verbatimTextOutput("selected_coords")
            )
          )
        )
      ),
      
      # Forum tab
      tabItem(tabName = "forum",
        fluidRow(
          column(
            width = 12,
            uiOutput("forum_ui")
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    logged_in = FALSE,
    user_id = NULL,
    username = NULL,
    current_thread = NULL,
    map_data = NULL,
    selected_lat = NULL,
    selected_lng = NULL,
    thread_sort = "newest",
    comment_sort = "oldest",
    show_incident_reports = FALSE,
    last_error = NULL
  )
  
  # Load data
  observe({
    values$map_data <- load_data()
  })
  
  # Navigation actions
  observeEvent(input$home_button, {
    updateTabItems(session, "sidebar", "home")
  })
  
  observeEvent(input$statistics_button, {
    updateTabItems(session, "sidebar", "statistics")
  })
  
  observeEvent(input$report_button, {
    updateTabItems(session, "sidebar", "report")
  })
  
  observeEvent(input$forum_button, {
    updateTabItems(session, "sidebar", "forum")
  })
  
  # Login/Registration UI
  output$login_ui <- renderUI({
    if (values$logged_in) {
      # Logged in UI
      div(
        class = "sidebar-login",
        p(paste("Logged in as:", values$username)),
        actionButton("logout_button", "Log Out", class = "btn-sm btn-block")
      )
    } else {
      # Login form
      div(
        class = "sidebar-login",
        actionButton("show_login_modal", "Login", class = "btn-sm btn-primary btn-block"),
        actionButton("show_register_modal", "Register", class = "btn-sm btn-success btn-block")
      )
    }
  })
  
  # Login modal
  observeEvent(input$show_login_modal, {
    showModal(
      modalDialog(
        title = "Login",
        textInput("login_username", "Username:"),
        passwordInput("login_password", "Password:"),
        footer = tagList(
          actionButton("login_submit", "Login", class = "btn-primary"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  # Register modal
  observeEvent(input$show_register_modal, {
    showModal(
      modalDialog(
        title = "Register",
        textInput("register_username", "Username:"),
        passwordInput("register_password", "Password:"),
        passwordInput("register_password_confirm", "Confirm Password:"),
        selectInput("contact_type", "Contact Method (Optional):",
                   choices = c("None" = "none", 
                               "Email" = "email",
                               "Phone" = "phone")),
        conditionalPanel(
          condition = "input.contact_type == 'email'",
          textInput("contact_email", "Email:")
        ),
        conditionalPanel(
          condition = "input.contact_type == 'phone'",
          textInput("contact_phone", "Phone:")
        ),
        footer = tagList(
          actionButton("register_submit", "Register", class = "btn-success"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  # Login submission
  observeEvent(input$login_submit, {
    result <- authenticate_user(input$login_username, input$login_password)
    
    if (result$success) {
      values$logged_in <- TRUE
      values$user_id <- result$user_id
      values$username <- input$login_username
      removeModal()
      showNotification("Login successful", type = "message")
    } else {
      showNotification("Invalid username or password", type = "error")
    }
  })
  
  # Register submission
  observeEvent(input$register_submit, {
    # Validation
    if (input$register_password != input$register_password_confirm) {
      showNotification("Passwords do not match", type = "error")
      return()
    }
    
    # Get contact info if provided
    contact <- NULL
    contact_type <- NULL
    
    if (input$contact_type == "email") {
      contact <- input$contact_email
      contact_type <- "email"
    } else if (input$contact_type == "phone") {
      contact <- input$contact_phone
      contact_type <- "phone"
    }
    
    # Register user
    result <- register_user(
      input$register_username, 
      input$register_password,
      contact,
      contact_type
    )
    
    if (result$success) {
      values$logged_in <- TRUE
      values$user_id <- result$user_id
      values$username <- input$register_username
      removeModal()
      showNotification("Registration successful", type = "message")
    } else {
      showNotification(result$message, type = "error")
    }
  })
  
  # Logout action
  observeEvent(input$logout_button, {
    values$logged_in <- FALSE
    values$user_id <- NULL
    values$username <- NULL
    showNotification("Logged out", type = "message")
  })
  
  # Home Map display
  output$home_map <- renderLeaflet({
    req(values$map_data)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -85.5872, lat = 42.2917, zoom = 13) %>%  # Centered on Kalamazoo
      addMarkers(
        data = values$map_data,
        ~longitude, ~latitude,
        popup = ~paste("<b>Date:</b>", date, "<br>",
                      "<b>Type:</b>", type, "<br>",
                      "<b>Location:</b>", location, "<br>",
                      "<b>Description:</b>", description),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Report Map display
  output$report_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -85.5872, lat = 42.2917, zoom = 13) %>%  # Centered on Kalamazoo
      addMarkers(
        data = values$map_data,
        ~longitude, ~latitude,
        popup = ~paste("<b>Date:</b>", date, "<br>",
                      "<b>Type:</b>", type, "<br>",
                      "<b>Location:</b>", location),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Home address search
  observeEvent(input$home_search_button, {
    req(input$home_address_search)
    
    if (input$home_address_search != "") {
      coords <- search_address_to_coordinates(input$home_address_search)
      
      leafletProxy("home_map") %>%
        setView(lng = coords$lng, lat = coords$lat, zoom = 15)
    }
  })
  
  # Report location search
  observeEvent(input$location_search_button, {
    req(input$incident_location)
    
    if (input$incident_location != "") {
      coords <- search_address_to_coordinates(input$incident_location)
      
      values$selected_lat <- coords$lat
      values$selected_lng <- coords$lng
      
      leafletProxy("report_map") %>%
        clearMarkers() %>%
        setView(lng = coords$lng, lat = coords$lat, zoom = 15) %>%
        addMarkers(lng = coords$lng, lat = coords$lat, popup = input$incident_location)
    }
  })
  
  # Map click to select location
  observeEvent(input$report_map_click, {
    click <- input$report_map_click
    
    values$selected_lat <- click$lat
    values$selected_lng <- click$lng
    
    leafletProxy("report_map") %>%
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat, popup = "Selected location")
  })
  
  # Show selected coordinates
  output$selected_coords <- renderText({
    if (!is.null(values$selected_lat) && !is.null(values$selected_lng)) {
      paste0("Latitude: ", round(values$selected_lat, 6), 
             "\nLongitude: ", round(values$selected_lng, 6))
    } else {
      "No location selected"
    }
  })
  
  # Recent incidents table on home page
  output$recent_incidents_table <- renderDataTable({
    req(values$map_data)
    
    # Sort by date (newest first)
    incidents <- values$map_data %>%
      mutate(date = as.Date(date)) %>%
      arrange(desc(date)) %>%
      select(date, type, location, description)
    
    datatable(
      incidents,
      options = list(
        pageLength = 5,
        lengthMenu = c(5, 10, 15),
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Statistics outputs
  output$trends_plot <- renderPlotly({
    req(values$map_data)
    
    # Convert dates to Date objects
    df <- values$map_data %>%
      mutate(date = as.Date(date))
    
    # Count incidents by month
    monthly_counts <- df %>%
      mutate(month = format(date, "%Y-%m")) %>%
      group_by(month) %>%
      summarize(count = n()) %>%
      arrange(month)
    
    # Create plotly plot
    plot_ly(monthly_counts, x = ~month, y = ~count, type = "scatter", mode = "lines+markers") %>%
      layout(title = "Incidents by Month",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Number of Incidents"))
  })
  
  output$statistics_plot <- renderPlotly({
    req(values$map_data)
    
    # Count by incident type
    type_counts <- values$map_data %>%
      group_by(type) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    
    # Create plotly plot
    plot_ly(type_counts, x = ~type, y = ~count, type = "bar") %>%
      layout(title = "Incidents by Type",
             xaxis = list(title = "Incident Type"),
             yaxis = list(title = "Number of Incidents"))
  })
  
  # Report submission
  observeEvent(input$submit_report, {
    # Validation
    if (input$incident_type == "") {
      showNotification("Please select an incident type", type = "error")
      return()
    }
    
    if (input$post_to_forum && (is.null(input$thread_title) || input$thread_title == "")) {
      showNotification("Please provide a title for the discussion thread", type = "error")
      return()
    }
    
    # Wrap everything in a tryCatch to prevent app from crashing
    tryCatch({
      # Get coordinates (from map selection or search)
      lat <- values$selected_lat
      lng <- values$selected_lng
      
      # If no coordinates selected, get from location text
      if (is.null(lat) || is.null(lng)) {
        coords <- search_address_to_coordinates(input$incident_location)
        lat <- coords$lat
        lng <- coords$lng
      }
      
      # Prepare report data
      incident_type <- ifelse(input$incident_type == "Other", input$other_type, input$incident_type)
      
      report <- list(
        id = generate_uuid(),
        date = as.character(input$incident_date),
        type = incident_type,
        location = input$incident_location,
        description = input$incident_description,
        latitude = lat,
        longitude = lng,
        include_on_map = input$include_on_map,
        timestamp = as.character(Sys.time())
      )
      
      # Save report
      report_id <- save_user_report(report)
      
      # Create forum thread if requested
      if (input$post_to_forum) {
        # If user is not logged in, use anonymous user ID
        user_id <- ifelse(values$logged_in, values$user_id, "anonymous")
        
        # Create thread content from report info
        thread_content <- paste0(
          "**Incident Type:** ", incident_type, "\n\n",
          "**Date:** ", as.character(input$incident_date), "\n\n",
          "**Location:** ", input$incident_location, "\n\n",
          "**Description:** ", input$incident_description
        )
        
        # Create the thread
        thread_result <- create_thread(user_id, input$thread_title, thread_content, report_id)
        
        # Check if thread was created successfully
        if (!thread_result$success) {
          showNotification("Report submitted but failed to create discussion thread.", type = "warning")
        }
      }
      
      showNotification("Report submitted successfully. Thank you for sharing this information.", type = "message")
      
      # Reset form
      updateDateInput(session, "incident_date", value = Sys.Date())
      updateTextInput(session, "incident_location", value = "")
      updateSelectInput(session, "incident_type", selected = "")
      updateTextInput(session, "other_type", value = "")
      updateTextAreaInput(session, "incident_description", value = "")
      updateCheckboxInput(session, "include_on_map", value = TRUE)
      updateCheckboxInput(session, "post_to_forum", value = TRUE)
      updateTextInput(session, "thread_title", value = "")
      
      # Clear selected coordinates
      values$selected_lat <- NULL
      values$selected_lng <- NULL
      
      # Clear markers
      leafletProxy("report_map") %>%
        clearMarkers() %>%
        setView(lng = -85.5872, lat = 42.2917, zoom = 13)
      
      # Refresh data
      values$map_data <- load_data()
      
      # If posting to forum, suggest going to forum tab
      if (input$post_to_forum) {
        showModal(
          modalDialog(
            title = "Report Posted",
            "Your incident has been reported and a discussion thread has been created. Would you like to view the discussion forum?",
            footer = tagList(
              actionButton("goto_forum", "Go to Forum", class = "btn-primary"),
              modalButton("Stay on This Page")
            )
          )
        )
      }
    }, 
    error = function(e) {
      values$last_error <- e$message
      showNotification(paste("Error submitting report:", e$message), type = "error")
    })
  })
  
  # Navigate to forum after report submission
  observeEvent(input$goto_forum, {
    removeModal()
    updateTabItems(session, "sidebar", "forum")
  })
  
  # Forum UI
  output$forum_ui <- renderUI({
    if (is.null(values$current_thread)) {
      # Thread list view
      div(
        class = "forum-container",
        h2("Community Discussion Forum"),
        p("This forum is for community discussion related to sexual assault awareness and prevention in Kalamazoo."),
        
        # Large prominent Start Thread button at the top
        div(
          style = "margin-bottom: 20px;",
          if (values$logged_in) {
            actionButton("new_thread_button", "Start New Discussion", class = "btn-primary start-thread-btn")
          } else {
            div(
              actionButton("login_to_post", "Login to Start a Discussion", class = "btn-primary start-thread-btn"),
              p(style = "font-size: 0.9em; color: #666; margin-top: 5px;", "You need to be logged in to start a discussion or comment on threads.")
            )
          }
        ),
        
        # Thread filtering controls in one row
        div(
          class = "forum-controls",
          # Left side: Filters
          div(
            class = "forum-filter",
            span(class = "filter-label", "Filter:"),
            checkboxInput("show_reports_only", "Show Incident Reports Only", value = values$show_incident_reports)
          ),
          # Right side: Sort options
          div(
            selectInput("thread_sort", "Sort by:", 
                       choices = c("Newest" = "newest", "Top Voted" = "top"),
                       selected = values$thread_sort,
                       width = "150px")
          )
        ),
        
        # Horizontal divider
        tags$hr(),
        
        # Thread list
        h3("Discussions"),
        uiOutput("thread_list")
      )
    } else {
      # Thread view
      div(
        class = "forum-container",
        actionButton("back_to_threads", "← Back to Discussions", class = "btn-secondary mb-3"),
        
        # Thread content
        uiOutput("thread_content"),
        
        # Comment form (if logged in)
        if (values$logged_in) {
          div(
            class = "comment-form",
            h4("Add Comment"),
            textAreaInput("comment_text", NULL, "", rows = 3),
            actionButton("submit_comment", "Post Comment", class = "btn-primary")
          )
        } else {
          div(
            class = "comment-form",
            p("You need to be logged in to comment on discussions."),
            actionButton("login_to_comment", "Login to Comment", class = "btn-primary")
          )
        }
      )
    }
  })
  
  # Login to post button
  observeEvent(input$login_to_post, {
    showModal(
      modalDialog(
        title = "Login",
        textInput("login_username", "Username:"),
        passwordInput("login_password", "Password:"),
        footer = tagList(
          actionButton("login_submit", "Login", class = "btn-primary"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  # Login to comment button
  observeEvent(input$login_to_comment, {
    showModal(
      modalDialog(
        title = "Login",
        textInput("login_username", "Username:"),
        passwordInput("login_password", "Password:"),
        footer = tagList(
          actionButton("login_submit", "Login", class = "btn-primary"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  # Thread sorting and filtering
  observeEvent(input$thread_sort, {
    values$thread_sort <- input$thread_sort
  })
  
  observeEvent(input$show_reports_only, {
    values$show_incident_reports <- input$show_reports_only
  })
  
  # Thread list
  output$thread_list <- renderUI({
    threads <- get_threads(sort_by = values$thread_sort, filter_reports = values$show_incident_reports)
    
    if (length(threads) == 0) {
      if (values$show_incident_reports) {
        return(p("No incident reports have been posted yet."))
      } else {
        return(p("No discussions yet. Be the first to start one!"))
      }
    }
    
    thread_items <- lapply(threads, function(thread) {
      # Get author info
      author <- get_user_by_id(thread$user_id)
      author_name <- ifelse(is.null(author), "Anonymous", author$username)
      
      # Format date
      date_str <- format(as.POSIXct(thread$created_at), "%B %d, %Y %I:%M %p")
      
      # Determine if this is an incident report
      is_report <- !is.null(thread$report_id)
      
      # Get vote count
      vote_count <- ifelse(is.null(thread$votes), 0, thread$votes)
      
      # Check user's vote status
      user_vote <- NULL
      if (values$logged_in) {
        user_vote <- check_user_vote(values$user_id, thread$id)
      }
      
      # Create vote buttons
      upvote_class <- ifelse(!is.null(user_vote) && user_vote == "upvote", "upvote-btn", "")
      downvote_class <- ifelse(!is.null(user_vote) && user_vote == "downvote", "downvote-btn", "")
      
      div(
        class = "thread-item",
        div(
          class = "vote-container",
          if (values$logged_in) {
            actionButton(paste0("upvote_thread_", thread$id), "", 
                       icon = icon("chevron-up"), 
                       class = paste("btn-sm", upvote_class))
          },
          span(class = "vote-count", vote_count),
          if (values$logged_in) {
            actionButton(paste0("downvote_thread_", thread$id), "", 
                       icon = icon("chevron-down"), 
                       class = paste("btn-sm", downvote_class))
          }
        ),
        div(
          class = "thread-header",
          h4(
            thread$title,
            if (is_report) {
              span(class = "incident-tag", "Incident Report")
            }
          ),
          p(class = "thread-meta", paste("Posted by", author_name, "on", date_str))
        ),
        p(class = "thread-preview", str_trunc(thread$content, 150)),
        actionButton(paste0("view_thread_", thread$id), "View Discussion", class = "btn-sm btn-info")
      )
    })
    
    do.call(tagList, thread_items)
  })
  
  # Thread view button handlers
  observe({
    threads <- get_threads(limit = 100)  # Increased limit to ensure all buttons are caught
    
    lapply(threads, function(thread) {
      # View thread button
      button_id <- paste0("view_thread_", thread$id)
      
      observeEvent(input[[button_id]], {
        values$current_thread <- thread$id
      }, ignoreInit = TRUE)
      
      # Upvote button
      upvote_id <- paste0("upvote_thread_", thread$id)
      
      observeEvent(input[[upvote_id]], {
        req(values$logged_in)
        add_vote(values$user_id, thread$id, "upvote")
      }, ignoreInit = TRUE)
      
      # Downvote button
      downvote_id <- paste0("downvote_thread_", thread$id)
      
      observeEvent(input[[downvote_id]], {
        req(values$logged_in)
        add_vote(values$user_id, thread$id, "downvote")
      }, ignoreInit = TRUE)
    })
  })
  
  # Back to threads button
  observeEvent(input$back_to_threads, {
    values$current_thread <- NULL
  })
  
  # New thread button
  observeEvent(input$new_thread_button, {
    showModal(
      modalDialog(
        title = "Start New Discussion",
        textInput("thread_title", "Title:"),
        textAreaInput("thread_content", "Content:", rows = 5),
        footer = tagList(
          actionButton("submit_thread", "Post Discussion", class = "btn-primary"),
          modalButton("Cancel")
        )
      )
    )
  })
  
  # Submit thread
  observeEvent(input$submit_thread, {
    req(values$user_id, input$thread_title, input$thread_content)
    
    # Validation
    if (input$thread_title == "") {
      showNotification("Please provide a title", type = "error")
      return()
    }
    
    if (input$thread_content == "") {
      showNotification("Please provide content for your discussion", type = "error")
      return()
    }
    
    # Check for map command in content
    map_location <- check_for_map_command(input$thread_content)
    
    # Create thread
    result <- create_thread(values$user_id, input$thread_title, input$thread_content)
    
    if (result$success) {
      removeModal()
      showNotification("Discussion posted successfully", type = "message")
      
      # If map command was found, add incident report
      if (!is.null(map_location)) {
        coords <- search_address_to_coordinates(map_location)
        
        report <- list(
          id = generate_uuid(),
          date = as.character(Sys.Date()),
          type = "Reported via forum",
          location = map_location,
          description = paste("Reported in forum discussion:", input$thread_title),
          latitude = coords$lat,
          longitude = coords$lng,
          include_on_map = TRUE,
          timestamp = as.character(Sys.time())
        )
        
        save_user_report(report)
        values$map_data <- load_data()
        
        showNotification("Location added to map", type = "message")
      }
      
      # View the new thread
      values$current_thread <- result$thread_id
    } else {
      showNotification("Error posting discussion", type = "error")
    }
  })
  
  # Thread content
  output$thread_content <- renderUI({
    req(values$current_thread)
    
    thread <- NULL
    threads <- get_threads(limit = 100)  # Increased limit to ensure thread is found
    for (t in threads) {
      if (t$id == values$current_thread) {
        thread <- t
        break
      }
    }
    
    if (is.null(thread)) {
      return(p("Thread not found"))
    }
    
    # Get author info
    author <- get_user_by_id(thread$user_id)
    author_name <- ifelse(is.null(author), "Anonymous", author$username)
    
    # Format date
    date_str <- format(as.POSIXct(thread$created_at), "%B %d, %Y %I:%M %p")
    
    # Determine if this is an incident report
    is_report <- !is.null(thread$report_id)
    
    # Get comments
    comments <- get_thread_comments(thread$id, sort_by = values$comment_sort)
    
    # Get vote count
    vote_count <- ifelse(is.null(thread$votes), 0, thread$votes)
    
    # Check user's vote status
    user_vote <- NULL
    if (values$logged_in) {
      user_vote <- check_user_vote(values$user_id, thread$id)
    }
    
    # Create vote buttons
    upvote_class <- ifelse(!is.null(user_vote) && user_vote == "upvote", "upvote-btn", "")
    downvote_class <- ifelse(!is.null(user_vote) && user_vote == "downvote", "downvote-btn", "")
    
    div(
      class = "thread-content",
      div(
        class = "thread-header",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h2(
            thread$title,
            if (is_report) {
              span(class = "incident-tag", "Incident Report")
            }
          ),
          div(
            class = "vote-container",
            if (values$logged_in) {
              actionButton(paste0("thread_content_upvote_", thread$id), "", 
                         icon = icon("chevron-up"), 
                         class = paste("btn-sm", upvote_class))
            },
            span(class = "vote-count", vote_count),
            if (values$logged_in) {
              actionButton(paste0("thread_content_downvote_", thread$id), "", 
                         icon = icon("chevron-down"), 
                         class = paste("btn-sm", downvote_class))
            }
          )
        ),
        p(class = "thread-meta", paste("Posted by", author_name, "on", date_str))
      ),
      div(class = "thread-body", HTML(gsub("\n", "<br/>", thread$content))),
      
      # Comment sorting
      div(
        class = "forum-controls",
        h3(paste(length(comments), "Comments")),
        div(
          selectInput("comment_sort", "Sort comments by:", 
                     choices = c("Oldest" = "oldest", "Newest" = "newest", "Top Voted" = "top"),
                     selected = values$comment_sort,
                     width = "150px")
        )
      ),
      
      # Comments section
      div(
        class = "comments-section",
        
        lapply(comments, function(comment) {
          # Get comment author
          comment_author <- get_user_by_id(comment$user_id)
          comment_author_name <- ifelse(is.null(comment_author), "Anonymous", comment_author$username)
          
          # Format date
          comment_date <- format(as.POSIXct(comment$created_at), "%B %d, %Y %I:%M %p")
          
          # Get vote count
          comment_vote_count <- ifelse(is.null(comment$votes), 0, comment$votes)
          
          # Check user's vote status
          comment_user_vote <- NULL
          if (values$logged_in) {
            comment_user_vote <- check_user_vote(values$user_id, comment$id)
          }
          
          # Create vote buttons
          comment_upvote_class <- ifelse(!is.null(comment_user_vote) && comment_user_vote == "upvote", "upvote-btn", "")
          comment_downvote_class <- ifelse(!is.null(comment_user_vote) && comment_user_vote == "downvote", "downvote-btn", "")
          
          div(
            class = "comment-item",
            div(
              style = "display: flex; justify-content: space-between; align-items: center;",
              div(
                class = "comment-header",
                p(class = "comment-meta", paste(comment_author_name, "on", comment_date))
              ),
              div(
                class = "vote-container",
                if (values$logged_in) {
                  actionButton(paste0("upvote_comment_", comment$id), "", 
                             icon = icon("chevron-up"), 
                             class = paste("btn-sm", comment_upvote_class))
                },
                span(class = "vote-count", comment_vote_count),
                if (values$logged_in) {
                  actionButton(paste0("downvote_comment_", comment$id), "", 
                             icon = icon("chevron-down"), 
                             class = paste("btn-sm", comment_downvote_class))
                }
              )
            ),
            div(class = "comment-body", HTML(gsub("\n", "<br/>", comment$content)))
          )
        })
      )
    )
  })
  
  # Thread content vote handlers
  observe({
    req(values$current_thread)
    
    # Thread upvote
    observeEvent(input[[paste0("thread_content_upvote_", values$current_thread)]], {
      req(values$logged_in)
      add_vote(values$user_id, values$current_thread, "upvote")
    }, ignoreInit = TRUE)
    
    # Thread downvote
    observeEvent(input[[paste0("thread_content_downvote_", values$current_thread)]], {
      req(values$logged_in)
      add_vote(values$user_id, values$current_thread, "downvote")
    }, ignoreInit = TRUE)
  })
  
  # Comment vote handlers
  observe({
    comments <- get_thread_comments(values$current_thread)
    
    lapply(comments, function(comment) {
      # Upvote button
      upvote_id <- paste0("upvote_comment_", comment$id)
      
      observeEvent(input[[upvote_id]], {
        req(values$logged_in)
        add_vote(values$user_id, comment$id, "upvote")
      }, ignoreInit = TRUE)
      
      # Downvote button
      downvote_id <- paste0("downvote_comment_", comment$id)
      
      observeEvent(input[[downvote_id]], {
        req(values$logged_in)
        add_vote(values$user_id, comment$id, "downvote")
      }, ignoreInit = TRUE)
    })
  })
  
  # Comment sorting
  observeEvent(input$comment_sort, {
    values$comment_sort <- input$comment_sort
  })
  
  # Submit comment
  observeEvent(input$submit_comment, {
    req(values$user_id, values$current_thread, input$comment_text)
    
    # Validation
    if (input$comment_text == "") {
      showNotification("Please enter a comment", type = "error")
      return()
    }
    
    # Check for map command in content
    map_location <- check_for_map_command(input$comment_text)
    
    # Add comment
    result <- add_comment(values$user_id, values$current_thread, input$comment_text)
    
    if (result$success) {
      # Reset comment text
      updateTextAreaInput(session, "comment_text", value = "")
      
      # If map command was found, add incident report
      if (!is.null(map_location)) {
        coords <- search_address_to_coordinates(map_location)
        
        report <- list(
          id = generate_uuid(),
          date = as.character(Sys.Date()),
          type = "Reported via forum",
          location = map_location,
          description = "Reported in forum comment",
          latitude = coords$lat,
          longitude = coords$lng,
          include_on_map = TRUE,
          timestamp = as.character(Sys.time())
        )
        
        save_user_report(report)
        values$map_data <- load_data()
        
        showNotification("Location added to map", type = "message")
      }
    } else {
      showNotification("Error posting comment", type = "error")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)