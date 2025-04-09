library(jsonlite)
library(digest)
library(uuid)

# File paths
USERS_FILE <- "users.json"
DISCUSSIONS_FILE <- "discussions.json"

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
}

# Hash password with salt
hash_password <- function(password, salt = NULL) {
  if (is.null(salt)) {
    salt <- digest(uuid::UUIDgenerate(), algo = "sha256")
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
  if (username %in% usernames) {
    return(list(
      success = FALSE,
      message = "Username already exists"
    ))
  }
  
  # Generate user ID
  user_id <- uuid::UUIDgenerate()
  
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
create_thread <- function(user_id, title, content) {
  # Read discussions
  discussions <- fromJSON(DISCUSSIONS_FILE)
  
  # Generate thread ID
  thread_id <- uuid::UUIDgenerate()
  
  # Create thread object
  thread <- list(
    id = thread_id,
    user_id = user_id,
    title = title,
    content = content,
    created_at = as.character(Sys.time())
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
  
  # Generate comment ID
  comment_id <- uuid::UUIDgenerate()
  
  # Create comment object
  comment <- list(
    id = comment_id,
    thread_id = thread_id,
    user_id = user_id,
    content = content,
    created_at = as.character(Sys.time())
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

# Get threads list
get_threads <- function(limit = 10, offset = 0) {
  # Read discussions
  discussions <- fromJSON(DISCUSSIONS_FILE)
  threads <- discussions$threads
  
  # Sort by created_at (newest first)
  if (length(threads) > 0) {
    created_times <- sapply(threads, function(t) as.POSIXct(t$created_at))
    threads <- threads[order(created_times, decreasing = TRUE)]
    
    # Apply limit and offset
    start_idx <- offset + 1
    end_idx <- min(offset + limit, length(threads))
    
    if (start_idx <= end_idx) {
      threads <- threads[start_idx:end_idx]
    } else {
      threads <- list()
    }
  }
  
  return(threads)
}

# Get comments for a thread
get_thread_comments <- function(thread_id) {
  # Read discussions
  discussions <- fromJSON(DISCUSSIONS_FILE)
  all_comments <- discussions$comments
  
  # Filter comments by thread_id
  thread_comments <- list()
  
  if (length(all_comments) > 0) {
    thread_comments <- Filter(function(c) c$thread_id == thread_id, all_comments)
    
    # Sort by created_at
    if (length(thread_comments) > 0) {
      created_times <- sapply(thread_comments, function(c) as.POSIXct(c$created_at))
      thread_comments <- thread_comments[order(created_times)]
    }
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