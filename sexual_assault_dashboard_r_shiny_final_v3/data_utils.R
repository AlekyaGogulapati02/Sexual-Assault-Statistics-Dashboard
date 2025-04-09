library(jsonlite)
library(dplyr)

# File paths
SAMPLE_DATA_FILE <- "sample_data.csv"
USER_REPORTS_FILE <- "user_reports.json"

# Load data from both sample data and user reports
load_data <- function() {
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
  
  # Load sample data
  sample_data <- read.csv(SAMPLE_DATA_FILE, stringsAsFactors = FALSE)
  
  # Create user reports file if it doesn't exist
  if (!file.exists(USER_REPORTS_FILE)) {
    write_json(list(), USER_REPORTS_FILE)
  }
  
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
    user_reports <- do.call(rbind, lapply(user_reports, function(report) {
      # Only include reports that should be on the map
      if (!is.null(report$include_on_map) && report$include_on_map) {
        data.frame(
          date = report$date,
          type = report$type,
          location = report$location,
          description = report$description,
          latitude = as.numeric(report$latitude),
          longitude = as.numeric(report$longitude),
          stringsAsFactors = FALSE
        )
      }
    }))
    
    # If no reports to include, initialize empty data frame
    if (length(user_reports) == 0 || nrow(user_reports) == 0) {
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
  combined_data <- rbind(sample_data, user_reports)
  
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

# Save a new user report
save_user_report <- function(report) {
  # Load existing reports
  reports <- load_user_reports()
  
  # Add new report
  reports[[length(reports) + 1]] <- report
  
  # Save to file
  write_json(reports, USER_REPORTS_FILE)
  
  return(TRUE)
}

# Search address to coordinates
# This is a simplified version that would be replaced with proper geocoding
search_address_to_coordinates <- function(address) {
  # Simplified version - in a real app, use a geocoding service
  # This function returns coordinates near Kalamazoo with some randomization
  
  # Default to Kalamazoo center
  lat <- 42.2917
  lng <- -85.5872
  
  # Add small random offset to simulate different locations
  lat_offset <- runif(1, -0.02, 0.02)
  lng_offset <- runif(1, -0.02, 0.02)
  
  return(list(
    lat = lat + lat_offset,
    lng = lng + lng_offset
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