# Sexual Assault Statistics Dashboard for Kalamazoo - R/Shiny Version

This is an R/Shiny implementation of the Sexual Assault Statistics Dashboard for Kalamazoo. The application provides a platform for the community to anonymously report sexual assault incidents, visualize statistics, and engage in meaningful discussions around this sensitive topic.

## Features

- Interactive map for viewing reported incidents centered on Kalamazoo
- Anonymous incident reporting system with location search functionality
- Data visualization tools to identify patterns and trends
- Reddit-style discussion forum with user authentication
- Support for user registration with optional email/phone contact information
- Secure user authentication with password hashing
- Map command integration for direct incident reporting from forum posts

## Files in this Project

- `app.R`: Main Shiny application file containing both UI and server logic
- `auth_utils.R`: Helper functions for user authentication and forum management
- `data_utils.R`: Functions for data loading, processing, and reporting
- `ui_components.R`: UI helper functions and custom styling
- `sample_data.csv`: Sample incident data for demonstration
- `www/custom.css`: Custom CSS styling for the application

## Requirements

The application requires the following R packages:
- shiny
- shinydashboard
- leaflet
- dplyr
- ggplot2
- plotly
- DT (DataTables)
- digest (for password hashing)
- jsonlite
- uuid
- stringr

## Running the Application

To run the application locally:

1. Ensure R is installed on your system
2. Install the required packages:
   ```R
   install.packages(c("shiny", "shinydashboard", "leaflet", "dplyr", "ggplot2", 
                     "plotly", "DT", "digest", "jsonlite", "uuid", "stringr"))
   ```
3. Run the app:
   ```R
   shiny::runApp("path/to/app/directory")
   ```

## Data Storage

The application uses JSON files for data persistence:
- `users.json`: Stores user account information
- `discussions.json`: Stores forum threads and comments
- `user_reports.json`: Stores user-submitted incident reports

## Functionality

### User Authentication
- Users can register with a username and password
- Optional contact information (email or phone)
- Secure password storage with salted hashing

### Incident Reporting
- Anonymous reporting of sexual assault incidents
- Location-based reporting with map integration
- Customizable incident types and descriptions

### Forum Discussion
- Reddit-style discussion threads
- Comment functionality for registered users
- Special map command integration for reporting incidents directly from forum posts

### Data Visualization
- Interactive map of reported incidents
- Trend analysis over time
- Incident type distribution
- Comprehensive data tables

## Note on Geocoding

In a production environment, you would integrate with a geocoding service API to convert addresses to coordinates. This implementation uses a simplified version for demonstration purposes.

## Deployment

For production deployment, consider:
- Hosting on shinyapps.io
- Setting up an R server with Shiny Server
- Implementing proper database storage instead of JSON files
- Adding SSL/TLS for secure communication
- Implementing more robust user authentication