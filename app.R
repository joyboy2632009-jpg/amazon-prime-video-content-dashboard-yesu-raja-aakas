# ================================================================
# AMAZON PRIME VIDEO CONTENT DASHBOARD
# Interactive | Attractive | Professional
# ================================================================

# -------------------- 1. INSTALL PACKAGES -----------------------

packages <- c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "plotly",
  "DT",
  "lubridate",
  "scales"
)

new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(new_packages) > 0){
  install.packages(new_packages, dependencies = TRUE)
}

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)
library(scales)

# -------------------- 2. LOAD DATA ------------------------------

# Select your CSV file
file_path <- file.choose("C:\\Users\\yesur\\Downloads\\archive (2)")

amazon <- read.csv(
  file_path,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA", "N/A", "null")
)

# -------------------- 3. DATA CLEANING --------------------------

amazon <- amazon %>%
  mutate(
    type = ifelse(is.na(type) | type == "", "Unknown", type),
    title = ifelse(is.na(title) | title == "", "Unknown", title),
    country = ifelse(is.na(country) | country == "", "Unknown", country),
    rating = ifelse(is.na(rating) | rating == "", "Not Rated", rating),
    listed_in = ifelse(is.na(listed_in) | listed_in == "", "Unknown", listed_in),
    release_year = as.numeric(release_year)
  )

# Date conversion
amazon$date_added <- suppressWarnings(
  as.Date(amazon$date_added, format = "%B %d, %Y")
)

# -------------------- 4. CREATE GENRE COLUMN --------------------

amazon$genre <- sapply(
  strsplit(amazon$listed_in, ","),
  function(x) trimws(x[1])
)

# -------------------- 5. UI -------------------------------------

ui <- dashboardPage(
  
  # --------------------------------------------------------------
  # HEADER
  # --------------------------------------------------------------
  
  dashboardHeader(
    title = "AMAZON PRIME VIDEO"
  ),
  
  # --------------------------------------------------------------
  # SIDEBAR
  # --------------------------------------------------------------
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Content Explorer",
        tabName = "explorer",
        icon = icon("film")
      ),
      
      menuItem(
        "Data Table",
        tabName = "data",
        icon = icon("table")
      )
      
    ),
    
    br(),
    
    h4(
      "FILTERS",
      style = "color:white; padding-left:15px;"
    ),
    
    # Content Type
    selectInput(
      "type",
      "Content Type:",
      choices = c(
        "All",
        sort(unique(amazon$type))
      ),
      selected = "All"
    ),
    
    # Rating
    selectInput(
      "rating",
      "Rating:",
      choices = c(
        "All",
        sort(unique(amazon$rating))
      ),
      selected = "All"
    ),
    
    # Country
    selectInput(
      "country",
      "Country:",
      choices = c(
        "All",
        sort(unique(amazon$country))
      ),
      selected = "All"
    ),
    
    # Year
    sliderInput(
      "year",
      "Release Year:",
      min = min(amazon$release_year, na.rm = TRUE),
      max = max(amazon$release_year, na.rm = TRUE),
      value = c(
        min(amazon$release_year, na.rm = TRUE),
        max(amazon$release_year, na.rm = TRUE)
      ),
      step = 1,
      sep = ""
    )
    
  ),
  
  # --------------------------------------------------------------
  # BODY
  # --------------------------------------------------------------
  
  dashboardBody(
    
    tags$head(
      
      tags$style(HTML("

        /* Main background */
        .content-wrapper,
        .right-side {
          background-color: #f4f6f9;
        }

        /* Dashboard title */
        .skin-blue .main-header .logo {
          background-color: #111827;
          font-weight: bold;
        }

        .skin-blue .main-header .navbar {
          background-color: #111827;
        }

        /* Sidebar */
        .skin-blue .main-sidebar {
          background-color: #111827;
        }

        /* Box styling */
        .box {
          border-radius: 12px;
          box-shadow: 0 3px 12px rgba(0,0,0,0.08);
          border-top: none;
        }

        /* Value boxes */
        .small-box {
          border-radius: 12px;
          box-shadow: 0 3px 12px rgba(0,0,0,0.10);
        }

        /* Headers */
        h2, h3, h4 {
          font-weight: 600;
        }

        /* Data table */
        .dataTables_wrapper {
          padding: 10px;
        }

      "))
      
    ),
    
    tabItems(
      
      # ==========================================================
      # DASHBOARD TAB
      # ==========================================================
      
      tabItem(
        tabName = "dashboard",
        
        fluidRow(
          
          # KPI 1
          valueBoxOutput(
            "total_titles",
            width = 3
          ),
          
          # KPI 2
          valueBoxOutput(
            "total_movies",
            width = 3
          ),
          
          # KPI 3
          valueBoxOutput(
            "total_tv",
            width = 3
          ),
          
          # KPI 4
          valueBoxOutput(
            "countries",
            width = 3
          )
          
        ),
        
        fluidRow(
          
          # Content Type Chart
          box(
            title = "Movies vs TV Shows",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput(
              "type_chart",
              height = "350px"
            )
          ),
          
          # Genre Chart
          box(
            title = "Top Genres",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput(
              "genre_chart",
              height = "350px"
            )
          )
          
        ),
        
        fluidRow(
          
          # Release Year Trend
          box(
            title = "Content by Release Year",
            status = "primary",
            solidHeader = TRUE,
            width = 8,
            plotlyOutput(
              "year_chart",
              height = "380px"
            )
          ),
          
          # Ratings
          box(
            title = "Ratings Distribution",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            plotlyOutput(
              "rating_chart",
              height = "380px"
            )
          )
          
        ),
        
        fluidRow(
          
          # Countries
          box(
            title = "Top 10 Countries",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput(
              "country_chart",
              height = "400px"
            )
          )
          
        )
        
      ),
      
      # ==========================================================
      # CONTENT EXPLORER
      # ==========================================================
      
      tabItem(
        tabName = "explorer",
        
        fluidRow(
          
          box(
            title = "Amazon Prime Content Explorer",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput(
              "explorer_table"
            )
            
          )
          
        )
        
      ),
      
      # ==========================================================
      # DATA TABLE
      # ==========================================================
      
      tabItem(
        tabName = "data",
        
        fluidRow(
          
          box(
            title = "Complete Amazon Prime Dataset",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput(
              "full_table"
            )
            
          )
          
        )
        
      )
      
    )
    
  )
)

# ================================================================
# SERVER
# ================================================================

server <- function(input, output, session) {
  
  # --------------------------------------------------------------
  # FILTER DATA
  # --------------------------------------------------------------
  
  filtered_data <- reactive({
    
    data <- amazon
    
    # Type filter
    if(input$type != "All"){
      
      data <- data %>%
        filter(type == input$type)
      
    }
    
    # Rating filter
    if(input$rating != "All"){
      
      data <- data %>%
        filter(rating == input$rating)
      
    }
    
    # Country filter
    if(input$country != "All"){
      
      data <- data %>%
        filter(country == input$country)
      
    }
    
    # Year filter
    data <- data %>%
      filter(
        release_year >= input$year[1],
        release_year <= input$year[2]
      )
    
    data
    
  })
  
  # ==============================================================
  # KPI CARDS
  # ==============================================================
  
  output$total_titles <- renderValueBox({
    
    valueBox(
      value = comma(nrow(filtered_data())),
      subtitle = "Total Titles",
      icon = icon("film"),
      color = "blue"
    )
    
  })
  
  output$total_movies <- renderValueBox({
    
    movies <- sum(
      filtered_data()$type == "Movie",
      na.rm = TRUE
    )
    
    valueBox(
      value = comma(movies),
      subtitle = "Movies",
      icon = icon("video"),
      color = "purple"
    )
    
  })
  
  output$total_tv <- renderValueBox({
    
    tv <- sum(
      filtered_data()$type == "TV Show",
      na.rm = TRUE
    )
    
    valueBox(
      value = comma(tv),
      subtitle = "TV Shows",
      icon = icon("tv"),
      color = "green"
    )
    
  })
  
  output$countries <- renderValueBox({
    
    country_count <- n_distinct(
      filtered_data()$country[
        filtered_data()$country != "Unknown"
      ]
    )
    
    valueBox(
      value = comma(country_count),
      subtitle = "Countries",
      icon = icon("globe"),
      color = "yellow"
    )
    
  })
  
  # ==============================================================
  # MOVIES VS TV SHOWS
  # ==============================================================
  
  output$type_chart <- renderPlotly({
    
    data <- filtered_data() %>%
      
      count(type) %>%
      
      arrange(desc(n))
    
    plot_ly(
      data,
      labels = ~type,
      values = ~n,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent",
      hovertemplate =
        "<b>%{label}</b><br>" ~
        "Titles: %{value}<br>" ~
        "Share: %{percent}<extra></extra>"
    ) %>%
      
      layout(
        showlegend = TRUE,
        margin = list(l = 20, r = 20, t = 20, b = 20)
      )
    
  })
  
  # ==============================================================
  # TOP GENRES
  # ==============================================================
  
  output$genre_chart <- renderPlotly({
    
    data <- filtered_data() %>%
      
      count(genre, sort = TRUE) %>%
      
      slice_head(n = 10) %>%
      
      arrange(n)
    
    plot_ly(
      data,
      x = ~n,
      y = ~reorder(genre, n),
      type = "bar",
      orientation = "h",
      text = ~comma(n),
      textposition = "outside",
      hovertemplate =
        "<b>%{y}</b><br>" ~
        "Titles: %{x}<extra></extra>"
    ) %>%
      
      layout(
        xaxis = list(
          title = "Number of Titles"
        ),
        yaxis = list(
          title = ""
        ),
        margin = list(l = 130, r = 50)
      )
    
  })
  
  # ==============================================================
  # RELEASE YEAR TREND
  # ==============================================================
  
  output$year_chart <- renderPlotly({
    
    data <- filtered_data() %>%
      
      filter(!is.na(release_year)) %>%
      
      count(release_year)
    
    plot_ly(
      data,
      x = ~release_year,
      y = ~n,
      type = "scatter",
      mode = "lines+markers",
      hovertemplate =
        "<b>Year: %{x}</b><br>" ~
        "Titles: %{y}<extra></extra>"
    ) %>%
      
      layout(
        xaxis = list(
          title = "Release Year"
        ),
        yaxis = list(
          title = "Number of Titles"
        ),
        hovermode = "x unified"
      )
    
  })
  
  # ==============================================================
  # RATINGS
  # ==============================================================
  
  output$rating_chart <- renderPlotly({
    
    data <- filtered_data() %>%
      
      count(rating, sort = TRUE) %>%
      
      slice_head(n = 10)
    
    plot_ly(
      data,
      labels = ~rating,
      values = ~n,
      type = "pie",
      hole = 0.45,
      textinfo = "label+percent",
      hovertemplate =
        "<b>%{label}</b><br>" ~
        "Titles: %{value}<extra></extra>"
    )
    
  })
  
  # ==============================================================
  # TOP COUNTRIES
  # ==============================================================
  
  output$country_chart <- renderPlotly({
    
    data <- filtered_data() %>%
      
      separate_rows(country, sep = ",") %>%
      
      mutate(
        country = trimws(country)
      ) %>%
      
      filter(
        country != "Unknown",
        country != ""
      ) %>%
      
      count(country, sort = TRUE) %>%
      
      slice_head(n = 10) %>%
      
      arrange(n)
    
    plot_ly(
      data,
      x = ~n,
      y = ~reorder(country, n),
      type = "bar",
      orientation = "h",
      text = ~comma(n),
      textposition = "outside",
      hovertemplate =
        "<b>%{y}</b><br>" ~
        "Titles: %{x}<extra></extra>"
    ) %>%
      
      layout(
        xaxis = list(
          title = "Number of Titles"
        ),
        yaxis = list(
          title = ""
        ),
        margin = list(l = 120, r = 50)
      )
    
  })
  
  # ==============================================================
  # CONTENT EXPLORER TABLE
  # ==============================================================
  
  output$explorer_table <- renderDT({
    
    filtered_data() %>%
      
      select(
        title,
        type,
        release_year,
        rating,
        country,
        genre,
        duration
      ) %>%
      
      arrange(desc(release_year)) %>%
      
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        filter = "top",
        rownames = FALSE
      )
    
  })
  
  # ==============================================================
  # FULL DATA TABLE
  # ==============================================================
  
  output$full_table <- renderDT({
    
    datatable(
      amazon,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      filter = "top",
      rownames = FALSE
    )
    
  })
  
}

# ================================================================
# RUN DASHBOARD
# ================================================================

shinyApp(
  ui = ui,
  server = server
)