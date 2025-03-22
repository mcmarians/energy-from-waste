# R Shiny App: Waste to Watts - Renewable Energy & Emissions from U.S. Landfills
# Load necessary packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(plotly)
library(DT)
library(lubridate)
library(janitor)

# Load the dataset (adjust path as needed)
landfills <- read_csv("landfill_data.csv")

# Clean column names
landfills <- landfills %>% clean_names()

# Data preprocessing
landfills_clean <- landfills %>%
  mutate(
    project_start_date = ymd(project_start_date),
    project_finish_date = ymd(project_finish_date),
    year = year(project_start_date),
    waste_in_place = as.numeric(waste_in_place_tons),
    lfg_collected = as.numeric(lfg_collected_mmscfd),
    mw_generation = as.numeric(actual_mw_generation),
    emission_direct = as.numeric(current_year_emission_reductions_mmtco2e_yr_direct),
    emission_avoided = as.numeric(current_year_emission_reductions_mmtco2e_yr_avoided),
    active_days = as.numeric(difftime(project_finish_date, project_start_date, units = "days"))
  )

# Calculate percent of landfills by ownership type for use in visualizations
ownership_summary <- landfills_clean %>%
  count(ownership_type) %>%
  mutate(percentage = n / sum(n) * 100)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Waste to Watts Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Map", tabName = "map", icon = icon("globe")),
      menuItem("Energy", tabName = "energy", icon = icon("bolt")),
      menuItem("Emissions", tabName = "emissions", icon = icon("leaf")),
      
      # Group State visualizations
      menuItem("State Analysis", tabName = "state_analysis", icon = icon("map-marker"),
               menuSubItem("#1 Landfills by State", tabName = "landfills_by_state"),
               menuSubItem("#2 Top 10 Landfill States", tabName = "top10_states_landfills"),
               menuSubItem("#3 Top 10 States by Waste", tabName = "top10_states_waste"),
               menuSubItem("#4 Most Wasteful per State", tabName = "most_waste_per_state"),
               menuSubItem("#5 Avg LFG by State", tabName = "avg_lfg_by_state"),
               menuSubItem("#6 State Landfills by Ownership", tabName = "landfills_by_ownership")
      ),
      
      # Group city and county analysis
      menuItem("City & County Analysis", tabName = "local_analysis", icon = icon("city"),
               menuSubItem("Top 10 Cities by Landfills", tabName = "top10_cities_landfills"),
               menuSubItem("Top 10 Counties by Landfills", tabName = "top10_counties_landfills"),
               menuSubItem("Top 10 Cities by Waste", tabName = "top10_cities_waste"),
               menuSubItem("Top 10 Counties by Waste", tabName = "top10_counties_waste")
      ),
      
      # Group Ownership type analysis
      menuItem("Ownership Analysis", tabName = "ownership_analysis", icon = icon("building"),
               menuSubItem("Landfills by Ownership", tabName = "landfills_by_ownership_pie"),
               menuSubItem("Avg Waste by Ownership", tabName = "avg_waste_by_ownership"),
               menuSubItem("Top 10 States by Landfill Ownership", tabName = "top10_states_by_ownership"),
               menuSubItem("Top 10 Cities by Landfill Ownership", tabName = "top10_cities_by_ownership"),
               menuSubItem("Top 10 Counties by Landfill Ownership", tabName = "top10_counties_by_ownership"),
               menuSubItem("Avg LFG by Ownership", tabName = "avg_lfg_by_ownership")
      ),
      
      # Additional insights
      menuItem("Additional Insights", tabName = "additional_insights", icon = icon("chart-line"),
               menuSubItem("Top 10 Active Landfills", tabName = "top10_active_landfills")
      )
    )
  ),
  # Added CSS to increase the width of the sidebar menu item as some names were going outside the sidebar boundary
  dashboardBody(
    tags$head(
      tags$style(HTML("
    .main-sidebar {
      width: 300px !important;
    }
    .main-header .logo {
      width: 300px !important;
      white-space: normal;
    }
    .main-header .navbar {
      margin-left: 300px !important;
    }
    .content-wrapper, .main-footer {
      margin-left: 300px !important;
    }
  "))
    ),
    tabItems(
      # Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_landfills"),
                valueBoxOutput("total_mw"),
                valueBoxOutput("total_emissions")
              ),
              DTOutput("landfill_table")
      ),
      
      # Map tab
      tabItem(tabName = "map",
              h4("In the map below, we not only unveil the extensive distribution of waste disposal sites but also expose critical information such as whether these sites are private or public. This provides us with more than just a surface view, offering a comprehensive insight into the operation of these areas."),
              leafletOutput("landfill_map", height = 600)
      ),
      
      # Energy tab
      tabItem(tabName = "energy",
              plotlyOutput("energy_plot", height = 500)
      ),
      
      # Emissions tab
      tabItem(tabName = "emissions",
              plotlyOutput("emission_plot", height = 500)
      ),
      
      # By State visualizations
      tabItem(tabName = "landfills_by_state",
              plotlyOutput("landfills_by_state", height = 600),
              h4("In the graph above, we can observe the distribution of landfill sites across states. The state with the most landfill sites is California, known for its diversity and size, while the state with the fewest landfill sites is Arizona, recognized for its hot and arid climate.")
      ),
      
      tabItem(tabName = "top10_states_landfills",
              h4("The graph above strikingly showcases the top 10 states with the most landfill sites, serving as an indicator of waste management and environmental consciousness."),
              plotlyOutput("top10_states_landfills", height = 600)
      ),
      
      tabItem(tabName = "landfills_by_ownership",
              plotlyOutput("landfills_by_state_ownership", height = 600),
              h4("This visualization shows the distribution of landfills by ownership type across different states, providing insight into the private versus public management of waste.")
      ),
      
      tabItem(tabName = "top10_states_waste",
              plotlyOutput("top10_states_waste", height = 600),
              h4("This chart highlights the states managing the largest volumes of waste, indicating where waste management infrastructure is most heavily utilized.")
      ),
      
      tabItem(tabName = "most_waste_per_state",
              plotlyOutput("most_waste_per_state", height = 600),
              h4("This visualization identifies the largest landfill in each state by waste volume, showing which facilities bear the greatest waste management burden.")
      ),
      
      tabItem(tabName = "avg_lfg_by_state",
              plotlyOutput("avg_lfg_by_state", height = 600),
              h4("The average landfill gas (LFG) collected by state provides insight into which regions are most effectively capturing this potential renewable energy source.")
      ),
      
      # City and County visualizations
      tabItem(tabName = "top10_cities_landfills",
              plotlyOutput("top10_cities_landfills", height = 600),
              h4("This visualization shows the cities with the highest concentration of landfill facilities, highlighting urban areas with significant waste management infrastructure.")
      ),
      
      tabItem(tabName = "top10_counties_landfills",
              plotlyOutput("top10_counties_landfills", height = 600),
              h4("Counties with the most landfills often indicate regions with higher population density or industrial activity requiring extensive waste management.")
      ),
      
      tabItem(tabName = "top10_cities_waste",
              plotlyOutput("top10_cities_waste", height = 600),
              h4("These cities manage the largest volumes of waste, indicating areas with significant waste generation or that serve as regional waste management hubs.")
      ),
      
      tabItem(tabName = "top10_counties_waste",
              plotlyOutput("top10_counties_waste", height = 600),
              h4("Counties handling the most waste often represent major regional waste management centers that service broader areas beyond their boundaries.")
      ),
      
      # Ownership analysis tabs
      tabItem(tabName = "landfills_by_ownership_pie",
              plotlyOutput("landfills_by_ownership_pie", height = 500),
              h4("The distribution of landfill ownership types provides insight into the public versus private management of waste infrastructure in the United States.")
      ),
      
      tabItem(tabName = "avg_waste_by_ownership",
              plotlyOutput("avg_waste_by_ownership", height = 500),
              h4("This visualization compares the average waste volume across different ownership types, showing which management models handle larger waste volumes on average.")
      ),
      
      tabItem(tabName = "top10_states_by_ownership",
              plotlyOutput("top10_states_by_ownership", height = 600),
              h4("This detailed breakdown shows how ownership types vary across the states with the most landfills, revealing regional patterns in waste management approaches.")
      ),
      
      tabItem(tabName = "top10_cities_by_ownership",
              plotlyOutput("top10_cities_by_ownership", height = 600),
              h4("Urban waste management strategies become evident when examining ownership patterns in cities with high concentrations of landfills.")
      ),
      
      tabItem(tabName = "top10_counties_by_ownership",
              plotlyOutput("top10_counties_by_ownership", height = 600),
              h4("County-level ownership patterns reveal regional waste management strategies and the relative roles of public and private entities.")
      ),
      
      tabItem(tabName = "avg_lfg_by_ownership",
              plotlyOutput("avg_lfg_by_ownership", height = 500),
              h4("This visualization shows which ownership types are most effective at capturing landfill gas, an important renewable energy source.")
      ),
      
      # Additional insights
      tabItem(tabName = "top10_active_landfills",
              plotlyOutput("top10_active_landfills", height = 600),
              h4("Landfills with the most active days represent facilities with long operational histories or high operational stability.")
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Value boxes for overview
  output$total_landfills <- renderValueBox({
    valueBox(
      value = n_distinct(landfills_clean$landfill_id),
      subtitle = "Total Landfills",
      icon = icon("trash"),
      color = "blue"
    )
  })
  
  output$total_mw <- renderValueBox({
    valueBox(
      value = round(sum(landfills_clean$mw_generation, na.rm = TRUE), 1),
      subtitle = "Total MW Generated",
      icon = icon("bolt"),
      color = "green"
    )
  })
  
  output$total_emissions <- renderValueBox({
    valueBox(
      value = round(sum(landfills_clean$emission_direct + landfills_clean$emission_avoided, na.rm = TRUE), 2),
      subtitle = "Total Emissions Reduced (MMTCO2e/year)",
      icon = icon("leaf"),
      color = "purple"
    )
  })
  
  # Map visualization
  output$landfill_map <- renderLeaflet({
    leaflet(data = landfills_clean) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~log10(waste_in_place + 1) * 2,
        color = ~case_when(
          ownership_type == "Public" ~ "green",
          ownership_type == "Private" ~ "red",
          TRUE ~ "orange"
        ),
        popup = ~paste0("<strong>", landfill_name, "</strong><br>Waste: ", waste_in_place, " tons<br>Status: ", ownership_type)
      )
  })
  
  # Energy plot
  output$energy_plot <- renderPlotly({
    df_energy <- landfills_clean %>%
      filter(!is.na(year), !is.na(lfg_collected), !is.na(mw_generation)) %>%
      group_by(year, project_type_category) %>%
      summarise(
        lfg_collected = sum(lfg_collected, na.rm = TRUE),
        mw_generated = sum(mw_generation, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(lfg_collected, mw_generated), names_to = "metric", values_to = "value")
    
    p <- ggplot(df_energy, aes(x = year, y = value, fill = metric)) +
      geom_col(position = "dodge") +
      facet_wrap(~project_type_category) +
      labs(title = "LFG Collected vs MW Generated by Year and Project Type", x = "Year", y = "Value") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Emissions plot
  output$emission_plot <- renderPlotly({
    df_emission <- landfills_clean %>%
      filter(!is.na(year)) %>%
      group_by(year) %>%
      summarise(
        direct = sum(emission_direct, na.rm = TRUE),
        avoided = sum(emission_avoided, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(direct, avoided), names_to = "type", values_to = "value")
    
    p <- ggplot(df_emission, aes(x = year, y = value, color = type)) +
      geom_line(size = 1.2) +
      labs(title = "Emission Reductions Over Time", x = "Year", y = "MMTCO2e Reduced") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Landfill table
  output$landfill_table <- renderDT({
    datatable(
      landfills_clean %>%
        select(
          landfill_name, state, city, county,
          project_type_category, current_project_status,
          waste_in_place, ownership_type,
          mw_generation, emission_direct, emission_avoided
        ),
      options = list(
        pageLength = 10,
        scrollX = TRUE  # <-- THIS enables horizontal scrolling
      ),
      class = 'stripe hover nowrap'
    )
  })
  
  
  # VISUALIZATIONS
  
  # Landfills by state
  output$landfills_by_state <- renderPlotly({
    df <- landfills_clean %>%
      count(state, name = "num_landfills")
    ggplotly(
      ggplot(df, aes(x = reorder(state, -num_landfills), y = num_landfills)) +
        geom_col(fill = "steelblue") +
        labs(title = "Number of Landfills by State", x = "State", y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  # Top 10 states by landfills
  output$top10_states_landfills <- renderPlotly({
    df <- landfills_clean %>%
      count(state, name = "num_landfills") %>%
      top_n(10, num_landfills)
    ggplotly(
      ggplot(df, aes(x = reorder(state, num_landfills), y = num_landfills)) +
        geom_col(fill = "darkgreen") +
        labs(title = "Top 10 States by Number of Landfills", x = "State", y = "Landfills") +
        coord_flip() +
        theme_minimal()
    )
  })
  
  # Landfills by state and ownership type
  output$landfills_by_state_ownership <- renderPlotly({
    df <- landfills_clean %>%
      count(state, ownership_type)
    ggplotly(
      ggplot(df, aes(x = state, y = n, fill = ownership_type)) +
        geom_col(position = "dodge") +
        labs(title = "Landfills by State and Ownership Type", x = "State", y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  # Top 10 states by waste
  output$top10_states_waste <- renderPlotly({
    df <- landfills_clean %>%
      group_by(state) %>%
      summarise(total_waste = sum(waste_in_place, na.rm = TRUE)) %>%
      top_n(10, total_waste)
    ggplotly(
      ggplot(df, aes(x = reorder(state, total_waste), y = total_waste)) +
        geom_col(fill = "orange") +
        labs(title = "Top 10 States with the Most Waste", x = "State", y = "Waste in Place (tons)") +
        coord_flip() +
        theme_minimal()
    )
  })
  
  # Most waste per state
  output$most_waste_per_state <- renderPlotly({
    df <- landfills_clean %>%
      group_by(state) %>%
      slice_max(order_by = waste_in_place, n = 1, with_ties = FALSE)
    ggplotly(
      ggplot(df, aes(x = reorder(state, waste_in_place), y = waste_in_place, fill = landfill_name)) +
        geom_col() +
        labs(title = "Landfills with the Most Waste in Each State", x = "State", y = "Waste in Place (tons)") +
        coord_flip() +
        theme_minimal()
    )
  })
  
  # Average LFG by state
  output$avg_lfg_by_state <- renderPlotly({
    df <- landfills_clean %>%
      group_by(state) %>%
      summarise(avg_lfg = mean(lfg_collected, na.rm = TRUE)) %>%
      filter(!is.na(avg_lfg))
    ggplotly(
      ggplot(df, aes(x = reorder(state, avg_lfg), y = avg_lfg)) +
        geom_col(fill = "purple") +
        labs(title = "Average LFG Collected by State", x = "State", y = "Average LFG (mmscfd)") +
        coord_flip() +
        theme_minimal()
    )
  })
  
  # NEW VISUALIZATIONS - CITY AND COUNTY ANALYSIS
  
  # Top 10 cities by number of landfills
  output$top10_cities_landfills <- renderPlotly({
    df <- landfills_clean %>%
      filter(!is.na(city)) %>%
      count(city, state, name = "num_landfills") %>%
      mutate(location = paste(city, state, sep = ", ")) %>%
      top_n(10, num_landfills)
    
    ggplotly(
      ggplot(df, aes(x = reorder(location, num_landfills), y = num_landfills)) +
        geom_col(fill = "skyblue") +
        labs(title = "Top 10 Cities by Number of Landfills", x = "City", y = "Count") +
        coord_flip() +
        theme_minimal()
    )
  })
  
  # Top 10 counties by number of landfills
  output$top10_counties_landfills <- renderPlotly({
    df <- landfills_clean %>%
      filter(!is.na(county)) %>%
      count(county, state, name = "num_landfills") %>%
      mutate(location = paste(county, state, sep = ", ")) %>%
      top_n(10, num_landfills)
    
    ggplotly(
      ggplot(df, aes(x = reorder(location, num_landfills), y = num_landfills)) +
        geom_col(fill = "lightgreen") +
        labs(title = "Top 10 Counties by Number of Landfills", x = "County", y = "Count") +
        coord_flip() +
        theme_minimal()
    )
  })
  
  # Top 10 cities by waste
  output$top10_cities_waste <- renderPlotly({
    df <- landfills_clean %>%
      filter(!is.na(city)) %>%
      group_by(city, state) %>%
      summarise(total_waste = sum(waste_in_place, na.rm = TRUE), .groups = "drop") %>%
      mutate(location = paste(city, state, sep = ", ")) %>%
      top_n(10, total_waste)
    
    ggplotly(
      ggplot(df, aes(x = reorder(location, total_waste), y = total_waste)) +
        geom_col(fill = "tomato") +
        labs(title = "Top 10 Cities with the Most Waste", x = "City", y = "Waste in Place (tons)") +
        coord_flip() +
        theme_minimal()
    )
  })
  
  # Top 10 counties by waste
  output$top10_counties_waste <- renderPlotly({
    df <- landfills_clean %>%
      filter(!is.na(county)) %>%
      group_by(county, state) %>%
      summarise(total_waste = sum(waste_in_place, na.rm = TRUE), .groups = "drop") %>%
      mutate(location = paste(county, state, sep = ", ")) %>%
      top_n(10, total_waste)
    
    ggplotly(
      ggplot(df, aes(x = reorder(location, total_waste), y = total_waste)) +
        geom_col(fill = "goldenrod") +
        labs(title = "Top 10 Counties with the Most Waste", x = "County", y = "Waste in Place (tons)") +
        coord_flip() +
        theme_minimal()
    )
  })
  
  # OWNERSHIP ANALYSIS
  
  # Landfills by ownership type (pie chart)
  output$landfills_by_ownership_pie <- renderPlotly({
    plot_ly(ownership_summary, labels = ~ownership_type, values = ~n, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = c("forestgreen", "firebrick", "goldenrod"))) %>%
      layout(title = "Number of Landfills & Percentage by Ownership Type")
  })
  
  # Average waste by ownership type
  output$avg_waste_by_ownership <- renderPlotly({
    df <- landfills_clean %>%
      group_by(ownership_type) %>%
      summarise(avg_waste = mean(waste_in_place, na.rm = TRUE))
    
    ggplotly(
      ggplot(df, aes(x = reorder(ownership_type, avg_waste), y = avg_waste)) +
        geom_col(fill = "coral") +
        labs(title = "Average Waste in Place by Ownership Type", x = "Ownership Type", y = "Average Waste (tons)") +
        theme_minimal()
    )
  })
  
  # Top 10 states by landfill ownership types
  output$top10_states_by_ownership <- renderPlotly({
    # Get top 10 states by total number of landfills
    top_states <- landfills_clean %>%
      count(state) %>%
      top_n(10, n) %>%
      pull(state)
    
    # Filter for just those states and count by ownership
    df <- landfills_clean %>%
      filter(state %in% top_states) %>%
      count(state, ownership_type)
    
    ggplotly(
      ggplot(df, aes(x = state, y = n, fill = ownership_type)) +
        geom_col(position = "stack") +
        labs(title = "Top 10 States by Number of Landfills (By Ownership Type)", 
             x = "State", y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  # Top 10 cities by landfill ownership types
  output$top10_cities_by_ownership <- renderPlotly({
    # Get top 10 cities by total number of landfills
    top_cities <- landfills_clean %>%
      filter(!is.na(city)) %>%
      count(city, state) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      mutate(location = paste(city, state, sep = ", "))
    
    # Filter for just those cities and count by ownership
    df <- landfills_clean %>%
      filter(!is.na(city)) %>%
      mutate(location = paste(city, state, sep = ", ")) %>%
      filter(location %in% top_cities$location) %>%
      count(location, ownership_type)
    
    ggplotly(
      ggplot(df, aes(x = location, y = n, fill = ownership_type)) +
        geom_col(position = "stack") +
        labs(title = "Top 10 Cities by Number of Landfills (By Ownership Type)", 
             x = "City", y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  # Top 10 counties by landfill ownership types
  output$top10_counties_by_ownership <- renderPlotly({
    # Get top 10 counties by total number of landfills
    top_counties <- landfills_clean %>%
      filter(!is.na(county)) %>%
      count(county, state) %>%
      arrange(desc(n)) %>%
      head(10) %>%
      mutate(location = paste(county, state, sep = ", "))
    
    # Filter for just those counties and count by ownership
    df <- landfills_clean %>%
      filter(!is.na(county)) %>%
      mutate(location = paste(county, state, sep = ", ")) %>%
      filter(location %in% top_counties$location) %>%
      count(location, ownership_type)
    
    ggplotly(
      ggplot(df, aes(x = location, y = n, fill = ownership_type)) +
        geom_col(position = "stack") +
        labs(title = "Top 10 Counties by Number of Landfills (By Ownership Type)", 
             x = "County", y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  # Average LFG collected by ownership type
  output$avg_lfg_by_ownership <- renderPlotly({
    df <- landfills_clean %>%
      group_by(ownership_type) %>%
      summarise(avg_lfg = mean(lfg_collected, na.rm = TRUE)) %>%
      filter(!is.na(avg_lfg))
    
    ggplotly(
      ggplot(df, aes(x = reorder(ownership_type, avg_lfg), y = avg_lfg)) +
        geom_col(fill = "slateblue") +
        labs(title = "Average LFG Collected by Ownership Type", 
             x = "Ownership Type", y = "Average LFG (mmscfd)") +
        theme_minimal()
    )
  })
  
  # ADDITIONAL INSIGHTS
  
  # Top 10 landfills with the most active days
  output$top10_active_landfills <- renderPlotly({
    df <- landfills_clean %>%
      filter(!is.na(active_days), active_days > 0) %>%
      arrange(desc(active_days)) %>%
      head(10) %>%
      mutate(label = paste(landfill_name, state, sep = ", "))
    
    ggplotly(
      ggplot(df, aes(x = reorder(label, active_days), y = active_days)) +
        geom_col(fill = "mediumseagreen") +
        labs(title = "Top 10 Landfills with the Most Active Days", 
             x = "Landfill", y = "Active Days") +
        coord_flip() +
        theme_minimal()
    )
  })
}

# Run the app
shinyApp(ui, server)

