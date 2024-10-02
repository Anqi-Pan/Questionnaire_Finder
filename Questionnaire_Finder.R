library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyBS)
library(bsplus)
library(readxl)
library(leaflet)
library(sp)
library(sf)
library(rworldmap)
library(DT)
library(dplyr)
library(stringr)
library(plotly)
library(rvest)
library(ggplot2)
library(reshape2)
library(sf)
library(tidyr)
library(rsconnect)
library(fastmap)
library(rmapshaper)



# Load files
# MDL
# CORPUS_Questions
# reference_table # Country coord
# UNHCR_bounds 





# Pre-processing
CORPUS_Questions <- CORPUS_Questions %>%
  mutate(
    IDP_Count = sapply(str_split(IDP_Questions, ","), function(x) length(x[!is.na(x) & x != ""])),
    Refugee_Count = sapply(str_split(Refugee_Questions, ","), function(x) length(x[!is.na(x) & x != ""]))
  )


unique_dictionary_p <- sort(unique(unlist(strsplit(as.character(MDL$Population), ", "))))
unique_countries <- sort(unique(unlist(strsplit(as.character(MDL$Country), ", "))))
unique_topics <- sort(unique(unlist(strsplit(as.character(MDL$Topics), ", "))))
unique_regions <- sort(unique(unlist(strsplit(as.character(MDL$Region), ", "))))
unique_collections <- sort(unique(unlist(strsplit(as.character(MDL$Collections), ", "))))

# Add "All" at the top of each sorted list
unique_dictionary_p <- c("All", unique_dictionary_p)
unique_countries <- c("All", unique_countries)
unique_topics <- c("All", unique_topics)
unique_regions <- c("All", unique_regions)
unique_collections <- c("All", unique_collections)


unique_idp_questions <- sort(unique(unlist(strsplit(CORPUS_Questions$IDP_Questions, ",\\s*"))))
unique_refugee_questions <- sort(unique(unlist(strsplit(CORPUS_Questions$Refugee_Questions, ",\\s*"))))


# Mapping for IDP Questions
idp_question_labels <- c(
  "1" = "While living in [survey country], have you ever had to flee home?",
  "2" = "What is the main reason why you had to flee home?",
  "3" = "Where were you living right before you were forced to flee home for the first time?",
  "4" = "After being forced to leave home, did you cross an international border?",
  "5" = "How long did you stay abroad? Less than 12 months or more than 12 months?",
  "6" = "What was the primary document that allowed you to stay abroad?",
  "7" = "When forced to flee, where did you move first?"
)

# Mapping for Refugee Questions
refugee_question_labels <- c(
  "1" = "Have you always lived in [survey country]?",
  "2" = "Of which country are you a citizen?",
  "3" = "While living in [survey country], have you ever had to flee home?",
  "4" = "While living in [other country], have you ever had to flee home?",
  "5" = "What is the main reason why you had to flee home?",
  "6" = "After being forced to leave home, did you cross an international border?",
  "7" = "How long did you stay abroad? Less than 12 months or more than 12 months?",
  "8" = "Did you apply for international protection?",
  "9" = "Did you have the intention to apply for international protection?",
  "10" = "What is the outcome of your application for international protection?",
  "11" = "What is the primary document that allows you to stay in [survey country]?"
)

filtered_idp_labels <- idp_question_labels[unique_idp_questions]
filtered_refugee_labels <- refugee_question_labels[unique_refugee_questions]



default_counts <- function(data, column) {
  expanded_values <- unlist(strsplit(as.character(data[[column]]), ", "))
  expanded_data <- data.frame(Value = expanded_values, ID = rep(data$ID, sapply(strsplit(as.character(data[[column]]), ", "), length)))
  value_data <- as.data.frame(table(expanded_data$Value))
  names(value_data) <- c("Value", "Count")
  return(value_data)
}

# Compute the default counts when the app initializes
default_country_counts <- default_counts(MDL, "Country")
default_topic_counts <- default_counts(MDL, "Topics")
default_population_group_counts <- default_counts(MDL, "Population")
default_region_counts <- default_counts(MDL, "Region")
default_collection_counts <- default_counts(MDL, "Collections")



# Co-occurrence matrix for Topics
co_occurrence_matrix <- matrix(0, nrow = length(unique_topics), ncol = length(unique_topics))
rownames(co_occurrence_matrix) <- unique_topics
colnames(co_occurrence_matrix) <- unique_topics


for (topics in MDL$Topics) {
  topic_list <- unlist(strsplit(as.character(topics), ", "))
  
  for (i in 1:length(topic_list)) {
    for (j in 1:length(topic_list)) {
      if (i != j && topic_list[i] %in% unique_topics && topic_list[j] %in% unique_topics) {
        co_occurrence_matrix[topic_list[i], topic_list[j]] <- co_occurrence_matrix[topic_list[i], topic_list[j]] + 1
      }
    }
  }
}


co_occurrence_df <- as.data.frame(as.table(co_occurrence_matrix))



# Aggregate_data function
aggregate_data <- function(availability, match, coverage, country, dictionary_p, topics, year_range, region, collections, unit) {
  filtered_data <- MDL
  
  # Availability filter
  if (!is.null(availability) && length(availability) > 0) {
    filtered_data <- filtered_data %>% filter(Availability_EN %in% availability)
  }
  
  # Match filter
  if (!is.null(match) && length(match) > 0) {
    filtered_data <- filtered_data %>% filter(Match %in% match)
  }
  
  # Coverage filter
  if (!is.null(coverage) && length(coverage) > 0) {
    filtered_data <- filtered_data %>% filter(Coverage %in% coverage)
  }
  
  # Unit of Analysis filter
  if (!is.null(unit) && length(unit) > 0) {
    filtered_data <- filtered_data %>% filter(Unit %in% unit)
  }
  
  # Population Group filter
  if (!is.null(dictionary_p) && length(dictionary_p) > 0) {
    filtered_data <- filtered_data %>%
      filter(sapply(strsplit(as.character(Population), ", "), function(x) {
        if (length(x) == 0 || all(is.na(x))) {
          return(FALSE)
        }
        any(dictionary_p %in% x)
      }))
  }
  
  # Topics filter
  if (!is.null(topics) && length(topics) > 0) {
    filtered_data <- filtered_data %>%
      filter(sapply(strsplit(as.character(Topics), ", "), function(x) {
        if (length(x) == 0 || all(is.na(x))) {
          return(FALSE)
        }
        # Check if all selected topics are present in the list
        all(topics %in% x)
      }))
  }
  
  
  if (nrow(filtered_data) == 0) {
    return(NULL)
  }
  
  # Country filter
  if (!is.null(country) && length(country) > 0) {
    filtered_data <- filtered_data %>%
      filter(sapply(strsplit(as.character(Country), ", "), function(x) {
        if (length(x) == 0 || all(is.na(x))) {
          return(FALSE)
        }
        any(country %in% x)
      }))
  }
  
  # Region filter
  if (!is.null(region) && length(region) > 0) {
    filtered_data <- filtered_data %>%
      filter(sapply(strsplit(as.character(Region), ", "), function(x) {
        if (length(x) == 0 || all(is.na(x))) {
          return(FALSE)
        }
        any(region %in% x)
      }))
  }
  
  # Collections filter
  if (!is.null(collections) && length(collections) > 0) {
    filtered_data <- filtered_data %>%
      filter(sapply(strsplit(as.character(Collections), ", "), function(x) {
        if (length(x) == 0 || all(is.na(x))) {
          return(FALSE)
        }
        any(collections %in% x)
      }))
  }
  
  # Year range filter
  if (nrow(filtered_data) > 0 && length(year_range) == 2) {
    filtered_data <- filtered_data %>% filter(Year >= year_range[1], Year <= year_range[2])
  }
  
  # Return NULL if no data after filtering
  if (nrow(filtered_data) == 0) {
    return(NULL)
  }
  
  
  process_expansion <- function(filtered_data, column_name, default_counts, result_name) {
    expanded_values <- unlist(strsplit(as.character(filtered_data[[column_name]]), ", "))
    expanded_values <- expanded_values[!is.na(expanded_values) & expanded_values != ""]
    expanded_data <- data.frame(Value = expanded_values, ID = rep(filtered_data$ID, sapply(strsplit(as.character(filtered_data[[column_name]]), ", "), length)))
    value_counts <- as.data.frame(table(expanded_data$Value))
    names(value_counts) <- c("Value", "Count")
    
    
    final_counts <- merge(default_counts, value_counts, by.x = "Value", by.y = "Value", all.x = TRUE, suffixes = c("", "_filtered"))
    final_counts$Count <- ifelse(is.na(final_counts$Count_filtered), 0, final_counts$Count_filtered)
    final_counts <- final_counts[, c("Value", "Count")]
    names(final_counts)[1] <- result_name
    
    return(final_counts)
  }
  
  
  country_data <- process_expansion(filtered_data, "Country", default_country_counts, "Country")
  topic_data <- process_expansion(filtered_data, "Topics", default_topic_counts, "Topics")
  population_group_data <- process_expansion(filtered_data, "Population", default_population_group_counts, "PopulationGroup")
  region_data <- process_expansion(filtered_data, "Region", default_region_counts, "Region")
  collection_data <- process_expansion(filtered_data, "Collections", default_collection_counts, "Collections")
  
  
  list(
    country_data = country_data,
    topic_data = topic_data,
    population_group_data = population_group_data,
    region_data = region_data,
    collection_data = collection_data,
    unique_ids = length(unique(filtered_data$ID)),
    year_range = year_range,
    table_data = filtered_data
  )
}


# Define UI
ui <- navbarPage(
  title = "Questionnaire Finder",
  id = "navbar",
  theme = shinytheme("cerulean"),
  tabPanel("Dashboard",
           fluidPage(
             tags$head(
               tags$style(HTML("
          .custom-column-left {
            margin: 0;
            padding: 0;
          }
          .custom-column-middle {
            margin: 0;
            padding: 0;
          }
          .custom-column-right {
            margin: 0;
            padding: 0;
          }
          .info-icon {
            margin-left: 5px;
            color: #0072BC;
            cursor: pointer;
          }
          .label-with-icon {
            display: flex;
            align-items: center;
          }
          .label-with-icon .fa-info-circle {
            margin-left: 1px;
          }
          .filters-title-action {
            display: flex;
            align-items: center;
            justify-content: space-between;
          }
        "))
             ),
             fluidRow(
               column(width = 2,
                      class = "custom-column-left",
                      style = "margin-bottom: 6rem;",
                      box(width = 12, 
                          height = "70rem", 
                          div(class = "filters-title-action",
                              h4(tags$span(style = "font-weight: bold; font-size: 3.2rem; color: #0072BC;", "Filters")),
                              actionButton("show_help", 
                                           label = HTML("About & How to Use<br>the Dashboard"), 
                                           icon = icon("info-circle"),
                                           style = "padding: 16px 16px; font-size: 1rem; height: 6rem;")
                          ),
                          selectizeInput("country", 
                                         "Country", 
                                         choices = c("All", unique_countries), 
                                         selected = NULL, multiple = TRUE),
                          selectizeInput("region", 
                                         "Region", 
                                         choices = c("All", unique_regions), 
                                         selected = NULL, multiple = TRUE),
                          div(class = "label-with-icon",
                              tags$label("Population Group"),
                              tags$i(class = "fa fa-info-circle info-icon", 
                                     id = "info-dictionary_p",
                                     style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                              bsTooltip("info-dictionary_p", 
                                        "Metadata may not be available for some surveys", 
                                        placement = "right", 
                                        trigger = "hover")
                          ),
                          selectizeInput("dictionary_p", 
                                         NULL, 
                                         choices = c("All", unique_dictionary_p), 
                                         selected = NULL, multiple = TRUE),
                          selectizeInput("topics", 
                                         "Topics", 
                                         choices = c("All", unique_topics), 
                                         selected = NULL, multiple = TRUE),
                          div(class = "label-with-icon",
                              tags$label("Representative Sample"),
                              tags$i(class = "fa fa-info-circle info-icon", 
                                     id = "info-match",
                                     style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                              bsTooltip("info-match", 
                                        "UNKNOWN: The microdata is awaiting classification", 
                                        placement = "right", 
                                        trigger = "hover")
                          ),
                          selectizeInput("match", 
                                         NULL, 
                                         choices = unique(MDL$Match), 
                                         selected = NULL, 
                                         multiple = TRUE),
                          div(class = "label-with-icon",
                              tags$label("Geographical Coverage"),
                              tags$i(class = "fa fa-info-circle info-icon", 
                                     id = "info-coverage",
                                     style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                              bsTooltip("info-coverage", 
                                        "Metadata may not be available for some surveys", 
                                        placement = "right", 
                                        trigger = "hover")
                          ),
                          selectizeInput("coverage", 
                                         NULL, 
                                         choices = unique(MDL$Coverage), 
                                         selected = NULL, multiple = TRUE),
                          selectizeInput("unit", 
                                         "Unit of Analysis", 
                                         choices = unique(MDL$Unit), 
                                         selected = NULL, multiple = TRUE),
                          selectizeInput("collections", 
                                         "Collections", 
                                         choices = c("All", unique_collections), 
                                         selected = NULL, multiple = TRUE),
                          
                          div(class = "label-with-icon",
                              tags$label("Language"),
                              tags$i(class = "fa fa-info-circle info-icon", 
                                     id = "info-EN",
                                     style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                              bsTooltip("info-EN", 
                                        "UNKNOWN: The microdata is awaiting classification", 
                                        placement = "right", 
                                        trigger = "hover")
                          ),
                          selectizeInput("availability", 
                                         NULL, 
                                         choices = unique(MDL$Availability_EN), 
                                         selected = NULL, multiple = TRUE),
                          uiOutput("year_slider")
                      )
               ),
               column(width = 7,
                      class = "custom-column-middle",
                      box(width = 12, 
                          height = "80rem", 
                          div(style = "text-align: center; margin-right: 1rem;",
                              h4(tags$span(style = "font-weight: bold; font-size: 2.8rem; color: #0072BC;", "Number of Datasets with Questionnaires")),
                              div(style = "display: flex; align-items: center; justify-content: center; margin-bottom: -1rem;",
                                  tags$span(style = "font-weight: bold; font-size: 3.5rem; color: #00B398;",
                                            textOutput("totalCount", 
                                                       inline = TRUE)),
                                  tags$i(class = "fa fa-info-circle info-icon", 
                                         id = "info-datasets",
                                         style = "margin-left: 0.5rem; color: #0072BC; font-size: 1.6rem;")
                              ),
                              bsTooltip("info-datasets", 
                                        HTML('To access datasets without questionnaires, please go to the <a href="https://microdata.unhcr.org/index.php/catalog/?page=1&ps=15" target="_blank"><b>UNHCR MDL</b></a>'), 
                                        placement = "right", 
                                        trigger = "click hover"),
                              div(style = "text-align: center; margin-bottom: 0.3rem; margin-top: 1rem; font-size: 1rem;", 
                                  "Click on a country to view the questionnaires available"
                              ),
                              leafletOutput("countryMap", 
                                            height = "64rem") 
                          ),
                          div(style = "font-size: 1.1rem; text-align: center; margin-top: 0.6rem; margin-bottom: 1.3rem;", 
                              "The boundaries, colors, denominations, and other information shown on any map in this work do not imply any judgment on the part of The World Bank or UNHCR concerning the legal status of any territory or the endorsement or acceptance of such boundaries")
                      )
               ),
               column(width = 3,
                      class = "custom-column-right",
                      box(width = 12, 
                          height = "70rem", 
                          div(style = "text-align:center; margin-bottom: 1.5rem;",
                              h4(tags$span(style = "font-weight: bold; font-size: 3.2rem; color: #0072BC;", "Data Table"))),
                          div(style = "font-size: 1.1rem;",
                              DTOutput("dataTable", 
                                       height = "70rem")
                              
                          )
                      )
               )
             )
           )
  ),
  
  
  
  
  
  
  navbarMenu("Explore",
             tabPanel("Representativeness",
                      fluidPage(
                        fluidRow(
                          column(width = 3,
                                 box(width = 8, 
                                     height = "82rem",
                                     style = "margin-right: -8rem; margin-bottom: 8rem",
                                     div(style = "text-align: left;",
                                         h4(tags$span(style = "font-weight: bold; font-size: 3.2rem; color: #0072BC;", "Filters"))),
                                     selectizeInput("desc_country", 
                                                    "Country", 
                                                    choices = c("All", unique_countries), 
                                                    selected = NULL, multiple = TRUE),
                                     selectizeInput("desc_region", 
                                                    "Region", 
                                                    choices = c("All", unique_regions), 
                                                    selected = NULL, multiple = TRUE),
                                     div(class = "label-with-icon",
                                         tags$label("Population Group"),
                                         tags$i(class = "fa fa-info-circle info-icon", 
                                                id = "info-desc_dictionary_p",
                                                style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                                         bsTooltip("info-desc_dictionary_p", 
                                                   "Metadata may not be available for some surveys", 
                                                   placement = "right", 
                                                   trigger = "hover")
                                     ),
                                     selectizeInput("desc_dictionary_p", 
                                                    NULL, 
                                                    choices = c("All", unique_dictionary_p), 
                                                    selected = NULL, multiple = TRUE),
                                     selectizeInput("desc_topics", 
                                                    "Topics", 
                                                    choices = c("All", unique_topics), 
                                                    selected = NULL, multiple = TRUE),
                                     div(class = "label-with-icon",
                                         tags$label("Representative Sample"),
                                         tags$i(class = "fa fa-info-circle info-icon", 
                                                id = "info-desc_match",
                                                style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                                         bsTooltip("info-desc_match", 
                                                   "UNKNOWN: The microdata is awaiting classification", 
                                                   placement = "right", 
                                                   trigger = "hover")
                                     ),
                                     selectizeInput("desc_match", 
                                                    NULL, 
                                                    choices = unique(MDL$Match), 
                                                    selected = NULL, 
                                                    multiple = TRUE),
                                     div(class = "label-with-icon",
                                         tags$label("Geographical Coverage"),
                                         tags$i(class = "fa fa-info-circle info-icon", 
                                                id = "info-desc_coverage",
                                                style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                                         bsTooltip("info-desc_coverage", 
                                                   "Metadata may not be available for some surveys", 
                                                   placement = "right", 
                                                   trigger = "hover")
                                     ),
                                     selectizeInput("desc_coverage", 
                                                    NULL, 
                                                    choices = unique(MDL$Coverage), 
                                                    selected = NULL, multiple = TRUE),
                                     selectizeInput("desc_unit", 
                                                    "Unit of Analysis", 
                                                    choices = unique(MDL$Unit), 
                                                    selected = NULL, multiple = TRUE),
                                     selectizeInput("desc_collections", 
                                                    "Collections", 
                                                    choices = c("All", unique_collections), 
                                                    selected = NULL, multiple = TRUE),
                                     div(class = "label-with-icon",
                                         tags$label("Language"),
                                         tags$i(class = "fa fa-info-circle info-icon", 
                                                id = "info-EN-DES",
                                                style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                                         bsTooltip("info-EN-DES", 
                                                   "UNKNOWN: The microdata is awaiting classification", 
                                                   placement = "right", 
                                                   trigger = "hover")
                                     ),
                                     selectizeInput("desc_availability", 
                                                    NULL, 
                                                    choices = unique(MDL$Availability_EN), 
                                                    selected = NULL, multiple = TRUE),
                                     uiOutput("desc_year_slider")
                                 )
                          ),
                          column(width = 9,
                                 fluidRow(
                                   column(width = 12,
                                          box(width = 12, 
                                              height = "6.25rem", 
                                              class = "small-box", 
                                              div(style = "text-align: center;",
                                                  h4(tags$span(style = "font-weight: bold; font-size: 2.8rem; color: #0072BC;", "Number of Datasets with Questionnaires")),
                                                  div(style = "display: inline-flex; align-items: center;",
                                                      tags$span(style = "font-weight: bold; font-size: 3.2rem; color: #00B398;", 
                                                                textOutput("desc_totalCount", inline = TRUE)),
                                                      tags$i(class = "fa fa-info-circle info-icon", 
                                                             id = "info-datasets-questionnaires",
                                                             style = "margin-left: 0.5rem; color: #0072BC; font-size: 1.6rem;")
                                                  )
                                              ),
                                              bsTooltip("info-datasets-questionnaires", 
                                                        HTML('To access datasets without questionnaires, please go to the <a href="https://microdata.unhcr.org/index.php/catalog/?page=1&ps=15" target="_blank"><b>UNHCR MDL</b></a>'), 
                                                        placement = "right", 
                                                        trigger = "hover click"),
                                              style = "font-size: 1rem; text-align: center;"  
                                          )
                                   )
                                 ),
                                 fluidRow(
                                   column(width = 6,
                                          box(width = 12,
                                              height = "60rem", 
                                              style = "margin-top: 2rem;",
                                              plotlyOutput("desc_barChart", 
                                                           height = "72rem") 
                                          )
                                   ),
                                   column(width = 6,
                                          box(width = 12, 
                                              height = "62rem", 
                                              style = "margin-top: 8rem;",
                                              plotlyOutput("desc_doughnutChart", 
                                                           height = "78rem") 
                                          )
                                   )
                                 )
                          ),
                          column(width = 12, 
                                 box(width = 12, 
                                     height = "50rem", 
                                     style = "margin-top: 1rem;",
                                     div(style = "overflow-x: auto; font-size: 1.5rem;", 
                                         DTOutput("desc_dataTable")
                                     )
                                 )
                          )
                        )
                      )
             ),
             
             
             
             
             
             
             tabPanel("Topic Explorer",
                      fluidPage(
                        fluidRow(
                          column(width = 3,
                                 box(width = 8, 
                                     height = "82rem", 
                                     style = "margin-bottom: 6rem; margin-right: -4rem;", 
                                     div(style = "text-align: left;",
                                         h4(tags$span(style = "font-weight: bold; font-size: 3.2rem; color: #0072BC;", "Filters"))),
                                     selectizeInput("topics_country", 
                                                    "Country", 
                                                    choices = c("All", unique_countries), 
                                                    selected = NULL, 
                                                    multiple = TRUE),
                                     selectizeInput("topics_region", 
                                                    "Region", 
                                                    choices = c("All", unique_regions), 
                                                    selected = NULL, 
                                                    multiple = TRUE),
                                     div(class = "label-with-icon",
                                         tags$label("Population Group"),
                                         tags$i(class = "fa fa-info-circle info-icon", 
                                                id = "info-topics_dictionary_p",
                                                style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                                         bsTooltip("info-topics_dictionary_p", 
                                                   "Metadata may not be available for some surveys", 
                                                   placement = "right", 
                                                   trigger = "hover")
                                     ),
                                     selectizeInput("topics_dictionary_p", 
                                                    NULL, 
                                                    choices = c("All", unique_dictionary_p), 
                                                    selected = NULL, 
                                                    multiple = TRUE),
                                     
                                     
                                     div(class = "label-with-icon",
                                         tags$label("Topics"),
                                         tags$i(class = "fa fa-info-circle info-icon", 
                                                id = "info-topics_topics",
                                                style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                                         bsTooltip("info-topics_topics", 
                                                   "Most questionnaires cover more than 1 topic", 
                                                   placement = "right", 
                                                   trigger = "hover")
                                     ),
                                     selectizeInput("topics_topics", 
                                                    NULL, 
                                                    choices = c("All", unique_topics), 
                                                    selected = NULL, multiple = TRUE),
                                     
                                     
                                     
                                     
                                     
                                     div(class = "label-with-icon",
                                         tags$label("Representative Sample"),
                                         tags$i(class = "fa fa-info-circle info-icon", 
                                                id = "info-topics_match",
                                                style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                                         bsTooltip("info-topics_match", 
                                                   "UNKNOWN: The microdata is awaiting classification", 
                                                   placement = "right", 
                                                   trigger = "hover")
                                     ),
                                     selectizeInput("topics_match", 
                                                    NULL, 
                                                    choices = unique(MDL$Match), 
                                                    selected = NULL, 
                                                    multiple = TRUE),
                                     div(class = "label-with-icon",
                                         tags$label("Geographical Coverage"),
                                         tags$i(class = "fa fa-info-circle info-icon", 
                                                id = "info-topics_coverage",
                                                style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                                         bsTooltip("info-topics_coverage", 
                                                   "Metadata may not be available for some surveys", 
                                                   placement = "right", 
                                                   trigger = "hover")
                                     ),
                                     selectizeInput("topics_coverage", 
                                                    NULL, 
                                                    choices = unique(MDL$Coverage), 
                                                    selected = NULL, 
                                                    multiple = TRUE),
                                     
                                     selectizeInput("topics_unit", 
                                                    "Unit of Analysis", 
                                                    choices = unique(MDL$Unit), 
                                                    selected = NULL, multiple = TRUE),
                                     
                                     selectizeInput("topics_collections", 
                                                    "Collections", 
                                                    choices = c("All", unique_collections), 
                                                    selected = NULL, 
                                                    multiple = TRUE),
                                     
                                     
                                     div(class = "label-with-icon",
                                         tags$label("Language"),
                                         tags$i(class = "fa fa-info-circle info-icon", 
                                                id = "info-EN-TOPIC",
                                                style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                                         bsTooltip("info-EN-TOPIC", 
                                                   "UNKNOWN: The microdata is awaiting classification", 
                                                   placement = "right", 
                                                   trigger = "hover")
                                     ),
                                     
                                     selectizeInput("topics_availability", 
                                                    NULL, 
                                                    choices = unique(MDL$Availability_EN), 
                                                    selected = NULL, 
                                                    multiple = TRUE),
                                     
                                     
                                     
                                     uiOutput("topics_year_slider")
                                 )
                          ),
                          column(width = 9,
                                 box(width = 12, 
                                     height = "70rem", 
                                     style = "margin-left: -8rem; margin-bottom: -3rem;", 
                                     div(style = "text-align: center;",
                                         h4(tags$span(style = "font-weight: bold; font-size: 3.2rem; color: #0072BC; margin-bottom: -8rem;", "Topics Distribution")),
                                         actionButton("show_treemap_info", 
                                                      label = "Learn more about the chart", 
                                                      icon = icon("info-circle")),
                                         
                                     ), 
                                     plotlyOutput("topics_treemap", height = "74rem")
                                 )
                          ),
                          column(width = 12, 
                                 box(width = 12, 
                                     height = "50rem", 
                                     style = "margin-top: 5rem;", 
                                     div(style = "overflow-x: auto; font-size: 1.5rem;", 
                                         DTOutput("topics_dataTable")
                                     )
                                 )
                          )
                        )
                      )
             ),
             
             
             tabPanel("Topic Co-occurrence",
                      fluidPage(
                        fluidRow(
                          div(style = "text-align: center;",
                              h4(tags$span(style = "font-weight: bold; font-size: 3.2rem; color: #0072BC;", "Filters")))
                        ),
                        fluidRow(
                          column(width = 1),  # left margin
                          column(width = 2, 
                                 selectizeInput("more_topics_country", 
                                                "Country", 
                                                choices = c("All", unique_countries), 
                                                selected = NULL, 
                                                multiple = TRUE)),
                          
                          column(width = 2,
                                 selectizeInput("more_topics_region", 
                                                "Region", 
                                                choices = c("All", unique_regions), 
                                                selected = NULL, multiple = TRUE)),
                          
                          column(width = 2,
                                 div(style = "display: flex; align-items: center; justify-content: space-between;",
                                     tags$label("Population Group"),
                                     tags$i(class = "fa fa-info-circle info-icon", 
                                            id = "info-moretopics_dictionary_p",
                                            style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                                     bsTooltip("info-moretopics_dictionary_p", 
                                               "Metadata may not be available for some surveys", 
                                               placement = "right", 
                                               trigger = "hover")
                                 ),
                                 selectizeInput("more_topics_dictionary_p", 
                                                NULL, 
                                                choices = c("All", unique_dictionary_p), 
                                                selected = NULL, multiple = TRUE)),
                          
                          column(width = 2, 
                                 div(style = "display: flex; align-items: center; justify-content: space-between;",
                                     tags$label("Topics"),
                                     tags$i(class = "fa fa-info-circle info-icon", 
                                            id = "info-moretopics_topics",
                                            style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                                     bsTooltip("info-moretopics_topics", 
                                               "Most questionnaires cover more than 1 topic", 
                                               placement = "right", 
                                               trigger = "hover")
                                 ),
                                 selectizeInput("more_topics_topics", 
                                                NULL, 
                                                choices = c("All", unique_topics), 
                                                selected = NULL, multiple = TRUE)),
                          
                          column(width = 2,
                                 div(style = "display: flex; align-items: center; justify-content: space-between;",
                                     tags$label("Representative Sample",
                                                style = "flex-grow: 1; font-size: 1.3rem;"),
                                     tags$i(class = "fa fa-info-circle info-icon", 
                                            id = "info-more_match",
                                            style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                                     bsTooltip("info-more_match", 
                                               "UNKNOWN: The microdata is awaiting classification", 
                                               placement = "right", 
                                               trigger = "hover")
                                 ),
                                 selectizeInput("more_topics_match", 
                                                NULL, 
                                                choices = unique(MDL$Match), 
                                                selected = NULL, multiple = TRUE)),
                          
                          column(width = 1)  # right margin
                        ),
                        fluidRow(
                          column(width = 1),  # left margin
                          
                          column(width = 2,
                                 div(style = "display: flex; align-items: center; justify-content: space-between;",
                                     tags$label("Geographical Coverage", 
                                                style = "flex-grow: 1; font-size: 1.3rem;"),
                                     tags$i(class = "fa fa-info-circle info-icon", 
                                            id = "info-more_coverage"),
                                     bsTooltip("info-more_coverage", 
                                               "Metadata may not be available for some surveys", 
                                               placement = "right", 
                                               trigger = "hover")
                                 ),
                                 selectizeInput("more_topics_coverage", 
                                                NULL, 
                                                choices = unique(MDL$Coverage), 
                                                selected = NULL, multiple = TRUE)),
                          column(width = 2,
                                 selectizeInput("more_topics_unit",
                                                "Unit of Analysis",
                                                choices = unique(MDL$Unit),
                                                selected = NULL, multiple = TRUE)),
                          
                          column(width = 2,
                                 selectizeInput("more_topics_collections", 
                                                "Collections", 
                                                choices = c("All", unique_collections), 
                                                selected = NULL, multiple = TRUE)),
                          
                          column(width = 2,
                                 div(style = "display: flex; align-items: center; justify-content: space-between;",
                                     tags$label("Language", 
                                                style = "flex-grow: 1; font-size: 1.3rem;"),
                                     tags$i(class = "fa fa-info-circle info-icon", 
                                            id = "info-more_TOPIC"),
                                     bsTooltip("info-more_TOPIC", 
                                               "UNKNOWN: The microdata is awaiting classification", 
                                               placement = "right", 
                                               trigger = "hover")
                                 ),
                                 selectizeInput("more_topics_availability", 
                                                NULL, 
                                                choices = unique(MDL$Availability_EN), 
                                                selected = NULL, multiple = TRUE)),
                          
                          
                          column(width = 2,
                                 div(style = "text-align: left;",
                                     uiOutput("more_topics_year_slider")))
                        ),
                        fluidRow(
                          column(width = 12,
                                 box(width = 12, 
                                     style = "margin-bottom: 1rem;", 
                                     div(style = "font-size: 1.5rem;",
                                         DTOutput("more_topics_dataTable") 
                                     )
                                 )
                          )
                        ),
                        fluidRow(
                          div(style = "text-align: center;",
                              h4(tags$span(style = "font-weight: bold; font-size: 3.2rem; color: #0072BC;", "Heatmap")),
                              div(style = "margin-top: 1rem;",  
                                  actionButton("show_heatmap_info", 
                                               label = "Learn more about the chart", 
                                               icon = icon("info-circle"))
                              )
                          ),
                          column(width = 12,
                                 box(width = 12, 
                                     style = "margin-top: 1rem; text-align: center;", 
                                     div(style = "display: inline-block;",  
                                         plotlyOutput("heatmap")) 
                                 )
                          )
                        )
                      )
             )
  ),
  
  
  
  
  
  
  tabPanel("EGRISS-related Language",
           fluidPage(
             fluidRow(
               column(width = 12,
                      box(width = 12,
                          title = tags$h3(tags$strong(tags$span(style = "color: #0072BC;", "What is EGRISS-related language?"))),
                          p("The International Recommendations on Refugee and IDP Statistics (IRRS and IRIS), developed by the Expert Group on Refugee and IDP Statistics (EGRISS) in 2018 and 2020, respectively, provide the statistical framework for identifying refugees and IDPs in official data production. This section follows ",
                            tags$a(href="https://egrisstats.org/wp-content/uploads/EGRISS-Methodological-Paper-Towards-a-standardized-approach-to-identify-IDPs-refugees-1.pdf", target="_blank", "EGRISS’s methodological paper"), 
                            " on question sets that are feasible for use in household surveys for both IDPs and refugees.",
                            br(), br(),
                            "There are 7 recommended questions to identify IDPs and 11 questions to identify refugees in household surveys. The ", tags$strong("Select IDP and Refugee Questions"), " filters allow you to filter by specific questions. However, ", 
                            tags$strong(tags$span(style = "color: #0072BC;",
                                                  "selecting the questions does not imply that the questionnaires contain the exact wording of the question. Rather, it indicates that the language aligns with the EGRISS criteria, meaning all the keywords of the question are present in the questionnaire.")), 
                            "For a more detailed and customized word search, please use the ", tags$strong("Word Search"), " section.",
                            br(), br()
                          )
                      )
               )
             ),
             fluidRow(
               column(width = 3,
                      style = "margin-bottom: 12rem;",
                      box(width = 12, 
                          height = "100rem", 
                          div(class = "filters-title-action",
                              h4(tags$span(style = "font-weight: bold; font-size: 3.2rem; color: #0072BC;", "Filters"))
                          ),
                          
                          div(style = "max-width: 300px;",  
                              bsCollapse(
                                id = "questions_collapse",
                                multiple = TRUE, 
                                bsCollapsePanel("Select IDP Questions", 
                                                checkboxGroupInput("idp_questions_checkboxes", 
                                                                   label = NULL, 
                                                                   choices = setNames(unique_idp_questions, idp_question_labels[unique_idp_questions])),
                                                style = "primary"),
                                bsCollapsePanel("Select Refugee Questions", 
                                                checkboxGroupInput("refugee_questions_checkboxes", 
                                                                   label = NULL, 
                                                                   choices = setNames(unique_refugee_questions, refugee_question_labels[unique_refugee_questions])),
                                                style = "primary")
                              )
                          ),
                          
                          
                          
                          
                          
                          selectizeInput("country_egriss", 
                                         "Country", 
                                         choices = c("All", unique_countries), 
                                         selected = NULL, multiple = TRUE),
                          selectizeInput("region_egriss", 
                                         "Region", 
                                         choices = c("All", unique_regions), 
                                         selected = NULL, multiple = TRUE),
                          
                          div(class = "label-with-icon",
                              tags$label("Population Group"),
                              tags$i(class = "fa fa-info-circle info-icon", 
                                     id = "info-dictionary_p_egriss",
                                     style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                              bsTooltip("info-dictionary_p_egriss", 
                                        "Metadata may not be available for some surveys", 
                                        placement = "right", 
                                        trigger = "hover")
                          ),
                          selectizeInput("dictionary_p_egriss", 
                                         NULL, 
                                         choices = c("All", unique_dictionary_p), 
                                         selected = NULL, multiple = TRUE),
                          
                          
                          selectizeInput("topics_egriss", 
                                         "Topics", 
                                         choices = c("All", unique_topics), 
                                         selected = NULL, multiple = TRUE),
                          
                          
                          
                          div(class = "label-with-icon",
                              tags$label("Representative Sample"),
                              tags$i(class = "fa fa-info-circle info-icon", 
                                     id = "info-match_egriss",
                                     style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                              bsTooltip("info-match_egriss", 
                                        "UNKNOWN: The microdata is awaiting classification", 
                                        placement = "right", 
                                        trigger = "hover")
                          ),
                          selectizeInput("match_egriss", 
                                         NULL, 
                                         choices = unique(MDL$Match), 
                                         selected = NULL, 
                                         multiple = TRUE),
                          
                          
                          div(class = "label-with-icon",
                              tags$label("Geographical Coverage"),
                              tags$i(class = "fa fa-info-circle info-icon", 
                                     id = "info-coverage_egriss",
                                     style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                              bsTooltip("info-coverage_egriss", 
                                        "Metadata may not be available for some surveys", 
                                        placement = "right", 
                                        trigger = "hover")
                          ),
                          selectizeInput("coverage_egriss", 
                                         NULL, 
                                         choices = unique(MDL$Coverage), 
                                         selected = NULL, multiple = TRUE),
                          
                          
                          selectizeInput("unit_egriss", 
                                         "Unit of Analysis", 
                                         choices = unique(MDL$Unit), 
                                         selected = NULL, multiple = TRUE),
                          selectizeInput("collections_egriss", 
                                         "Collections", 
                                         choices = c("All", unique_collections), 
                                         selected = NULL, multiple = TRUE),
                          
                          
                          div(class = "label-with-icon",
                              tags$label("Language"),
                              tags$i(class = "fa fa-info-circle info-icon", 
                                     id = "info-EN_egriss",
                                     style = "margin-left: 0.5rem; font-size: 1.2rem;"),
                              bsTooltip("info-EN_egriss", 
                                        "UNKNOWN: The microdata is awaiting classification", 
                                        placement = "right", 
                                        trigger = "hover")
                          ),
                          selectizeInput("availability_egriss", 
                                         NULL, 
                                         choices = unique(MDL$Availability_EN), 
                                         selected = NULL, multiple = TRUE),
                          
                          
                          
                          
                          uiOutput("year_slider_egriss")
                          
                      )
               ),
               column(width = 9,
                      box(width = 12, 
                          height = "74rem", 
                          div(style = "margin-left: -2rem; text-align: center;",
                              h3(tags$span(style = "font-size: 3.2rem; font-weight: bold; color: #0072BC;", "Data Table")), 
                              DTOutput("egriss_dataTable")   
                          )
                      )
               )
             )
           )
  ),
  
  
  
  
  
  tabPanel("Words Search",
           fluidPage(
             fluidRow(
               column(width = 3,
                      div(style = "text-align: left; margin-bottom: 1rem;",  
                          actionButton("show_search_info", 
                                       label = ("How to Use the Search Tool"), 
                                       icon = icon("info-circle"))
                      )
               ),
               column(width = 6,
                      div(style = "text-align: center; margin-bottom: 1rem;",
                          h3(style = "font-size: 3rem; font-weight: bold; margin: 0; color: #0072BC;", "Search Key Words")
                      )
               ),
               column(width = 3) 
             ),
             fluidRow(
               column(width = 12,
                      div(style = "text-align: center; margin-bottom: 2rem;",
                          div(style = "display: inline-block; width: 50%;",
                              tags$input(id = "word_search_input", 
                                         type = "text", 
                                         placeholder = "Type any word(s) to search...", 
                                         style = "width: 100%; font-size: 1.5rem; padding: 0.5rem; border: 4px solid #000; border-radius: 6px;")
                          )
                      )
               )
             ),
             br(),
             box(width = 12, 
                 style = "margin-bottom: 1rem;", 
                 div(style = "font-size: 1.5rem;",
                     DTOutput("word_search_dataTable")  
                 )
             )
           )
  ),
  
  
  
  
  
  tabPanel("Summary Statistics",
           fluidPage(
             fluidRow(
               column(width = 12,
                      div(style = "margin-bottom: 2rem; font-size: 1.6rem; font-weight: bold; color: #0072BC;", 
                          "You can select a variable to see the summary statistics of the questionnaires on this Dashboard"),
                      selectInput("desc_var", 
                                  "Select", 
                                  choices = c("Year", "Country", "Topics", "Region", "Collections","Population Group", "Unit of Analysis", "Representative Sample"), 
                                  selected = "Year"),
                      box(width = 12, 
                          height = "calc(100vh - 7rem)", 
                          plotlyOutput("descriptive_barChart", 
                                       height = "calc(100vh - 10rem)", 
                                       width = "100%")), 
                      div(style = "text-align:center; margin-top: 2rem; margin-bottom: 2rem;", 
                          "This is only for visualization. To access the links for more information, please go back to the Dashboard.")
               )
             )
           )
  ),
  
  
  
  tabPanel("Database",
           fluidPage(
             fluidRow(
               column(width = 12,
                      downloadButton("downloadData", "Download Dataset"),
                      DTOutput("databaseTable")
               )
             )
           )
  ),
  
  
  tabPanel("Methodology",
           fluidPage(
             fluidRow(
               column(width = 12,
                      box(width = 12,
                          title = tags$h3(tags$strong(tags$span(style = "color: #0072BC;", "Background"))),
                          p("The UNHCR Microdata Library (MDL) is a public portal for anonymized microdata collected by UNHCR and its partners, facilitating evidence-based programming and policy-making. Developed in collaboration with the World Bank and the UNHCR Joint Data Center (JDC), the Global Data Service team, and its partners, it serves a wide range of users, including governments, international organizations, academia, and civil society. By making extensive datasets accessible, the MDL enables development partners to address issues related to forced displacement and the implementation of humanitarian and development agendas.",
                            br(), br(),
                            "The MDL includes numerous survey questionnaires uploaded as part of the dataset listings. To enhance accessibility and discoverability, the JDC, in support of the MDL curation team, has initiated a review to determine the number of available questionnaires, assess their representative samples, and evaluate their geographical coverage, topics, population coverage, and language using an automated process. The resulting \"Questionnaire Finder\" aims to increase the discoverability and accessibility of these questionnaires, facilitating their potential reuse or adaptation by governments and other organizations conducting related surveys. The data and dashboard can be automatically updated when new datasets and questionnaires are added to the UNHCR MDL.",
                            br(), br(),
                            "Please note that the Questionnaire Finder Dashboard hosts only datasets that include questionnaires from the UNHCR MDL. To access other datasets without questionnaires, please go to the ",
                            tags$a(href = "https://microdata.unhcr.org/index.php/catalog/?page=1&ps=15", target = "_blank", "UNHCR MDL.")
                          )
                      )
               )
             ),
             fluidRow(
               column(width = 12,
                      box(width = 12,
                          title = tags$h3(tags$strong(tags$span(style = "color: #0072BC;", "Variables"))),
                          p("The variables provided in the dashboard are as follows:",
                            br(), br(),
                            "• ", tags$b("ID:"), " Represents the unique identifier used on the UNHCR MDL for each survey.", br(), br(),
                            "• ", tags$b("Title:"), " The title of the survey.", br(), br(),
                            "• ", tags$b("Country:"), " The country or countries where the survey was conducted.", br(), br(),
                            "• ", tags$b("Year:"), " The year in which the survey data was collected.", br(), br(),
                            "• ", tags$b("Region:"), " The region where the survey was conducted.", br(), br(),
                            "• ", tags$b("Collection:"), " The collection of the survey.", br(), br(),
                            "• ", tags$b("URL:"), " The link to the MDL that provides detailed descriptions of the survey and download links for the questionnaires.", br(), br(),
                            "• ", tags$b("Representative Sample:"), " Indicates whether the survey has a representative sample according to the definition of ", tags$a(href = "https://documents.worldbank.org/en/publication/documents-reports/documentdetail/099556112062334277/idu0161b812905efa046ca09d4909a91003ae709", "Masaki and Madson’s working paper."), 
                            " If it is marked as \"Unknown\", it means that the microdata hasn't been classified yet, and the work is still ongoing.", br(), br(),
                            "• ", tags$b("Geographical Coverage:"), " Shows whether the survey has national or sub-national coverage.", br(), br(),
                            "• ", tags$b("Population Group:"), " Lists the population groups covered by the survey.", br(), br(),
                            "• ", tags$b("Topics:"), " Lists the topics covered by the survey that are recorded in the MDL metadata.", br(), br(),
                            "• ", tags$b("Unit of Analysis:"), " Indicates whether the survey is conducted at the household level, individual level, both, or involves key informants.", br(), br(),
                            "• ", tags$b("Language:"), " Indicates the language of the questionnaires. If a questionnaire is available in English, it may or may not include other languages, but only English is selected when multiple languages are present. If English is not available, the questionnaire is available in only one other language, which the filter specifies.", br(), br(),
                            "• ", tags$b("EGRISS-related Language:"), " Identifies questionnaires that contain language recommended by the", tags$a(href = "https://egrisstats.org/wp-content/uploads/EGRISS-Methodological-Paper-Towards-a-standardized-approach-to-identify-IDPs-refugees-1.pdf", "Expert Group on Refugee and Internally Displaced Persons Statistics (EGRISS) for use in household surveys."), br()
                          )
                      )
               )
             ),
             fluidRow(
               column(width = 12,
                      box(width = 12,
                          title = tags$h3(tags$strong(tags$span(style = "color: #0072BC;", "Method"))),
                          p("The variables ID, Title, Country, Year, Region, Collection, Unit of Analysis, and URL are sourced from the UNHCR MDL using API, while other variables are derived using different methods.",
                            "To evaluate whether the questionnaire has a representative sample, a matching process was conducted using ", tags$a(href = "https://rstudio.unhcr.org/FDMAtlas/#shiny-tab-home", "JDC’s previous work"), " that investigated representative surveys.",
                            "For geographical coverage and topics, information is taken from the MDL metadata, cleaned, and listed.",
                            "For the population group, a combination of metadata and a dictionary method were used to identify the population of concern covered by the questionnaires.",
                            "For language, all files in the questionnaire section of the UNHCR MDL were checked using the R package", tags$a(href = "https://github.com/cld2owners/cld2#readme", "Chromium Compact Language Detector 2 (CLD2)."), 
                            "CLD2 is a language detection library developed by Google that facilitates the identification of languages in text data using probabilistic models.",
                            "Capable of recognizing over 80 languages, R CLD2 is particularly useful in text mining, natural language processing, and data preprocessing for multilingual datasets.",
                            "In the case of this dashboard, if any documents in the questionnaire section are in English, the \"Language\" variable is marked \"English+\". The \"+\" symbol is used because a single questionnaire might contain multiple languages. However, while the precision and accuracy of English detection is at 99%, the detection of a second language is less reliable. Therefore, the term \"English+\" has been chosen. If none of the documents are in English, the variable will indicate the detected language of the document.",
                            "It is important to note that if English is not present, there will only be one language, which CLD2 can accurately and precisely detect.", br(), br(),
                            "EGRISS has developed a set of recommended questions to accurately identify Internally Displaced Persons (IDPs) and refugees within household surveys. These questions include specific wording and key terms crucial for the precise identification and categorization of these populations. The methodology involves using these pre-defined questions as a reference to scan through survey questionnaires. Each survey is systematically assessed to determine if it includes language that closely aligns with the EGRISS-recommended questions. This involves a keyword-matching process, wherein specific terms from the recommended questions are identified within the questionnaire. However, it is important to note that this approach has certain limitations. The reliance on keyword matching may lead to the identification of false positives: where a questionnaire contains similar language but does not fully align with the intended meaning of the EGRISS criteria. While this approach has its flaws, it remains an efficient, systematic, and scalable approach for identifying potential instances of EGRISS-related language in household surveys and provides a valuable foundation for subsequent in-depth analysis.", br()
                          )
                      )
               )
             ),
             fluidRow(
               column(width = 12,
                      box(width = 12,
                          title = tags$h3(tags$strong(tags$span(style = "color: #0072BC;", "Number of Dataset with Questionnaires VS Number of Unique Questionnaires"))),
                          p(
                            "The dashboard reports ", tags$b("\"the number of datasets with questionnaires\""), " rather than ", tags$b("\"the number of unique questionnaires\""), " due to variations in questionnaire implementation. In some cases, questionnaire filenames and content remain consistent across datasets (e.g., the Voluntary Repatriation in Afghanistan). However, in other cases, datasets may share the same filename but contain different content (e.g., the WASH KAP surveys in the Republic of Congo, which is in French and contains different questions from the same series of surveys in South Sudan). Moreover, another issue is that a single dataset might contain more than one questionnaire. As of September 2024, the UNHCR MDL hosts 562 datasets with questionnaires, from which 1235 questionnaire files are attached. Among these, 909 have unique file names. The following table presents the occurrence of the most repeated filenames:"
                          ),
                          tableOutput("duplicate_table")
                      )
               )
             )
           )
  ),
  
  
  tabPanel(
    "Code",
    tags$script(HTML('
      document.querySelector("a[data-value=Code]").onclick = function() {
        window.location.href = "https://github.com/Anqi-Pan/MDL-Dashboard-";
      }
    '))
  )
)






# Server

server <- function(input, output, session) {
  min_year <- min(MDL$Year)
  max_year <- max(MDL$Year)
  
  # Mapping abbreviated country names to full names 
  country_name_map <- c(
    "S. Sudan" = "South Sudan",
    "S. Korea" = "South Korea",
    "N. Korea" = "North Korea"
  )
  
  UNHCR_bounds <- ms_simplify(UNHCR_bounds, keep = 0.01)
  
  # Reactive value to store the map view
  map_view <- reactiveValues(lat = 0, lng = 0, zoom = 1.5)
  
  # Reactive value to store the filtered data
  filtered_data <- reactive({
    availability <- input$availability
    match <- input$match
    coverage <- input$coverage
    unit <- input$unit
    
    country <- if ("All" %in% input$country) unique_countries else input$country
    dictionary_p <- if ("All" %in% input$dictionary_p) unique_dictionary_p else input$dictionary_p
    topics <- if ("All" %in% input$topics) unique_topics else input$topics
    region <- if ("All" %in% input$region) unique_regions else input$region
    collections <- if ("All" %in% input$collections) unique_collections else input$collections
    
    aggregate_data(availability, match, coverage, country, dictionary_p, topics, input$year_range, region, collections, unit)
  })
  
  
  output$year_slider <- renderUI({
    sliderInput("year_range", 
                "Select Year Range", 
                min = min_year, 
                max = max_year, 
                value = c(min_year, max_year), 
                step = 1, 
                sep = "")
  })
  
  # Reactive value to store the total count
  totalCount <- reactiveVal(0)
  
  observe({
    data <- filtered_data()
    totalCount(if (is.null(data)) 0 else data$unique_ids)
  })
  
  output$totalCount <- renderText({
    totalCount()
  })
  
  output$countryMap <- renderLeaflet({
    data <- filtered_data()
    
    if (is.null(data)) {
      leaflet() %>% 
        addTiles() %>%
        addControl("No questionnaires available", position = "topright")
    } else {
      country_data <- data$country_data
      
      # Normalize country names
      country_data$Country <- recode(country_data$Country, !!!country_name_map)
      
      # Filter data to include only the selected country
      selected_country <- input$country
      if (!is.null(selected_country) && length(selected_country) > 0) {
        country_data <- country_data[country_data$Country %in% selected_country, ]
      }
      
      
      merged_data <- UNHCR_bounds %>%
        left_join(country_data, by = c("gis_name" = "Country"))
      
      
      overall_data <- data$country_data
      overall_data$Country <- recode(overall_data$Country, !!!country_name_map)
      overall_merged_data <- UNHCR_bounds %>%
        left_join(overall_data, by = c("gis_name" = "Country"))
      
      pal <- colorNumeric(palette = c("#044F85", 
                                      "#589BE5",
                                      "#FFF9CB", 
                                      "#FF8490", 
                                      "#B41C37"), 
                          domain = overall_merged_data$Count, 
                          na.color = "transparent")
      
      pal_rev <- colorNumeric(palette = rev(c("#044F85", 
                                              "#589BE5", 
                                              "#FFF9CB", 
                                              "#FF8490", 
                                              "#B41C37")), 
                              domain = overall_merged_data$Count, 
                              na.color = "transparent")
      
      leaflet(merged_data) %>%
        addProviderTiles(providers$Esri.WorldTerrain) %>%
        setView(lng = map_view$lng, lat = map_view$lat, zoom = map_view$zoom) %>%
        addPolygons(
          fillColor = ~ifelse(is.na(Count) | Count == 0, "transparent", pal(Count)),
          weight = 1,
          opacity = 1,
          color = "#666666",
          dashArray = "3",
          fillOpacity = ~ifelse(is.na(Count) | Count == 0, 0, 0.7),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666666",
            dashArray = "3",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = ~paste(gis_name, ": ", ifelse(is.na(Count), 0, Count)),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"),
          layerId = ~gis_name
        ) %>%
        addLegend(pal = pal_rev, values = overall_merged_data$Count, opacity = 0.7, title = "Count",
                  position = "bottomright", labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    }
  })
  
  output$dataTable <- renderDT({
    data <- filtered_data()
    
    if (is.null(data)) {
      datatable(data.frame(Message = "No questionnaires available"), 
                options = list(dom = 't'), 
                rownames = FALSE)
    } else {
      table_data <- data$table_data
      
      truncate_string <- function(x) {
        ifelse(nchar(x) > 30, 
               sprintf('<span title="%s">%s...</span>', x, substr(x, 1, 30)), 
               x)
      }
      
      create_hyperlink <- function(url) {
        sprintf('<a href="%s" target="_blank">link</a>', url)
      }
      
      if (nrow(table_data) > 0) {
        table_data$Title <- sapply(table_data$Title, truncate_string)
        table_data$Country <- sapply(table_data$Country, truncate_string)
        table_data$Collections <- sapply(table_data$Collections, truncate_string)
        table_data$URL_Materials <- sapply(table_data$URL_Materials, create_hyperlink)
        
        datatable(table_data[, c("Title", "Country", "Year", "Collections", "URL_Materials")],
                  colnames = c("Title", "Country", "Year", "Collections", "Download"),
                  options = list(pageLength = 8,
                                 scrollX = TRUE,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('th').css('text-align', 'center');",
                                   "$('td').css('text-align', 'center');",
                                   "}"
                                 )),
                  escape = FALSE,
                  rownames = FALSE)
      } else {
        datatable(data.frame(Title = character(0), URL_Materials = character(0)),
                  options = list(pageLength = 8,
                                 scrollX = TRUE,  
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('th').css('text-align', 'center');",
                                   "$('td').css('text-align', 'center');",
                                   "}"
                                 )),
                  escape = FALSE)
      }
    }
  })
  
  observeEvent(input$countryMap_shape_click, {
    event <- input$countryMap_shape_click
    country <- event$id
    if (!is.null(country)) {
     
      normalized_country <- recode(country, !!!country_name_map)
      
      
      this_shape <- reference_table[reference_table$Country == normalized_country,]
      
      if (nrow(this_shape) == 1) {
        leafletProxy("countryMap") %>% 
          setView(
            lng = this_shape$Longitude..average.,
            lat = this_shape$Latitude..average.,
            zoom = 4  
          )
        map_view$lng <- this_shape$Longitude..average.
        map_view$lat <- this_shape$Latitude..average.
        map_view$zoom <- 4
      } else {
        print(paste("Coordinates not found for country:", normalized_country))
      }
      
      selected_country <- input$country
      if (is.null(selected_country)) {
        updateSelectizeInput(session, "country", selected = normalized_country)
      } else if (!(normalized_country %in% selected_country)) {
        updateSelectizeInput(session, "country", selected = c(selected_country, normalized_country))
      }
    }
  })
  
  observe({
    selected_country <- input$country
    if (is.null(selected_country) || length(selected_country) == 0) {
      leafletProxy("countryMap") %>%
        setView(lng = 0, lat = 0, zoom = 1.5)
    } else {
      
      normalized_countries <- recode(selected_country, !!!country_name_map)
      
      # Find the center coordinates of the last selected country
      last_selected_country <- tail(normalized_countries, 1)
      this_shape <- reference_table[reference_table$Country == last_selected_country,]
      
      if (nrow(this_shape) == 1) {
        leafletProxy("countryMap") %>% 
          setView(
            lng = this_shape$Longitude..average.,
            lat = this_shape$Latitude..average.,
            zoom = 4
          )
        map_view$lng <- this_shape$Longitude..average.
        map_view$lat <- this_shape$Latitude..average.
        map_view$zoom <- 4
      } else {
        print(paste("Coordinates not found for country:", last_selected_country))
      }
    }
  })
  
  # Update map view to default when no country is selected
  observe({
    if (is.null(input$country) || length(input$country) == 0) {
      leafletProxy("countryMap") %>%
        setView(lng = 0, lat = 0, zoom = 1.5)
      map_view$lng <- 0
      map_view$lat <- 0
      map_view$zoom <- 1.5
    }
  })
  
  
  
  # Reactive value to store the filtered data for descriptives
  desc_filtered_data <- reactive({
    availability <- input$desc_availability
    match <- input$desc_match
    coverage <- input$desc_coverage
    unit <- input$desc_unit
    
    country <- if ("All" %in% input$desc_country) unique_countries else input$desc_country
    dictionary_p <- if ("All" %in% input$desc_dictionary_p) unique_dictionary_p else input$desc_dictionary_p
    topics <- if ("All" %in% input$desc_topics) unique_topics else input$desc_topics
    region <- if ("All" %in% input$desc_region) unique_regions else input$desc_region
    collections <- if ("All" %in% input$desc_collections) unique_collections else input$desc_collections
    
    aggregate_data(availability, match, coverage, country, dictionary_p, topics, input$desc_year_range, region, collections, unit)
  })
  
  # Dynamic UI for the year slider in descriptives
  output$desc_year_slider <- renderUI({
    sliderInput("desc_year_range", 
                "Select Year Range", 
                min = min_year, 
                max = max_year, 
                value = c(min_year, max_year), 
                step = 1, 
                sep = "")
  })
  
  # Reactive value to store the total count for descriptives
  desc_totalCount <- reactiveVal(0)
  
  observe({
    data <- desc_filtered_data()
    desc_totalCount(if (is.null(data)) 0 else data$unique_ids)
  })
  
  output$desc_totalCount <- renderText({
    desc_totalCount()
  })
  
  output$desc_barChart <- renderPlotly({
    data <- desc_filtered_data()
    
    if (is.null(data)) {
      plot_ly(
        type = 'scatter', 
        mode = 'text',  
        text = "No questionnaires available",
        x = 0.5,
        y = 0.5,
        textfont = list(size = 20),
        showlegend = FALSE
      ) %>%
        layout(
          xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
          yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
          plot_bgcolor = 'rgba(255,255,255,1)',  # Set plot background to white
          paper_bgcolor = 'rgba(255,255,255,1)'  # Set paper background to white
        )
    } else {
      desc_data <- data$table_data
      year_count <- table(desc_data$Year)
      
      plot_ly(x = names(year_count), 
              y = as.numeric(year_count), 
              type = "bar", 
              marker = list(color = "#0072BC")) %>%
        layout(
          title = "",
          xaxis = list(title = ""),
          yaxis = list(title = "<b>Number of questionnaires<b>"),
          barmode = "group"
        )
    }
  })
  
  output$desc_doughnutChart <- renderPlotly({
    data <- desc_filtered_data()
    
    if (is.null(data)) {
      plot_ly(
        type = 'scatter', 
        mode = 'text',  
        text = "No questionnaires available",
        x = 0.5,
        y = 0.5,
        textfont = list(size = 20),
        showlegend = FALSE
      ) %>%
        layout(
          xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
          yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
          plot_bgcolor = 'rgba(255,255,255,1)',  # Set plot background to white
          paper_bgcolor = 'rgba(255,255,255,1)'  # Set paper background to white
        )
    } else {
      match_data <- data$table_data
      match_count <- table(match_data$Match)
      
      # Ensure the labels are in the specified order
      labels <- c("Yes", "No", "Unknown")
      
      # Get the counts for each label, defaulting to 0 if a label is not present
      values <- sapply(labels, function(label) ifelse(label %in% names(match_count), match_count[label], 0))
      
      # Define a color mapping for each label
      color_mapping <- list("Yes" = "#00B398", "No" = "#0072BC", "Unknown" = "#E6E6E6")
      colors <- sapply(labels, function(label) color_mapping[[label]])
      
      plot_ly(labels = ~labels, 
              values = ~values, 
              type = 'pie', 
              hole = 0.6,
              insidetextorientation = 'auto',
              text = values,
              hoverinfo = 'none',
              marker = list(colors = colors)) %>%
        layout(title = list(text = "<b>Representative Sample</b>", x = 0.5), 
               showlegend = TRUE,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
  })
  
  
  output$desc_treemap <- renderPlotly({
    data <- desc_filtered_data()
    
    if (is.null(data)) {
      plot_ly() %>%
        add_annotations(
          type = "scatter",
          mode = "text", 
          text = "No questionnaires available",
          x = 0.5,
          y = 0.5,
          showarrow = FALSE,
          font = list(size = 20)
        ) %>%
        layout(
          xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
          yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE)
        )
    } else {
      topics_data <- data$table_data
      topics_list <- unlist(strsplit(as.character(topics_data$Topics), ", "))
      topics_count <- table(topics_list)
      labels <- names(topics_count)
      values <- as.numeric(topics_count)
      
      plot_ly(
        type = "treemap",
        labels = labels,
        parents = rep("", length(labels)),
        values = values,
        textinfo = "label+value+percent entry",
        marker = list(colors = colorRampPalette(RColorBrewer::brewer.pal(9, "Set3"))(length(labels)))
      ) %>%
        layout(title = "")
    }
  })
  
  output$desc_dataTable <- renderDT({
    data <- desc_filtered_data()
    
    if (is.null(data)) {
      datatable(data.frame(Message = "No questionnaires available"), 
                options = list(dom = 't'), 
                rownames = FALSE)
    } else {
      table_data <- data$table_data
      
      truncate_string <- function(x) {
        ifelse(nchar(x) > 50, 
               sprintf('<span title="%s">%s...</span>', x, substr(x, 1, 50)), 
               x)
      }
      
      create_hyperlink <- function(url) {
        sprintf('<a href="%s" target="_blank">link</a>', url)
      }
      
      if (nrow(table_data) > 0) {
        table_data$Title <- sapply(table_data$Title, truncate_string)
        table_data$Country <- sapply(table_data$Country, truncate_string)
        table_data$Topics <- sapply(table_data$Topics, truncate_string)
        table_data$Collections <- sapply(table_data$Collections, truncate_string)
        table_data$URL_Materials <- sapply(table_data$URL_Materials, create_hyperlink)
        
        datatable(table_data[, c("Title", "Country", "Year", "Topics", "Collections", "URL_Materials")],
                  colnames = c("Title", "Country", "Year", "Topics", "Collections", "Download"),
                  options = list(pageLength = 5,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('th').css('text-align', 'center');",
                                   "$('td').css('text-align', 'center');",
                                   "}"
                                 )),
                  escape = FALSE,
                  rownames = FALSE)
      } else {
        datatable(data.frame(Title = character(0), URL_Materials = character(0)),
                  options = list(pageLength = 5,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('th').css('text-align', 'center');",
                                   "$('td').css('text-align', 'center');",
                                   "}"
                                 )),
                  escape = FALSE)
      }
    }
  })
  
  # Reactive value to store the filtered data for topics
  topics_filtered_data <- reactive({
    availability <- input$topics_availability
    match <- input$topics_match
    coverage <- input$topics_coverage
    unit <- input$topics_unit
    
    country <- if ("All" %in% input$topics_country) unique_countries else input$topics_country
    dictionary_p <- if ("All" %in% input$topics_dictionary_p) unique_dictionary_p else input$topics_dictionary_p
    topics <- if ("All" %in% input$topics_topics) unique_topics else input$topics_topics
    region <- if ("All" %in% input$topics_region) unique_regions else input$topics_region
    collections <- if ("All" %in% input$topics_collections) unique_collections else input$topics_collections
    
    aggregate_data(availability, match, coverage, country, dictionary_p, topics, input$topics_year_range, region, collections, unit)
  })
  
  # Dynamic UI for the year slider in topics
  output$topics_year_slider <- renderUI({
    sliderInput("topics_year_range", "Select Year Range", 
                min = min_year, 
                max = max_year, 
                value = c(min_year, max_year), 
                step = 1, 
                sep = "")
  })
  
  # Reactive value to store the total count for topics
  topics_totalCount <- reactiveVal(0)
  
  observe({
    data <- topics_filtered_data()
    topics_totalCount(if (is.null(data)) 0 else data$unique_ids)
  })
  
  output$topics_totalCount <- renderText({
    topics_totalCount()
  })
  
  output$topics_treemap <- renderPlotly({
    data <- topics_filtered_data()
    
    if (is.null(data) || nrow(data$table_data) == 0) {
      plot_ly(
        type = 'scatter',  
        mode = 'text',  
        x = 0.5,
        y = 0.5,
        text = "No questionnaires available",
        textfont = list(size = 20),
        showlegend = FALSE
      ) %>%
        layout(
          xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
          yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
          plot_bgcolor = 'rgba(255,255,255,1)',  # Set plot background to white
          paper_bgcolor = 'rgba(255,255,255,1)'  # Set paper background to white
        )
    } else {
      topics_data <- data$table_data
      topics_list <- unlist(strsplit(as.character(topics_data$Topics), ", "))
      topics_count <- table(topics_list)
      labels <- names(topics_count)
      values <- as.numeric(topics_count)
      
      plot_ly(
        type = "treemap",
        labels = labels,
        parents = rep("", length(labels)),
        values = values,
        textinfo = "label+value+percent entry",
        marker = list(colors = colorRampPalette(RColorBrewer::brewer.pal(9, "Set3"))(length(labels)))
      ) %>%
        layout(title = "")
    }
  })
  
  output$topics_dataTable <- renderDT({
    data <- topics_filtered_data()
    
    if (is.null(data)) {
      datatable(data.frame(Message = "No questionnaires available"), 
                options = list(dom = 't'), 
                rownames = FALSE)
    } else {
      table_data <- data$table_data
      
      truncate_string <- function(x) {
        ifelse(nchar(x) > 40, 
               sprintf('<span title="%s">%s...</span>', x, substr(x, 1, 40)), 
               x)
      }
      
      create_hyperlink <- function(url) {
        sprintf('<a href="%s" target="_blank">link</a>', url)
      }
      
      if (nrow(table_data) > 0) {
        table_data$Title <- sapply(table_data$Title, truncate_string)
        table_data$Country <- sapply(table_data$Country, truncate_string)
        table_data$Topics <- sapply(table_data$Topics, truncate_string)
        table_data$Collections <- sapply(table_data$Collections, truncate_string)
        table_data$URL_Materials <- sapply(table_data$URL_Materials, create_hyperlink)
        
        datatable(table_data[, c("Title", "Country", "Year", "Topics", "Collections", "URL_Materials")],
                  colnames = c("Title", "Country", "Year", "Topics", "Collections", "Download"),
                  options = list(pageLength = 5,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('th').css('text-align', 'center');",
                                   "$('td').css('text-align', 'center');",
                                   "}"
                                 )),
                  escape = FALSE,
                  rownames = FALSE)
      } else {
        datatable(data.frame(Title = character(0), URL_Materials = character(0)),
                  options = list(pageLength = 5,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('th').css('text-align', 'center');",
                                   "$('td').css('text-align', 'center');",
                                   "}"
                                 )),
                  escape = FALSE)
      }
    }
  })
  
  # Reactive value to store the filtered data for more topics
  more_topics_filtered_data <- reactive({
    availability <- input$more_topics_availability
    match <- input$more_topics_match
    coverage <- input$more_topics_coverage
    unit <- input$more_topics_unit
    
    country <- if ("All" %in% input$more_topics_country) unique_countries else input$more_topics_country
    dictionary_p <- if ("All" %in% input$more_topics_dictionary_p) unique_dictionary_p else input$more_topics_dictionary_p
    topics <- if ("All" %in% input$more_topics_topics) unique_topics else input$more_topics_topics
    region <- if ("All" %in% input$more_topics_region) unique_regions else input$more_topics_region
    collections <- if ("All" %in% input$more_topics_collections) unique_collections else input$more_topics_collections
    
    aggregate_data(availability, match, coverage, country, dictionary_p, topics, input$more_topics_year_range, region, collections, unit)
  })
  
  # Dynamic UI for the year slider in more topics
  output$more_topics_year_slider <- renderUI({
    sliderInput("more_topics_year_range", 
                "Select Year Range", 
                min = min_year, 
                max = max_year, 
                value = c(min_year, max_year), 
                step = 1, 
                sep = "")
  })
  
  # Reactive value to store the filtered co-occurrence matrix
  filtered_co_occurrence_matrix <- reactive({
    data <- more_topics_filtered_data()$table_data
    co_occurrence_matrix <- matrix(0, nrow = length(unique_topics), ncol = length(unique_topics))
    rownames(co_occurrence_matrix) <- unique_topics
    colnames(co_occurrence_matrix) <- unique_topics
    
    for (topics in data$Topics) {
      topic_list <- unlist(strsplit(as.character(topics), ", "))
      for (i in 1:length(topic_list)) {
        for (j in 1:length(topic_list)) {
          if (i != j && topic_list[i] %in% unique_topics && topic_list[j] %in% unique_topics) {
            co_occurrence_matrix[topic_list[i], topic_list[j]] <- co_occurrence_matrix[topic_list[i], topic_list[j]] + 1
          }
        }
      }
    }
    co_occurrence_matrix[co_occurrence_matrix <= 2] <- NA  # Filter to keep only co-occurrences greater than 2
    
    # Mask the upper triangle of the matrix, including the diagonal
    co_occurrence_matrix[upper.tri(co_occurrence_matrix, diag = TRUE)] <- NA
    co_occurrence_matrix
  })
  
  output$heatmap <- renderPlotly({
    co_occurrence_matrix <- filtered_co_occurrence_matrix()
    
    if (all(is.na(co_occurrence_matrix))) {
      # Show "No questionnaires available" message
      plot_ly(
        type = 'scatter',
        mode = 'text',
        text = "No questionnaires available",
        x = 0.5,
        y = 0.5,
        textfont = list(size = 20),
        showlegend = FALSE
      ) %>%
        layout(
          xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
          yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
          plot_bgcolor = 'rgba(255,255,255,1)',
          paper_bgcolor = 'rgba(255,255,255,1)'
        )
    } else {
      # Filter rows and columns to only show those with non-NA values
      valid_indices <- which(rowSums(!is.na(co_occurrence_matrix)) > 0 & colSums(!is.na(co_occurrence_matrix)) > 0)
      filtered_topics <- unique_topics[valid_indices]
      filtered_matrix <- co_occurrence_matrix[valid_indices, valid_indices]
      
      # Ensure the filtered matrix is not empty or invalid
      if (length(valid_indices) == 0 || all(is.na(filtered_matrix))) {
        # Show "No valid co-occurrences available" message if no valid indices exist
        plot_ly(
          type = 'scatter',
          mode = 'text',
          text = "No valid co-occurrences available",
          x = 0.5,
          y = 0.5,
          textfont = list(size = 20),
          showlegend = FALSE
        ) %>%
          layout(
            xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
            yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
            plot_bgcolor = 'rgba(255,255,255,1)',
            paper_bgcolor = 'rgba(255,255,255,1)'
          )
      } else {
        # Prepare hover info, set to "none" for upper triangle
        hoverinfo_matrix <- matrix("none", nrow = nrow(filtered_matrix), ncol = ncol(filtered_matrix))
        hoverinfo_matrix[lower.tri(hoverinfo_matrix, diag = TRUE)] <- "x+y+z"
        
        # Check for min/max values to ensure they are not Inf/-Inf
        matrix_min <- min(filtered_matrix, na.rm = TRUE)
        matrix_max <- max(filtered_matrix, na.rm = TRUE)
        
        # If matrix is entirely 0, avoid passing Inf to the color scale
        if (matrix_min == Inf || matrix_max == -Inf || matrix_min == matrix_max) {
          plot_ly(
            type = 'scatter',
            mode = 'text',
            text = "No valid data available",
            x = 0.5,
            y = 0.5,
            textfont = list(size = 20),
            showlegend = FALSE
          ) %>%
            layout(
              xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
              yaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
              plot_bgcolor = 'rgba(255,255,255,1)',
              paper_bgcolor = 'rgba(255,255,255,1)'
            )
        } else {
          # Proceed with heatmap rendering if valid numeric data exists
          plot_ly(
            z = filtered_matrix, 
            x = filtered_topics, 
            y = filtered_topics, 
            type = "heatmap",
            colors = colorRamp(c("white", "red")),
            showscale = TRUE,
            hoverinfo = hoverinfo_matrix,
            xgap = 3,
            ygap = 3,
            width = 1200,
            height = 1200
          ) %>%
            layout(
              title = "",
              xaxis = list(
                title = "",
                tickvals = 0:(length(filtered_topics) - 1),
                ticktext = filtered_topics,
                automargin = TRUE,
                tickangle = 45,
                side = "top",
                showgrid = FALSE,
                zeroline = FALSE,
                gridcolor = 'rgba(0,0,0,0)',
                backgroundcolor = 'rgba(0,0,0,0)'
              ),
              yaxis = list(
                title = "",
                tickvals = 0:(length(filtered_topics) - 1),
                ticktext = filtered_topics,
                automargin = TRUE,
                tickangle = 0,
                side = "left",
                showgrid = FALSE,
                zeroline = FALSE,
                gridcolor = 'rgba(0,0,0,0)',
                backgroundcolor = 'rgba(0,0,0,0)'
              ),
              plot_bgcolor = 'rgba(0,0,0,0)',
              paper_bgcolor = 'rgba(0,0,0,0)',
              margin = list(l = 50, b = 100)
            )
        }
      }
    }
  })
  
  
  output$more_topics_dataTable <- renderDT({
    data <- more_topics_filtered_data()
    
    if (is.null(data) || nrow(data$table_data) == 0) {
      datatable(data.frame(Message = "No questionnaires available"), 
                options = list(dom = 't'), 
                rownames = FALSE)
    } else {
      table_data <- data$table_data
      
      truncate_string <- function(x) {
        ifelse(nchar(x) > 40, 
               sprintf('<span title="%s">%s...</span>', x, substr(x, 1, 40)), 
               x)
      }
      
      create_hyperlink <- function(url) {
        sprintf('<a href="%s" target="_blank">link</a>', url)
      }
      
      if (nrow(table_data) > 0) {
        table_data$Title <- sapply(table_data$Title, truncate_string)
        table_data$Country <- sapply(table_data$Country, truncate_string)
        table_data$Topics <- sapply(table_data$Topics, truncate_string)
        table_data$Topics <- sapply(table_data$Collections, truncate_string)
        table_data$URL_Materials <- sapply(table_data$URL_Materials, create_hyperlink)
        
        datatable(table_data[, c("Title", "Country", "Year", "Topics", "Collections", "URL_Materials")],
                  colnames = c("Title", "Country", "Year", "Topics", "Collections", "Download"),
                  options = list(pageLength = 5,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('th').css('text-align', 'center');",
                                   "$('td').css('text-align', 'center');",
                                   "}"
                                 )),
                  escape = FALSE,
                  rownames = FALSE)
      } else {
        datatable(data.frame(Title = character(0), URL_Materials = character(0)),
                  options = list(pageLength = 5,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('th').css('text-align', 'center');",
                                   "$('td').css('text-align', 'center');",
                                   "}"
                                 )),
                  escape = FALSE)
      }
    }
  })
  
  
  
  # EGRISS
  egriss_filtered_data <- reactive({
   
    availability <- if (is.null(input$availability_egriss) || "All" %in% input$availability_egriss) NULL else input$availability_egriss
    match <- if (is.null(input$match_egriss) || "All" %in% input$match_egriss) NULL else input$match_egriss
    coverage <- if (is.null(input$coverage_egriss) || "All" %in% input$coverage_egriss) NULL else input$coverage_egriss
    unit <- if (is.null(input$unit_egriss) || "All" %in% input$unit_egriss) NULL else input$unit_egriss
    
    country <- if (is.null(input$country_egriss) || "All" %in% input$country_egriss) NULL else input$country_egriss
    dictionary_p <- if (is.null(input$dictionary_p_egriss) || "All" %in% input$dictionary_p_egriss) NULL else input$dictionary_p_egriss
    topics <- if (is.null(input$topics_egriss) || "All" %in% input$topics_egriss) NULL else input$topics_egriss
    region <- if (is.null(input$region_egriss) || "All" %in% input$region_egriss) NULL else input$region_egriss
    collections <- if (is.null(input$collections_egriss) || "All" %in% input$collections_egriss) NULL else input$collections_egriss
    
    
    idp_count <- if (is.null(input$idp_questions_filter) || "All" %in% input$idp_questions_filter) NULL else as.numeric(input$idp_questions_filter)
    refugee_count <- if (is.null(input$refugee_questions_filter) || "All" %in% input$refugee_questions_filter) NULL else as.numeric(input$refugee_questions_filter)
    
    
    selected_idp_questions <- input$idp_questions_checkboxes
    selected_refugee_questions <- input$refugee_questions_checkboxes
    
    
    filtered_data <- aggregate_data(
      availability = availability,
      match = match,
      coverage = coverage,
      country = country,
      dictionary_p = dictionary_p,
      topics = topics,
      year_range = input$year_range,  
      region = region,
      collections = collections,
      unit = unit
    )
    
    
    if (!is.null(filtered_data$table_data)) {
      filtered_data$table_data <- filtered_data$table_data %>%
        left_join(CORPUS_Questions %>% select(ID, IDP_Questions, Refugee_Questions), by = "ID")
      
      
      
      if (!is.null(selected_idp_questions)) {
        filtered_data$table_data <- filtered_data$table_data %>%
          filter(sapply(IDP_Questions, function(q) all(selected_idp_questions %in% strsplit(q, ",\\s*")[[1]])))
      }
      
      
      if (!is.null(selected_refugee_questions)) {
        filtered_data$table_data <- filtered_data$table_data %>%
          filter(sapply(Refugee_Questions, function(q) all(selected_refugee_questions %in% strsplit(q, ",\\s*")[[1]])))
      }
    } else {
      
      filtered_data$table_data <- CORPUS_Questions
    }
    
    return(filtered_data)
  })
  
  
  
  # Render the data table with filtered data
  output$egriss_dataTable <- renderDT({
    data <- egriss_filtered_data()
    
    if (is.null(data) || nrow(data$table_data) == 0) {
      datatable(data.frame(Message = "No questionnaires available"),
                options = list(dom = 't'),
                rownames = FALSE)
    } else {
      table_data <- data$table_data
      
      
      truncate_string <- function(x) {
        ifelse(nchar(x) > 30, 
               sprintf('<span title="%s">%s...</span>', x, substr(x, 1, 30)), 
               x)
      }
      
      create_hyperlink <- function(url) {
        sprintf('<a href="%s" target="_blank">link</a>', url)
      }
      
      if (nrow(table_data) > 0) {
        # Apply the truncation and hyperlink formatting
        table_data$Title <- sapply(table_data$Title, truncate_string)
        table_data$Country <- sapply(table_data$Country, truncate_string)
        table_data$Collections <- sapply(table_data$Collections, truncate_string)
        table_data$URL_Materials <- sapply(table_data$URL_Materials, create_hyperlink)
        
        datatable(table_data[, c("Title", "Country", "Year", "Collections", "URL_Materials")],
                  colnames = c("Title", "Country", "Year", "Collections", "Download"),
                  options = list(pageLength = 20,
                                 scrollX = TRUE,
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('th').css('text-align', 'center');",
                                   "$('td').css('text-align', 'center');",
                                   "}"
                                 )),
                  escape = FALSE,  
                  rownames = FALSE)
      } else {
        datatable(data.frame(Title = character(0), URL_Materials = character(0)),
                  options = list(pageLength = 20,
                                 scrollX = TRUE,  
                                 initComplete = JS(
                                   "function(settings, json) {",
                                   "$('th').css('text-align', 'center');",
                                   "$('td').css('text-align', 'center');",
                                   "}"
                                 )),
                  escape = FALSE)
      }
    }
  })
  
  # Render the year slider UI
  output$year_slider_egriss <- renderUI({
    # Assuming MDL has a Year column
    min_year <- min(MDL$Year, na.rm = TRUE)
    max_year <- max(MDL$Year, na.rm = TRUE)
    
    sliderInput("year_range", 
                "Select Year Range", 
                min = min_year, 
                max = max_year, 
                value = c(min_year, max_year), 
                step = 1, 
                sep = "")
  })
  
  
  
  # Word Search
  output$word_search_dataTable <- renderDT({
    # Function to truncate strings
    truncate_string <- function(x, length = 40) {
      ifelse(nchar(x) > length, 
             sprintf('<span title="%s">%s...</span>', x, substr(x, 1, length)), 
             x)
    }
    
    # Function to create hyperlinks
    create_hyperlink <- function(url) {
      sprintf('<a href="%s" target="_blank">link</a>', url)
    }
    
    
    match_words <- function(text, words) {
      
      word_list <- unlist(strsplit(words, "\\s+"))
      
      
      found_words <- sapply(word_list, function(word) {
        grepl(paste0("\\b", word, "\\b"), text, ignore.case = TRUE)
      })
      
      
      return(all(found_words))
    }
    
    # Filter MDL based on search input
    search_input <- input$word_search_input
    
    filtered_MDL <- if (!is.null(search_input) && search_input != "") {
      
      MDL[sapply(MDL$Text, function(text) {
        match_words(text, search_input)
      }), ]
    } else {
      MDL
    }
    
    
    filtered_MDL$Title <- sapply(filtered_MDL$Title, function(x) truncate_string(x, 40))
    filtered_MDL$Country <- sapply(filtered_MDL$Country, function(x) truncate_string(x, 40))
    filtered_MDL$Topics <- sapply(filtered_MDL$Topics, function(x) truncate_string(x, 15))
    filtered_MDL$Region <- sapply(filtered_MDL$Region, function(x) truncate_string(x, 20))
    filtered_MDL$Collections <- sapply(filtered_MDL$Collections, function(x) truncate_string(x, 15))
    filtered_MDL$URL_Materials <- sapply(filtered_MDL$URL_Materials, create_hyperlink)
    
    datatable(
      filtered_MDL[, c("Title", "Country", "Region", "Year", "Collections", "Match", "Topics", "Population", "Availability_EN", "URL_Materials")],
      colnames = c("Title", "Country", "Region", "Year", "Collections", "Representative Sample", "Topics", "Population Group", "Language", "Information & Download page"),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100, -1),
        dom = 'rtip',  
        autoWidth = FALSE,
        initComplete = JS(
          "function(settings, json) {",
          "$('input[type=search]').css({'width':'100%'});",
          "$('.dataTables_filter input').attr({'placeholder':''});",
          "$('table.dataTable').css('table-layout', 'fixed');",
          "$('th').css('text-align', 'center');",
          "$('td').css('text-align', 'center');",
          "}"
        )
      ),
      filter = 'top',
      rownames = FALSE,
      escape = FALSE
    )
  })
  
  
  observeEvent(input$show_search_info, {
    showModal(modalDialog(
      title = tags$strong("About the Words Search Table"),
      tags$p("The Words Search function allows you to type in any words present in the questionnaires. It matches whole words regardless of case and enables you to search for multiple words that should appear together in any order."),
      tags$p("For example, if you type the words  \"border\" and \"food\", the table will return all the questionnaires that contain both words simultaneously."),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  
  
  
  
  output$descriptive_barChart <- renderPlotly({
    desc_var <- input$desc_var
    desc_data <- MDL
    
    if (desc_var == "Representative Sample") {
      desc_var <- "Match"
    } else if (desc_var == "Population Group") {
      desc_var <- "Population"
    } else if (desc_var == "Unit of Analysis") {
      desc_var <- "Unit"
    }
    
    if (desc_var == "Country" || desc_var == "Topics" || desc_var == "Population" || desc_var == "Region"|| desc_var == "Collections") {
      values_list <- unlist(strsplit(as.character(desc_data[[desc_var]]), ", "))
      desc_data <- data.frame(Value = values_list)
      desc_var <- "Value"
    }
    
    var_count <- table(desc_data[[desc_var]])
    
    plot_ly(
      x = names(var_count),
      y = as.numeric(var_count),
      type = "bar",
      marker = list(color = "#00B398")
    ) %>%
      layout(
        title = "",
        xaxis = list(title = ""),
        yaxis = list(title = "Number of questionnaires"),
        barmode = "group"
      )
  })
  
  output$databaseTable <- renderDT({
    truncate_string <- function(x, length = 40) {
      ifelse(nchar(x) > length, 
             sprintf('<span title="%s">%s...</span>', x, substr(x, 1, length)), 
             x)
    }
    
    create_hyperlink <- function(url) {
      sprintf('<a href="%s" target="_blank">link</a>', url)
    }
    
    MDL$Title <- sapply(MDL$Title, function(x) truncate_string(x, 40))
    MDL$Country <- sapply(MDL$Country, function(x) truncate_string(x, 40))
    MDL$Topics <- sapply(MDL$Topics, function(x) truncate_string(x, 15))
    MDL$Region <- sapply(MDL$Region, function(x) truncate_string(x, 20))
    MDL$Collections <- sapply(MDL$Collections, function(x) truncate_string(x, 15))
    MDL$Unit <- sapply(MDL$Unit, function(x) truncate_string(x, 15))
    MDL$URL_Materials <- sapply(MDL$URL_Materials, create_hyperlink)
    
    datatable(
      MDL[, c("Title", "Country", "Region", "Year", "Collections", "Match", "Topics", "Population", "Unit", "Availability_EN", "URL_Materials")],
      colnames = c("Title", "Country", "Region", "Year", "Collections", "Representative Sample", "Topics", "Population Group", "Unit of Analysis", "Language", "Information & Download page"),
      options = list(
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100, -1),
        dom = 'frtip',  
        autoWidth = FALSE,
        
        initComplete = JS(
          "function(settings, json) {",
          "$('input[type=search]').css({'width':'100%'});",
          "$('.dataTables_filter input').attr({'placeholder':''});",
          "$('table.dataTable').css('table-layout', 'fixed');",
          "$('th').css('text-align', 'center');",
          "$('td').css('text-align', 'center');",
          "}"
        )
      ),
      filter = 'top',
      rownames = FALSE,
      escape = FALSE
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("MDL_Dataset", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(MDL, file, row.names = FALSE)
    }
  )
  
  
  
  observeEvent(input$show_help, {
    showModal(modalDialog(
      title = tags$strong("ABOUT & HOW TO USE THE DASHBOARD"),
      tags$div(
        class = "modal-body",
        tags$h3("Aim", style = "font-weight: bold; color: #0072BC;"),
        tags$p("The", tags$strong("Questionnaire Finder"), "is an interactive tool that allows users to explore datasets with available questionnaires from the UNHCR Microdata Library. This tool enables users to search questionnaires by various criteria, including country, region, population group, topic, representative sample, geographical coverage, collection, and language. The goal is to enhance the discoverability and accessibility of UNHCR Microdata Library questionnaires among global communities of practice, thereby facilitating their reuse and adaptation by governments and partner organizations conducting similar studies."),
        tags$h3("Structure", style = "font-weight: bold; color: #0072BC;"),
        tags$p("At the top of the website, you will find a clear and concise navigation menu, allowing you to switch between different sections:"),
        tags$h4("1. Dashboard", style = "font-weight: bold; color: #0072BC;"),
        tags$p("Your main interface to overview and interact with the data. It is formed by three main components:"),
        tags$ul(
          tags$li(tagList(tags$strong("Filters:"), " A list of filter selections to help you find exactly what you need. Choose any of the following options: Country, Topics, Population Group, Representative Sample, Language, and Year. Each field presents a dropdown list where you can select the variables or type to search.")),
          tags$li(tagList(tags$strong("Interactive Map:"), " An interactive world map colour-coded by the number of questionnaires available for each country. You can click on any country to view detailed information or use the Country filter to achieve the same result.")),
          tags$li(tagList(tags$strong("Data Table:"), " A searchable and sortable table displaying information and download links for each questionnaire. It will be updated according to the filters applied and/or countries selected on the map."))
        ),
        tags$h4("2. Explore", style = "font-weight: bold; color: #0072BC;"),
        tags$p(tagList(
          "Delve deeper into the data with ", 
          tags$strong("filters,"),  
          tags$strong("data table"), " and ", 
          tags$strong("charts."), "It is further divided into three sections:"
        )),
        tags$ul(
          tags$li(tagList(tags$strong("Representativeness:"), " Understand the distribution of representative samples by year.")),
          tags$li(tagList(tags$strong("Topic Explorer:"), " A treemap visualization to see the prevalence of various topics within the questionnaires.")),
          tags$li(tagList(tags$strong("Topic Co-occurrence:"), " Analyze how different topics are interrelated through a heatmap."))
        ),
        tags$h4("3. EGRISS-related Language", style = "font-weight: bold; color: #0072BC;"),
        tags$p("Identify and filter questionnaires that contain EGRISS-related language for household surveys, specifically for identifying IDPs and refugees."),
        tags$h4("4. Words Search", style = "font-weight: bold; color: #0072BC;"),
        tags$p("Type in any words present in the questionnaires. It matches whole words regardless of case and enables you to search for multiple words that should appear together in any order."),
        tags$h4("5. Summary Statistics", style = "font-weight: bold; color: #0072BC;"),
        tags$p("Explore high-level statistics and trends about the questionnaires by selecting a variable."),
        tags$h4("6. Database", style = "font-weight: bold; color: #0072BC;"),
        tags$p("Access the full dataset and utilize search options to find the data."),
        tags$h4("7. Methodology", style = "font-weight: bold; color: #0072BC;"),
        tags$p("Learn about the methodologies behind the data collection."),
        tags$h4("8. Code", style = "font-weight: bold; color: #0072BC;"),
        tags$p("Access code used for the development of the dashboard.")
      ),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  
  observeEvent(input$show_treemap_info, {
    showModal(modalDialog(
      title = tags$strong("About the Treemap"),
      tags$p("One survey can cover multiple topics. If the treemap shows more topics than you selected in your filters, it means those additional topics are present in surveys that include your filtered topics."),
      tags$p("For example, if you select Health and Nutrition as filters, the treemap will display the number of questionnaires containing both Health and Nutrition together. Additionally, it may show other topics like Livelihood. The number associated with Livelihood represents the questionnaires that include Health, Nutrition, and Livelihood together."),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  observeEvent(input$show_heatmap_info, {
    showModal(modalDialog(
      title = tags$strong("About the Heatmap"),
      tags$p("One survey can cover multiple topics. If the heatmap shows more topics than you selected in your filters, it means those additional topics are present in surveys that include your filtered topics."),
      tags$p("For example, if you select Health and Nutrition as filters, the heatmap will display the number of questionnaires containing both Health and Nutrition together. Additionally, it may show other topics like Livelihood. The number associated with Livelihood represents the questionnaires that include Health, Nutrition, and Livelihood together."),
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    ))
  })
  
  
  
  

  
  
  # Methodology Table
  
  output$duplicate_table <- renderTable({
    data.frame(
      Title = c("Livelihoods Programme Monitoring Beneficiary Survey", 
                "Joint Protection Monitoring", 
                "Energy Monitoring Framework Survey", 
                "Livelihoods Programme Monitoring Beneficiary Survey", 
                "Voluntary Repatriation"),
      `Questionnaire File Name` = c("LIS_questionnaire", 
                                    "codebook (contains also the questionnaire)", 
                                    "EIS_questionnaire", 
                                    "Beneficiary_Survey_V6_English", 
                                    "UNHCR_VolRep_AFG_questionnaire"),
      Number = c(126L, 47L, 36L, 13L, 10L),
      check.names = FALSE
    )
  })
  
  
  
  
}


# Run
shinyApp(ui, server)

