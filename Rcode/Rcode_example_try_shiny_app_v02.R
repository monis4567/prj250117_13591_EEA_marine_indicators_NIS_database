# install.packages("shiny")
# install.packages("worrms")

library(shiny)
library(worrms)

# Define the marine regions vector
marine_regions <- c(
  "North-East Atlantic Ocean",
  "Baltic Sea",
  "Mediterranean Sea",
  "Black Sea",
  "Norwegian Sea",
  "Barents Sea",
  "North Sea",
  "Icelandic Waters"
)

ui <- fluidPage(
  titlePanel("WoRMS AphiaID Finder"),
  sidebarLayout(
    sidebarPanel(
      textInput("species", "Enter Latin species name:", value = "Crassostrea gigas"),
      numericInput("year", "Enter year found:", value = 2024, min = 1800, max = as.numeric(format(Sys.Date(), "%Y"))),
      selectInput("region", "Select marine region:", choices = marine_regions),
      actionButton("search", "Search")
    ),
    mainPanel(
      verbatimTextOutput("aphia_id"),
      tableOutput("record_table")
    )
  )
)

server <- function(input, output, session) {
  # Reactive value to store the data frame
  records <- reactiveVal(data.frame(
    EnteredSpecies = character(),
    ValidSpecies = character(),
    ValidAphiaID = numeric(),
    YearFound = numeric(),
    MarineRegion = character(),
    stringsAsFactors = FALSE
  ))
  
  result <- eventReactive(input$search, {
    species_name <- input$species
    year_found <- input$year
    region <- input$region
    
    if (nchar(species_name) == 0) return("Please enter a species name.")
    
    tryCatch({
      match <- wm_records_name(name = species_name)
      if (length(match) == 0) {
        return("No match found.")
      } else {
        # Store the results in the records data frame
        new_record <- data.frame(
          EnteredSpecies = species_name,
          ValidSpecies = match$valid_name,
          ValidAphiaID = match$valid_AphiaID,
          YearFound = year_found,
          MarineRegion = region,
          stringsAsFactors = FALSE
        )
        updated <- rbind(records(), new_record)
        records(updated)
        
        return(paste(
          "AphiaID:", match$AphiaID, "\n",
          "Valid name:", match$valid_name, "\n",
          "Valid AphiaID:", match$valid_AphiaID, "\n",
          "Year found:", year_found, "\n",
          "Marine region:", region
        ))
      }
    }, error = function(e) {
      return(paste("Error:", e$message))
    })
  })
  
  output$aphia_id <- renderText({
    result()
  })
  
  output$record_table <- renderTable({
    records()
  })
}

shinyApp(ui = ui, server = server)
