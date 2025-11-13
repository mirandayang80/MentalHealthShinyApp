library(shiny)
library(bslib)
library(markdown)
library(leaflet)
library(sf)
library(ggplot2)
library(plotly)
library(tidyverse)
library(shinythemes)

# This code added by Maxine
# Data sets of average academic adjustment for each degree (including any
# undergraduate degree) and for academic adjustment and mental health by year
# are loaded in
avg_adjust_aca_undergrad <-
  readRDS("final_wrangled_data/avg_adjust_aca_undergrad.rds")
avg_adjust_aca_assoc <-
  readRDS("final_wrangled_data/avg_adjust_aca_assoc.rds")
avg_adjust_aca_bach <-
  readRDS("final_wrangled_data/avg_adjust_aca_bach.rds")
prop_yr_all <-
  readRDS("final_wrangled_data/prop_yr_all.rds")

# Added by Miranda
# read in us states shape file
states_sf <- read_sf("cb_2018_us_state_500k.shp") %>%
  # solves sf layer has inconsistent datum issue
  sf::st_transform('+proj=longlat +datum=WGS84')
combinedgeo <- readRDS("final_wrangled_data/combinedgeo.rds")
combinedr1_ease <- readRDS("final_wrangled_data/combinedr1_ease.rds")
combinedr2_ease <- readRDS("final_wrangled_data/combinedr2_ease.rds")
combinedr3_ease <- readRDS("final_wrangled_data/combinedr3_ease.rds")
combinedr4_ease <- readRDS("final_wrangled_data/combinedr4_ease.rds")
combinedr5_ease <- readRDS("final_wrangled_data/combinedr5_ease.rds")
combinedr6_ease <- readRDS("final_wrangled_data/combinedr6_ease.rds")
combinedr7_ease <- readRDS("final_wrangled_data/combinedr7_ease.rds")
combinedr8_ease <- readRDS("final_wrangled_data/combinedr8_ease.rds")
combinedr9_ease <- readRDS("final_wrangled_data/combinedr9_ease.rds")
combinedr1_anx <- readRDS("final_wrangled_data/combinedr1_anx.rds")
combinedr2_anx <- readRDS("final_wrangled_data/combinedr2_anx.rds")
combinedr3_anx <- readRDS("final_wrangled_data/combinedr3_anx.rds")
combinedr4_anx <- readRDS("final_wrangled_data/combinedr4_anx.rds")
combinedr5_anx <- readRDS("final_wrangled_data/combinedr5_anx.rds")
combinedr6_anx <- readRDS("final_wrangled_data/combinedr6_anx.rds")
combinedr7_anx <- readRDS("final_wrangled_data/combinedr7_anx.rds")
combinedr8_anx <- readRDS("final_wrangled_data/combinedr8_anx.rds")
combinedr9_anx <- readRDS("final_wrangled_data/combinedr9_anx.rds")
combinedr1_dep <- readRDS("final_wrangled_data/combinedr1_dep.rds")
combinedr2_dep <- readRDS("final_wrangled_data/combinedr2_dep.rds")
combinedr3_dep <- readRDS("final_wrangled_data/combinedr3_dep.rds")
combinedr4_dep <- readRDS("final_wrangled_data/combinedr4_dep.rds")
combinedr5_dep <- readRDS("final_wrangled_data/combinedr5_dep.rds")
combinedr6_dep <- readRDS("final_wrangled_data/combinedr6_dep.rds")
combinedr7_dep <- readRDS("final_wrangled_data/combinedr7_dep.rds")
combinedr8_dep <- readRDS("final_wrangled_data/combinedr8_dep.rds")
combinedr9_dep <- readRDS("final_wrangled_data/combinedr9_dep.rds")

# Define UI ----

# code added by Miranda unless otherwise specified
# specifiy ui
ui <- page_fluid(
  # specify theme
  theme = shinytheme("flatly"),
  navbarPage(
    "Mental Health of Undergrad Students",
    
    # first tab panel - scatterplot
    tabPanel(
      # Name of tab
      "Academic Adjustment",
      
      # Side bar layout with drop down menu and radio buttons
      sidebarLayout(
        sidebarPanel(
          # Text written by Maxine
          h5(
            "This plot shows the relationship between anxiety or depression",
            "symptoms and the average academic adjustment score (a measure of",
            "difficulty) among college students reporting each anxiety or",
            "depression score, recorded from 2016 to 2025."
          ),
          
          # Radio buttons
          radioButtons(
            inputId = "degType",
            label = "Degree Type",
            choices = c(
              "Associate's",
              "Bachelor's",
              "All Undergraduate"
            ),
            select = "All Undergraduate"
          ),
          
          # Dropdown box (from Maxine)
          selectInput("condition", "Mental Condition:", 
                      c("Anxiety", "Depression"))
        ),
        
        # main panel showing scatterplot, using Plotly
        mainPanel(
          plotlyOutput("plot")
        )
      ),
    ),
    
    
    # second tab panel - bar graph
    tabPanel(
      # name of tab
      "Across Years",
      
      # side bar layout
      sidebarLayout(
        sidebarPanel(
          # Text added by Maxine based on text by Miranda (plot 3)
          h5(
            "The plot displays the percentage of students over time who",
            "answered adjusting to academic demands is easy, reported",
            "a depression diagnosis, or reported an anxiety diagnosis, based",
            "on user input."
          ),
          
          # Dropdown box
          selectInput( # Added by Henry
            inputId = "g2_explanatory",
            label = "Select Explanatory Variable",
            choices = c("Anxiety", "Depression", "Ease of Adjustment")
          ),
          # Radio Buttons
          radioButtons(
            inputId = "g2_degType",
            label = "Degree Type",
            choices = c(
              "Associate's" = "g2_a",
              "Bachelor's" = "g2_b",
              "Both" = "g2_all"
            )
          ),
          # slider
          sliderInput( # Added by Henry
            inputId = "g2_yr",
            # title for slider
            label = "Select Year",
            # min year
            min = 2007,
            # max year
            max = 2025,
            # positions of years
            value = c(2007, 2025),
            # not include comma as separator for year
            sep = ""
          )
        ),
        mainPanel(plotOutput("bar"))
      )
    ),
    
    # Added by Miranda
    # third tab panel - leaflet
    tabPanel(title = "Map", sidebarLayout(
      sidebarPanel(
        h5(
          "By region, proportion of students who",
          br(),
          "(1) answered adjusting to academic demands is easy",
          br(),
          "(2) are diagnosed with anxiety",
          br(),
          "(3) are diagnosed with depression",
          br(),
          br(),
          "Note:",
          br(),
          "- Please select only one variable at a time to prevent overlaps",
          br(),
          "- For some years, some regions don't have data"
        ),
        
        # slider
        sliderInput(
          inputId = "map_yr",
          # title for slider
          label = "Select Year",
          # min year
          min = 2018,
          # max year
          max = 2025,
          # start position of year
          value = 2018,
          
          # not include comma as separator for year
          sep = ""
        )
      ), mainPanel(
        # plot leaflet onto main panel
        card(leafletOutput("map")),
      )
    )),
    
    # Added by Miranda
    # fourth - about data
    navbarMenu("More", tabPanel(
      title = "About our data", mainPanel(
        # added by Maxine
        h5(
          "The data comes from the Healthy Minds Study among Colleges and",
          "Universities, conducted by the Healthy Minds Network. The study",
          "asks students across the United States to self report their",
          "mental health symptoms and status, along with struggles they may",
          "be facing and other demographic factors.",
          br(),
          br(),
          # data citation
          "Citation: Healthy Minds Network (2007-2025). Healthy Minds Study 
          among Colleges and Universities, 2007-2025 datasets. Healthy Minds
          Network, University of Michigan, University of California 
          Los Angeles, Boston University, and Wayne State University. 
          https://healthymindsnetwork.org/research/data-for-researchers.",
          br(),
          br(),
          # Description of Degree Type added by Maxine
          "Degree Type is primarily derived from deg_ass and deg_bach",
          ", as well as deg_mast, deg_jd, deg_md, and deg_phd from the dataset.", 
          "More specifically, students are assigned to each",
          "category based on the highest degree they are pursuing.",
          "Students pursuing Associate's and Bachelor's degrees are",
          "recorded in the Bachelor's category but not the Associate's",
          "category, and no students pursuing a graduate degree are",
          "included as Associate's or Bachelor's even if they pursue",
          "these degrees simultaneously."
        ),
        br(),
        # table of variables used
        tags$img(src = "table.png", 
                 width = "700px", 
                 height = "550px") 
      )
    ))
  )
)


# Define server logic ----
server <- function(input, output) {
  # Code under this function is by Maxine (except as otherwise noted)
  # It plots academic adjustment by symptoms of each mental condition
  output$plot <- renderPlotly({
    
    # First, the data set is selected based on the user's input as to which
    # degree is studied. This data set is stored as "data_by_degree".
    data_by_degree <- switch(
      input$degType,
      "Associate's" = avg_adjust_aca_assoc,
      "Bachelor's" = avg_adjust_aca_bach,
      "All Undergraduate" = avg_adjust_aca_undergrad
    )
    
    # The x variable is either Anxiety or Depression scores
    # based on user input
    x <- switch(
      input$condition,
      "Anxiety" = data_by_degree$anx_score,
      "Depression" = data_by_degree$deprawsc
    )
    
    # The y variable is always average academic adjustment for each score
    # as in the x variable
    y <- data_by_degree$"Avg Academic Adjustment"
    
    # Gets the label for the x-axis
    xstring <- paste(input$condition, "score") # Added by Miranda
    
    # Gets the label for the color legend
    color_string <- input$degType
    
    # Creates a plot of the data, to be rendered with Plotly
    g <- ggplot(data = data_by_degree, aes_string(x = x, y = y)) +
      # Depression and anxiety data is included in each data set, but only
      # one will be used. Removing the NAs for Anxiety or Depression ensures
      # only the data points for the chosen mental condition are included
      # for both points plotted and the linear regression.
      geom_smooth(color = "darkgray", method = "lm", na.rm = TRUE) +
      geom_point(aes(color = Degree), na.rm = TRUE) +
      scale_color_manual(
        values = c(
          "Associate's" = "RED",
          "Bachelor's" = "BLUE",
          "All" = "PURPLE"
        ),
        labels = c(
          "Associate's" = "Associate's",
          "Bachelor's" = "Bachelor's",
          "All" = "Undergraduate All"
        )
      ) +
      # "labs" line below by Miranda
      labs(
        x = xstring,
        color = "Degree Type",
        y = "Avg Academic Adjustment Difficulty score",
        title = "Relating Mental Health and College Difficulty"
      ) +
      # Centers the title
      theme(plot.title = element_text(hjust = 0.5))
    
    # Use Plotly to render the plot, so that x and y values appear when the
    # mouse hovers over a point
    ggplotly(g) |>
      # added by Miranda
      config(displayModeBar = FALSE)
  })
  
  # Bar Graph added by Henry, with exceptions
  output$bar <- renderPlot({
    # Reading Input
    g2_min = input$g2_yr[1]
    g2_max = input$g2_yr[2]
    g2_explanatory = input$g2_explanatory
    g2_degType = input$g2_degType
    
    # Gives g2_by_degree numeric values depending 
    # on the input from the selector tool
    g2_by_degree <- switch(
      g2_degType,
      "g2_a" = 1,
      "g2_b" = 2,
      "g2_all" = 0
    )
    
    # Changes the title of the graph depending on the inputs
    g2_title = "Students Reporting"
    if(g2_explanatory == "Anxiety") {
      g2_title = paste(g2_title, "Diagnosed Anxiety")
    }
    if(g2_explanatory == "Depression") {
      g2_title = paste(g2_title, "Diagnosed Depression")
    }
    if(g2_explanatory == "Ease of Adjustment") {
      g2_title = paste(g2_title, "Easy Academic Adjustment")
    }
    if(g2_by_degree == 0){
      g2_title = paste("Proportion of Undergraduate", g2_title)
    }
    if(g2_by_degree == 1){
      g2_title = paste("Proportion of Associate", g2_title)
    }
    if(g2_by_degree == 2){
      g2_title = paste("Proportion of Bachelor", g2_title)
    }
    
    bardata <- prop_yr_all |>
      # Filters the data so that only the selected years will be shown
      filter(survey_year >= g2_min & survey_year <= g2_max) |>
      # Filters the data so that only the specified degree type will be shown
      # Also doesn't filter if all the degree types are requested
      filter(g2_by_degree == degree_type | g2_by_degree == 0) #|>
    
    # Changes degree_type to a categorical variable
    bardata$degree_type = as.factor(bardata$degree_type)
    
    # Selects the correct column to plot given the explanatory variable
    g2_x <- switch(
      g2_explanatory,
      "Anxiety" = bardata$prop_anx,
      "Depression" = bardata$prop_dep,
      "Ease of Adjustment" = bardata$prop_easy
    )
    
    # Creates a barplot with years on x-axis, 
    # explanatory variable on y-axis, and degree type on colors
    ggplot(data = bardata, aes(y = g2_x, 
                               x = bardata$survey_year, 
                               fill = bardata$degree_type)) +
      # Alters the barplot so both columns can be shown side by side 
      # if both degrees are selected
      geom_col(position = "dodge2", na.rm = TRUE) +
      # Maintains the scale to be between 1 and 0 
      # and switches the values to percentages
      # Makes it easier to switch between degrees and explanatory variables
      scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
      # Adds tick marks for each year for easier understanding
      scale_x_continuous(minor_breaks = bardata$survey_year) + 
      scale_fill_manual(
        # Selects consistent colors to be given to the degree columns
        values = c("1" = "RED", "2" = "BLUE"),
        # Renames the values given for the Degree Type in the legend
        labels = c("1" = "Associate's", "2" = "Bachelor's")
      ) +
      # Adds titles
      labs(title = g2_title, 
           fill = "Degree Type",
           x = "Year",  # added by Miranda
           y = "Percentage")   # added by Miranda
       })
  
  # Added by Miranda
  # leaflet output
  output$map <- renderLeaflet({
    
    # Reading year input
    g3_yr = input$map_yr
    
    # Drop NAs
    combinedgeo_easy <- combinedgeo |>
      drop_na(prop_easy)
    
    combinedgeo_anx <- combinedgeo |>
      drop_na(prop_anx)
    
    combinedgeo_dep <- combinedgeo |>
      drop_na(prop_dep)
    
    # Defining palettes
    pal <- colorNumeric(
      palette = "Greens",
      domain = combinedgeo_easy$prop_easy,
      na.color = NA
    )
    
    pal2 <- colorNumeric(
      palette = "RdPu",
      domain = combinedgeo_anx$prop_anx,
      na.color = NA
    )
    
    pal3 <- colorNumeric(
      palette = "Blues",
      domain = combinedgeo_dep$prop_dep,
      na.color = NA
    )
    
    # filter to only the selected year for each combo of region and condition
    
    # prop of ease for each region
    filteredr1_ease <- combinedr1_ease |>
      filter(survey_year == g3_yr)
    filteredr1_ease <- st_as_sf(filteredr1_ease)
    
    filteredr2_ease <- combinedr2_ease |>
      filter(survey_year == g3_yr)
    filteredr2_ease <- st_as_sf(filteredr2_ease)
    
    filteredr3_ease <- combinedr3_ease |>
      filter(survey_year == g3_yr)
    filteredr3_ease <- st_as_sf(filteredr3_ease)
    
    filteredr4_ease <- combinedr4_ease |>
      filter(survey_year == g3_yr)
    filteredr4_ease <- st_as_sf(filteredr4_ease)
    
    filteredr5_ease <- combinedr5_ease |>
      filter(survey_year == g3_yr)
    filteredr5_ease <- st_as_sf(filteredr5_ease)
    
    filteredr6_ease <- combinedr6_ease |>
      filter(survey_year == g3_yr)
    filteredr6_ease <- st_as_sf(filteredr6_ease)
    
    filteredr7_ease <- combinedr7_ease |>
      filter(survey_year == g3_yr)
    filteredr7_ease <- st_as_sf(filteredr7_ease)
    
    filteredr8_ease <- combinedr8_ease |>
      filter(survey_year == g3_yr)
    filteredr8_ease <- st_as_sf(filteredr8_ease)
    
    filteredr9_ease <- combinedr9_ease |>
      filter(survey_year == g3_yr)
    filteredr9_ease <- st_as_sf(filteredr9_ease)
    
    # prop of anxiety for each region
    filteredr1_anx <- combinedr1_anx |>
      filter(survey_year == g3_yr)
    filteredr1_anx <- st_as_sf(filteredr1_anx)
    
    filteredr2_anx <- combinedr2_anx |>
      filter(survey_year == g3_yr)
    filteredr2_anx <- st_as_sf(filteredr2_anx)
    
    filteredr3_anx <- combinedr3_anx |>
      filter(survey_year == g3_yr)
    filteredr3_anx <- st_as_sf(filteredr3_anx)
    
    filteredr4_anx <- combinedr4_anx |>
      filter(survey_year == g3_yr)
    filteredr4_anx <- st_as_sf(filteredr4_anx)
    
    filteredr5_anx <- combinedr5_anx |>
      filter(survey_year == g3_yr)
    filteredr5_anx <- st_as_sf(filteredr5_anx)
    
    filteredr6_anx <- combinedr6_anx |>
      filter(survey_year == g3_yr)
    filteredr6_anx <- st_as_sf(filteredr6_anx)
    
    filteredr7_anx <- combinedr7_anx |>
      filter(survey_year == g3_yr)
    filteredr7_anx <- st_as_sf(filteredr7_anx)
    
    filteredr8_anx <- combinedr8_anx |>
      filter(survey_year == g3_yr)
    filteredr8_anx <- st_as_sf(filteredr8_anx)
    
    filteredr9_anx <- combinedr9_anx |>
      filter(survey_year == g3_yr)
    filteredr9_anx <- st_as_sf(filteredr9_anx)
    
    # prop of depression for each region
    filteredr1_dep <- combinedr1_dep |>
      filter(survey_year == g3_yr)
    filteredr1_dep <- st_as_sf(filteredr1_dep)
    
    filteredr2_dep <- combinedr2_dep |>
      filter(survey_year == g3_yr)
    filteredr2_dep <- st_as_sf(filteredr2_dep)
    
    filteredr3_dep <- combinedr3_dep |>
      filter(survey_year == g3_yr)
    filteredr3_dep <- st_as_sf(filteredr3_dep)
    
    filteredr4_dep <- combinedr4_dep |>
      filter(survey_year == g3_yr)
    filteredr4_dep <- st_as_sf(filteredr4_dep)
    
    filteredr5_dep <- combinedr5_dep |>
      filter(survey_year == g3_yr)
    filteredr5_dep <- st_as_sf(filteredr5_dep)
    
    filteredr6_dep <- combinedr6_dep |>
      filter(survey_year == g3_yr)
    filteredr6_dep <- st_as_sf(filteredr6_dep)
    
    filteredr7_dep <- combinedr7_dep |>
      filter(survey_year == g3_yr)
    filteredr7_dep <- st_as_sf(filteredr7_dep)
    
    filteredr8_dep <- combinedr8_dep |>
      filter(survey_year == g3_yr)
    filteredr8_dep <- st_as_sf(filteredr8_dep)
    
    filteredr9_dep <- combinedr9_dep |>
      filter(survey_year == g3_yr)
    filteredr9_dep <- st_as_sf(filteredr9_dep)
    
    # leaflet
    leaflet(states_sf) |>
      # add map tiles (layout)
      addTiles() |>
      addLayersControl(
        # map options
        overlayGroups = c("Ease of adjustment", "Anxiety", "Depression"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) |>
      # add shape for region 1, adjust_easy
      addPolygons(
        data = filteredr1_ease,
        # specify fill color for region
        fillColor = pal(filteredr1_ease$prop_easy),
        # specify transparency of color
        fillOpacity = 0.8,
        # outline of polygon
        color = "black",
        # thickness of polygon
        weight = 1,
        # transparency of polygon
        opacity = 1,
        # when clicked, show prop value, rounded to 2 decimal pts
        popup = ~ paste0("Region 1 Adjustment:</br>", 
                         format(round(prop_easy, 2), nsmall = 2)),
        # specify group to correspond to selected button
        group = "Ease of adjustment"
      ) |>
      # region 2, adjust_easy
      addPolygons(
        data = filteredr2_ease,
        fillColor = pal(filteredr2_ease$prop_easy),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 2 Adjustment:</br>", 
                         format(round(prop_easy, 2), nsmall = 2)),
        
        group = "Ease of adjustment"
      ) |>
      # region 3, adjust_easy
      addPolygons(
        data = filteredr3_ease,
        fillColor = pal(filteredr3_ease$prop_easy),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 3 Adjustment:</br>", 
                         format(round(prop_easy, 2), nsmall = 2)),
        
        group = "Ease of adjustment"
      ) |>
      # region 4 adjust_easy
      addPolygons(
        data = filteredr4_ease,
        fillColor = pal(filteredr4_ease$prop_easy),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 4 Adjustment:</br>", 
                         format(round(prop_easy, 2), nsmall = 2)),
        group = "Ease of adjustment"
      ) |>
      # region 5, adjust_easy
      addPolygons(
        data = filteredr5_ease,
        fillColor = pal(filteredr5_ease$prop_easy),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 5 Adjustment:</br>", 
                         format(round(prop_easy, 2), nsmall = 2)),
        
        group = "Ease of adjustment"
      ) |>
      # region 6, adjust_easy
      addPolygons(
        data = filteredr6_ease,
        fillColor = pal(filteredr6_ease$prop_easy),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 6 Adjustment:</br>", 
                         format(round(prop_easy, 2), nsmall = 2)),
        group = "Ease of adjustment"
      ) |>
      # region 7, adjust_easy
      addPolygons(
        data = filteredr7_ease,
        fillColor = pal(filteredr7_ease$prop_easy),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 7 Adjustment:</br>", 
                         format(round(prop_easy, 2), nsmall = 2)),
        group = "Ease of adjustment"
      ) |>
      # region 8, adjust_easy
      addPolygons(
        data = filteredr8_ease,
        fillColor = pal(filteredr8_ease$prop_easy),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 8 Adjustment:</br>", 
                         format(round(prop_easy, 2), nsmall = 2)),
        group = "Ease of adjustment"
      ) |>
      # region 9_ease, adjust_easy
      addPolygons(
        data = filteredr9_ease,
        fillColor = pal(filteredr9_ease$prop_easy),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 9 Adjustment:</br>", 
                         format(round(prop_easy, 2), nsmall = 2)),
        group = "Ease of adjustment"
      ) |>
      # region 1, anxiety
      addPolygons(
        data = filteredr1_anx,
        fillColor = pal2(filteredr1_anx$prop_anx),
        
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        # when clicked, show prop value
        popup = ~ paste0("Region 1 Anxiety:</br>", 
                         format(round(prop_anx, 2), nsmall = 2)),
        group = "Anxiety"
      ) |>
      # region 2, anxiety
      addPolygons(
        data = filteredr2_anx,
        fillColor = pal2(filteredr2_anx$prop_anx),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 2 Anxiety:</br>", 
                         format(round(prop_anx, 2), nsmall = 2)),
        
        group = "Anxiety"
      ) |>
      # region 3, anxiety
      addPolygons(
        data = filteredr3_anx,
        fillColor = pal2(filteredr3_anx$prop_anx),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 3 Anxiety:</br>", 
                         format(round(prop_anx, 2), nsmall = 2)),
        
        group = "Anxiety"
      ) |>
      # region 4 anxiety
      addPolygons(
        data = filteredr4_anx,
        fillColor = pal2(filteredr4_anx$prop_anx),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 4 Anxiety:</br>", 
                         format(round(prop_anx, 2), nsmall = 2)),
        group = "Anxiety"
      ) |>
      # region 5, anxiety
      addPolygons(
        data = filteredr5_anx,
        fillColor = pal2(filteredr5_anx$prop_anx),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 5 Anxiety:</br>", 
                         format(round(prop_anx, 2), nsmall = 2)),
        
        group = "Anxiety"
      ) |>
      # region 6, anxiety
      addPolygons(
        data = filteredr6_anx,
        fillColor = pal2(filteredr6_anx$prop_anx),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 6 Anxiety:</br>", 
                         format(round(prop_anx, 2), nsmall = 2)),
        group = "Anxiety"
      ) |>
      # region 7, anxiety
      addPolygons(
        data = filteredr7_anx,
        fillColor = pal2(filteredr7_anx$prop_anx),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 7 Anxiety:</br>", 
                         format(round(prop_anx, 2), nsmall = 2)),
        group = "Anxiety"
      ) |>
      # region 8, anxiety
      addPolygons(
        data = filteredr8_anx,
        fillColor = pal2(filteredr8_anx$prop_anx),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 8 Anxiety:</br>", 
                         format(round(prop_anx, 2), nsmall = 2)),
        group = "Anxiety"
      ) |>
      # region 9, anxiety
      addPolygons(
        data = filteredr9_anx,
        fillColor = pal2(filteredr9_anx$prop_anx),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 9 Anxiety:</br>", 
                         format(round(prop_anx, 2), nsmall = 2)),
        group = "Anxiety"
      ) |>
      # region 1, depression
      addPolygons(
        data = filteredr1_dep,
        fillColor = pal3(filteredr1_dep$prop_dep),
        
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        # when clicked, show prop value
        popup = ~ paste0("Region 1 Depression:</br>", 
                         format(round(prop_dep, 2), nsmall = 2)),
        group = "Depression"
      ) |>
      # region 2, depression
      addPolygons(
        data = filteredr2_dep,
        fillColor = pal3(filteredr2_dep$prop_dep),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 2 Depression:</br>", 
                         format(round(prop_dep, 2), nsmall = 2)),
        group = "Depression"
      ) |>
      # region 3, depression
      addPolygons(
        data = filteredr3_dep,
        fillColor = pal3(filteredr3_dep$prop_dep),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 3 Depression:</br>", 
                         format(round(prop_dep, 2), nsmall = 2)),
        
        group = "Depression"
      ) |>
      # region 4 depression
      addPolygons(
        data = filteredr4_dep,
        fillColor = pal3(filteredr4_dep$prop_dep),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 4 Depression:</br>", 
                         format(round(prop_dep, 2), nsmall = 2)),
        group = "Depression"
      ) |>
      # region 5, depression
      addPolygons(
        data = filteredr5_dep,
        fillColor = pal3(filteredr5_dep$prop_dep),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 5 Depression:</br>", 
                         format(round(prop_dep, 2), nsmall = 2)),
        
        group = "Depression"
      ) |>
      # region 6, depression
      addPolygons(
        data = filteredr6_dep,
        fillColor = pal3(filteredr6_dep$prop_dep),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 6 Depression:</br>", 
                         format(round(prop_dep, 2), nsmall = 2)),
        group = "Depression"
      ) |>
      # region 7, depression
      addPolygons(
        data = filteredr7_dep,
        fillColor = pal3(filteredr7_dep$prop_dep),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 7 Depression:</br>", 
                         format(round(prop_dep, 2), nsmall = 2)),
        group = "Depression"
      ) |>
      # region 8, depression
      addPolygons(
        data = filteredr8_dep,
        fillColor = pal3(filteredr8_dep$prop_dep),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 8 Depression:</br>", 
                         format(round(prop_dep, 2), nsmall = 2)),
        group = "Depression"
      ) |>
      # region 9, depression
      addPolygons(
        data = filteredr9_dep,
        fillColor = pal3(filteredr9_dep$prop_dep),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        opacity = 1,
        popup = ~ paste0("Region 9 Depression:</br>", 
                         format(round(prop_dep, 2), nsmall = 2)),
        group = "Depression"
      ) |>
      # set view to center in the us
      setView(lng = -97,
              lat = 40,
              zoom = 4) |>
      # add legends for each group
      addLegend(
        # position
        "bottomleft",
        # palette
        pal = pal,
        # range of values for the palette
        values = combinedgeo$prop_easy,
        # title of legend
        title = "Proportion of easy adjustment",
        # transparency of legend
        opacity = 1,
        # which group legend corresponds to
        group = "Ease of adjustment"
      ) |>
      addLegend(
        "bottomleft",
        pal = pal2,
        values = combinedgeo$prop_anx,
        title = "Proportion of anxiety",
        opacity = 1,
        group = "Anxiety"
      ) |>
      addLegend(
        "bottomleft",
        pal = pal3,
        values = combinedgeo$prop_dep,
        title = "Proportion of depression",
        opacity = 1,
        group = "Depression"
        ) 
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
