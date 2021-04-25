library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)

joined <- read_csv("JoinedData.csv")
colnames(joined)[c(77,78)] <- c("usmaplon","usmaplat")
joined <- joined[,c(4,3,79:228,75:78,5:72)]
not_joined <- read_csv("NotJoinedData.csv")
colnames(not_joined)[1] <- "State"
colnames(not_joined)[c(155,156)] <- c("usmaplon","usmaplat")
not_joined[colnames(joined)[157:224]] <- NA

df <- rbind(not_joined,joined)

colleges <- read_csv("CollegeCrimes.csv")
hcrimes <- read_csv("HateCrimes.csv")


crime_dict <- list("RaceCrimes_" = "Race Crimes: ", 
                   "ReligionCrimes_" = "Religion Crimes: ",
                   "SexCrimes_" = "Sex Crimes: ", 
                   "GenderCrimes_" = "Gender Crimes: ",
                   "DisabilityCrimes_" = "Disability Crimes: ")

dem_stat_dict <- list("WhiteBlackDiss" = "White/Black Dissimilarity: ",
                      "WhiteHispDiss" = "White/Hispanic Dissimilarity: ",
                      "WhiteAsianDiss" = "White/Asian Dissimilarity: ",
                      "BlackHispDiss" = "Black/Hispanic Dissimilarity: ",
                      "BlackAsianDiss" = "Black/Asian Dissimilarity: ",
                      "HispAsianDiss" = "Hispanic/Asian Dissimilarity: ",
                      "WhitePerc" = "Percent White: ",
                      "BlackPerc" = "Percent Black: ",
                      "HispPerc" = "Percent Hispanic: ",
                      "AsianPerc" = "Percent Asian: ",
                      "OtherPerc" = "Percent Other Race: ",
                      "TotalCount" = "Total Population: ",
                      "WhiteCount" = "Number of Whites: ",
                      "BlackCount" = "Number of Blacks: ",
                      "HispCount" = "Number of Hispanics: ",
                      "AsianCount" = "Number of Asians: ",
                      "OtherCount" = "Number of Other Race: ")

dem_type_dict <- list("WhiteBlackDiss" = "Index of Dissimilarity",
                      "WhiteHispDiss" = "Index of Dissimilarity",
                      "WhiteAsianDiss" = "Index of Dissimilarity",
                      "BlackHispDiss" = "Index of Dissimilarity",
                      "BlackAsianDiss" = "Index of Dissimilarity",
                      "HispAsianDiss" = "Index of Dissimilarity",
                      "WhitePerc" = "Percent (%)",
                      "BlackPerc" = "Percent (%)",
                      "HispPerc" = "Percent (%)",
                      "AsianPerc" = "Percent (%)",
                      "OtherPerc" = "Percent (%)",
                      "TotalCount" = "Count",
                      "WhiteCount" = "Count",
                      "BlackCount" = "Count",
                      "HispCount" = "Count",
                      "AsianCount" = "Count",
                      "OtherCount" = "Count")


getData <- function(state_in, crime_in, year_in, census_in, dem_in) {
  segregation_var <- which(colnames(df) == str_c(census_in, dem_in, sep = ""))
  
  Crime_Years <- as.character(c(year_in[1]:year_in[2]))
  crime_vars <- which(colnames(df) %in% str_c(crime_in, Crime_Years))
  
  if(state_in == "US") {
    filtered_df <- df %>% 
      select(c(1,2, segregation_var, crime_vars, 153:156))
    filtered_df$CrimeTotal <- 0
    for (f in 1:length(filtered_df[,1])) {
      c_sum = 0
      for (v in 4:(3+length(crime_vars))) {
        c_sum = c_sum + filtered_df[f,v]
      }
      filtered_df$CrimeTotal[f] <- c_sum
    }
  }
  else {
    filtered_df <- df %>% 
      select(c(1,2, segregation_var, crime_vars, 153:156)) %>%
      filter(State == state_in)
    filtered_df$CrimeTotal <- 0
    for (f in 1:length(filtered_df$CrimeTotal)) {
      c_sum = 0
      for (v in 4:(3+length(crime_vars))) {
        c_sum = c_sum + filtered_df[f,v]
      }
      filtered_df$CrimeTotal[f] <- c_sum
    }
  }
  colnames(filtered_df)[3] <- "DemStat"
  return(filtered_df)
}

test <- getData("WI", "SexCrimes_", c(2000,2005), "09", "BlackPerc")

ui <- fluidPage(
  titlePanel("Hate Crimes and Segregation"),
  fluidRow(
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        helpText("Create a map of data about the number 
                 of hate crimes in a city, and the index
                 of dissimilarity representing how segregated
                 that city is."),
        
        selectInput("State", h3("Select a state"), 
                    choices = list("All" = "US", "Alabama" = "AL", "Alaska" = "AK", 
                                   "Arizona" = "AZ", "Arkansas" = "AR", 
                                   "California" = "CA", "Colorado" = "CO",
                                   "Connecticut" = "CT", "Delaware" = "DE", 
                                   "District of Columbia" = "DC","Florida" = "FL", 
                                   "Georgia" = "GA", "Hawaii" = "HI", "Idaho" = "ID",
                                   "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA",
                                   "Kansas" = "KS", "Kentucky" = "KY", 
                                   "Louisiana" = "LA", "Maine" = "ME", 
                                   "Maryland" = "MD", "Massachusetts" = "MA",
                                   "Michigan" = "MI", "Minnesota" = "MN",
                                   "Mississippi" = "MS", "Missouri" = "MO",
                                   "Montana" = "MT", "Nebraska" = "NE", 
                                   "Nevada" = "NV", "New Hampshire" = "NH",
                                   "New Jersey" = "NJ", "New Mexico" = "NM",
                                   "New York" = "NY", "North Carolina" = "NC",
                                   "North Dakota" = "ND", "Ohio" = "OH", 
                                   "Oklahoma" = "OK", "Oregon" = "OR", 
                                   "Pennsylvania" = "PA", "Rhode Island" = "RI",
                                   "South Carolina" = "SC", "South Dakota" = "SD",
                                   "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT",
                                   "Vermont" = "VT", "Virginia" = "VA", 
                                   "Washington" = "WA", "West Virginia" = "WV",
                                   "Wisconsin" = "WI", "Wyoming" = "WY"), 
                    selected = "US"),
        
        radioButtons("CrimeType", h3("Select a type of hate crime"),
                     choices = list("Race" = "RaceCrimes_", "Religion" = "ReligionCrimes_",
                                    "Sex" = "SexCrimes_", "Gender" = "GenderCrimes_",
                                    "Disability" = "DisabilityCrimes_"),
                     selected = "RaceCrimes_"),
        
        sliderInput("Year", h3("Select a year range"),
                    min = 1991, max = 2019, value = c(1991, 2019), sep = ""),
        
        radioButtons("CensusYear", h3("Select a Census year"),
                     choices = list("1980" = "80", "1990" = "90",
                                    "2000" = "00", "2005-2009 Survey" = "09"),
                     selected = "09"),
        selectInput("dem", h3("Select a demographic measurement"),
                    choices = list("White/Black Dissimilarity" = "WhiteBlackDiss",
                                   "White/Hispanic Dissimilarity" = "WhiteHispDiss",
                                   "White/Asian Dissimilarity" = "WhiteAsianDiss",
                                   "Black/Hispanic Dissimilarity" = "BlackHispDiss",
                                   "Black/Asian Dissimilarity" = "BlackAsianDiss",
                                   "Hispanic/Asian Dissimilarity" = "HispAsianDiss",
                                   "Percent White" = "WhitePerc",
                                   "Percent Black" = "BlackPerc",
                                   "Percent Hispanic" = "HispPerc",
                                   "Percent Asian" = "AsianPerc",
                                   "Percent Other Race" = "OtherPerc",
                                   "Total Population" = "TotalCount",
                                   "Number of Whites" = "WhiteCount",
                                   "Number of Blacks" = "BlackCount",
                                   "Number of Hispanics" = "HispCount",
                                   "Number of Asians" = "AsianCount",
                                   "Number of Other Race" = "OtherCount"),
                    selected = "WhiteBlackDiss")
        
      ),
      
      mainPanel(
        plotlyOutput(outputId = "fig"),
        textOutput(outputId = "test")
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$fig <- renderPlotly({
    
    instate <- as.character(input$State)
    incrime <- as.character(input$CrimeType)
    inyear <- c(as.numeric(input$Year[1]),as.numeric(input$Year[2]))
    incensus <- as.character(input$CensusYear)
    indem <- as.character(input$dem)
    
    data_df <- getData(instate, incrime, inyear, 
                       incensus, indem)
    data_df$CrimeTotal <- as.numeric(data_df$CrimeTotal)
    data_df$DemStat <- as.numeric(data_df$DemStat)
    
    g <- list(
      domain = list(x = 1, y = 1),
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showland = TRUE,
      showcounties = TRUE,
      showframe = TRUE,
      landcolor = toRGB("gray95"),
      subunitcolor = toRGB("#262626"),
      countrycolor = toRGB("gray85"),
      countrywidth = 0.5,
      subunitwidth = 0.5
    )
    

    m <- list(
      l = 50,
      r = 50,
      b = 100,
      t = 100,
      pad = 4
    )
    
    
    fig <- plot_geo(data = data_df, lat = ~lat, lon = ~lng)
    
    
    fig <- fig %>% add_markers(
      text = ~paste(City, State, paste(crime_dict[[incrime]], CrimeTotal, sep = ""),
                    paste(dem_stat_dict[[indem]], DemStat), 
                    sep = "<br />"),
      color = ~DemStat,
      size = ~CrimeTotal)
    
    fig <- fig %>% colorbar(title = dem_type_dict[[indem]])
    fig <- fig %>% layout(geo = g)
    
    fig
    })
  
}

shinyApp(ui, server)
