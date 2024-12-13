library(shiny)
library(WDI)
library(ggplot2)

# Create prediction frame
# Retrieve historical population data for Macao
population_data <- WDI(country = "MAC", indicator = "SP.POP.TOTL", start = 2000, end = 2021)

# Build a linear model for population projection
population_model <- lm(SP.POP.TOTL ~ year, data = population_data)

# Generate future years for projection
future_years <- data.frame(year = 2022:2041)
future_years$SP.POP.TOTL <- predict(population_model, newdata = future_years)

# Combine historical and projected data
projected_data <- rbind(population_data[, c("year", "SP.POP.TOTL")], future_years)

# Create comparasion frame
two_data <- WDI(country = c("MAC", "HKG"),
                indicator = c("NY.GDP.MKTP.CD", "SP.POP.TOTL"),
                start = 2000, end = 2021)

# UI definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "body {
         background-image: url('3a.jpg'); /* Background image for the Welcome page */
         background-size: cover;
         background-repeat: no-repeat;
         background-attachment: fixed;
       }
       .text-box {
         background-color: rgba(255, 255, 255, 0.85); /* Semi-transparent white background */
         padding: 20px;
         border-radius: 10px;
         max-width: 800px;
         margin: auto;
         color: black
       }
       .swot-row {
      display: flex;
      justify-content: space-between; 
      gap: 20px;
    }
  "))
  ),
  
  titlePanel("Basic Introduction to Macao"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Navigation"),
      selectInput("tab", "Choose a tab:", 
                  choices = c("Welcome & General Information", "History", "Government", "Economy", "People", "Prediction", "Comparison", "SWOT Analysis"))
    ),
    
    mainPanel(
      uiOutput("mainContent")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  output$mainContent <- renderUI({
    switch(input$tab,
           "Welcome & General Information" = {
             fluidRow(
               column(12, div(class = "text-box", 
                              h3("General Information"),
                              p("Macao (Macau), officially known as the Macao Special Administrative Region of the People's Republic of China, is a vibrant and densely populated region located on the southern coast of China near the Pearl River Delta. With an area of just 32.9 square kilometers."),
                              tags$img(src = "macau-location-map.jpg", height = "300px", width = "80%", alt = "Map of Macao"),
                              tags$img(src = "Macau.webp", height = "300px", width = "50%", alt = "Macao's global location")
               ))
             )
           },
           "History" = {
             fluidRow(
               column(12, div(class = "text-box", 
                              h3("History"),
                              p("Macao used to be a former Portuguese colony. It was established as a Portuguese settlement in 1557, it was the first and last European colony in China, and it acted as a bridge between the East and West for centuries. Portuguese merchants used Macao as a hub for trade between China, Japan, and Europe, fostering a cultural and economic exchange that left an indelible mark on the city’s architecture, language, and traditions."),
                              p("In 1999, Macao was handed back to China, marking the end of over 400 years of Portuguese administration. Under the One Country, Two Systems framework, Macao retained its legal and economic systems, autonomy in domestic affairs, and freedom of religion and expression. This unique governance structure allows Macao to thrive as a multicultural hub, blending Chinese and Portuguese influences, evident in its UNESCO World Heritage sites such as the Ruins of St. Paul’s and A-Ma Temple. Today, Macao stands as a global tourist and cultural destination, preserving its historical legacy while embracing modern development."),
                              div(style = "display: flex; justify-content: center; align-items: center; gap: 20px;",
                                  tags$img(src = "Flag.png", height = "150px", alt = "Flag"),
                                  tags$img(src = "Emblem.png", height = "150px", alt = "Emblem")
                              )
               ))
             )
           },
           "Government" = {
             fluidRow(
               column(12, div(class = "text-box", 
                              h3("Government"),
                              p("Macao operates as a Special Administrative Region (SAR) of the People's Republic of China. This unique arrangement grants Macao a high degree of autonomy, except in matters of defense and foreign affairs, which are managed by the central Chinese government. However, Macao has its own legal and judicial system, which is based on Portuguese civil law, and retains control over its economy, taxation, immigration policies, and public security. The government is headed by the Chief Executive, who is selected by an election committee and formally appointed by the central government in Beijing."),
                              p("The Macao Legislative Assembly serves as the region’s law-making body, with members elected through a combination of direct elections, indirect elections, and appointments. The government structure also includes five principal administrative departments, each responsible for areas such as finance, education, and healthcare. Macao’s governance system prioritizes stability and economic growth, aligning its policies with its strategic role as a global tourism and gaming hub while embracing efforts to diversify into finance and technology sectors.")
               ))
             )
           },
           "Economy" = {
             fluidRow(
               column(12, div(class = "text-box", 
                              h3("Economy"),
                              p("Macao is consistently ranked among the wealthiest regions globally, boasting one of the highest GDP per capita levels. According to the International Monetary Fund, in 2021, its GDP per capita exceeded $40,000, which placed it in the top 10 globally for this metric. Known as the Las Vegas of Asia, Macao's gaming revenue not only surpasses that of Las Vegas but also positions it as the largest gaming market in the world. This has earned Macao the number one spot in gaming revenue worldwide, with billions of dollars in annual earnings. Its world-class casinos, luxury resorts, and vibrant entertainment facilities make it a leading destination for international and domestic tourists, placing it in the top 20 globally for visitor numbers."),
                              p("In terms of tourism competitiveness, Macao ranks highly within Asia for its infrastructure and cultural offerings, as recognized by the World Travel and Tourism Council. It is also one of the top 10 cities globally for tourism revenue. While the economy is heavily reliant on gaming, the government is working to diversify its economic base. Macao ranks highly in global economic freedom indexes and is noted for its tax-friendly policies and open market system. Nowadays, Macao vigorously promotes financial services, cultural tourism, and traditional Chinese medicine to reduce dependency on gaming, thus ensuring a more sustainable economic future.")
               ))
             )
           },
           "People" = {
             fluidRow(
               column(12, div(class = "text-box", 
                              h3("People"),
                              p("Macao is one of the most densely populated regions in the world with a population of approximately 684,000. 92% of the residents are Chinese, and the remaining population includes Portuguese, mixed Chinese and Portuguese descent (Macanese), and other smaller ethnic communities."),
                              p("Macao is a bilingual region, its official languages are Cantonese and Portuguese. Cantonese is the most widely spoken language, while Portuguese remains prominent in legal, governmental, and ceremonial contexts. English is also widely used in business and tourism."),
                              p("Education in Macao is highly regarded, with literacy rates exceeding 96%. The education system incorporates both Chinese and Portuguese curricula, and higher education institutions, such as the University of Macau, attract students from around the world. Despite its small size, Macao has a vibrant cultural scene, with festivals like the Dragon Boat Festival, Mid-Autumn Festival, and Lusofonia Festival celebrating both Chinese and Portuguese traditions.")
               ))
             )
           },
           "Prediction" = {
             fluidRow(
               column(12, div(class = "text-box", 
                              h3("20 Years Population Prediction"),
                              p("The chart shows the population prediction for Macao in future 20 years based on historical data. It is obvious that the population is increasing in 20 years without considering other effects like policies."),
                              plotOutput("populationPlot")
               ))
             )
           },
           "Comparison" = {
             fluidRow(
               column(12, div(class = "text-box", 
                              h3("Comparison"),
                              tabsetPanel(
                                tabPanel("GDP", 
                                         plotOutput("gdpComparisonPlot"),
                                         p("This chart compares the GDP of Macao and Hong Kong from 2000 to 2021. Hong Kong consistently has a higher GDP due to its diversified economy.")
                                ),
                                tabPanel("Population", 
                                         plotOutput("populationComparisonPlot"),
                                         p("This chart compares the population of Macao and Hong Kong from 2000 to 2021. Hong Kong has a significantly larger population due to its greater geographic size and economic base.")
                                )
                              )
               ))
             )
           },
           "SWOT Analysis" = {
             fluidRow(
               column(12, div(class = "text-box", 
                              h3("SWOT Analysis"),
                              p("A simple analysis of Macao's strengths, weaknesses, opportunities, and threats.")
               )),
               fluidRow(
                 column(5, div(class = "text-box",
                               h4("Strengths"),
                               p("1. Macao is a global leader in gaming revenue, making it the largest gaming market in the world."),
                               p("2. High GDP per capita, consistently ranked among the top 10 globally."),
                               p("3. Unique cultural blend of Portuguese and Chinese influences.")
                 )),
                 column(5, div(class = "text-box",
                               h4("Weaknesses"),
                               p("1. Heavy reliance on gaming and tourism industries, leading to economic vulnerability."),
                               p("2. Limited land area, causing constraints in expansion and infrastructure."),
                               p("3. Small population and workforce limit local talent pool.")
                 ))
               ),
               fluidRow(
                 column(5, div(class = "text-box",
                               h4("Opportunities"),
                               p("1. Diversification into financial services, technology, and cultural tourism."),
                               p("2. Enhanced regional cooperation with Mainland China, especially through the Greater Bay Area initiative."),
                               p("3. Growth in education and healthcare sectors to attract global talent.")
                 )),
                 column(5, div(class = "text-box",
                               h4("Threats"),
                               p("1. Economic downturns or global recessions can significantly impact tourism and gaming."),
                               p("2. Rising competition from other gaming and entertainment hubs."),
                               p("3. Environmental concerns such as land reclamation and climate change impacts.")
                 ))
               )
             )
           }
           
    )
  })
  
  # Render population projection plot
  output$populationPlot <- renderPlot({
    ggplot(projected_data, aes(x = year, y = SP.POP.TOTL)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(data = population_data, aes(x = year, y = SP.POP.TOTL), color = "red", size = 2) +
      labs(title = "Population Projection for Macao (2000-2041)",
           x = "Year", y = "Population") +
      theme_minimal()
  })

# Render GDP comparison plot
output$gdpComparisonPlot <- renderPlot({
  ggplot(two_data, aes(x = year, y = NY.GDP.MKTP.CD, color = country)) +
    geom_line(size = 1.2) +
    labs(title = "GDP Comparison: Macao vs Hong Kong (2000-2021)",
         x = "Year", y = "GDP (Current US$)") +
    scale_y_continuous(breaks = seq(0, max(two_data$NY.GDP.MKTP.CD, na.rm = TRUE), by = 5e10)) +
    theme_minimal()
})

# Render Population comparison plot
output$populationComparisonPlot <- renderPlot({
  ggplot(two_data, aes(x = year, y = SP.POP.TOTL, color = country)) +
    geom_line(size = 1.2) +
    labs(title = "Population Comparison: Macao vs Hong Kong (2000-2021)",
         x = "Year", y = "Population") +
    scale_y_continuous(breaks = seq(0, max(two_data$SP.POP.TOTL, na.rm = TRUE), by = 1e6)) +
    theme_minimal()
})
}

# Run the application
shinyApp(ui = ui, server = server)
