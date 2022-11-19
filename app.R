library(shiny)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(fontawesome)

starbucks <- read.csv("starbucks_data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  tags$style(".note {color: #d62b1f;} #element {color: #d62b1f;}",
             ".title {color: #00704A;} #element {color: #00704A;}"),
  tags$style(HTML("
                  @import url('https://fonts.googleapis.com/css2?family=Open+Sans:wght@800&display=swap');
                  .title {
                    font-family: 'Open Sans', sans-serif;
                  }")),
  tags$style(HTML("
                  @import url('https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400;800&display=swap');
                  p {
                   font-family: 'Open Sans', sans-serif;
                  }
                  h2 {
                   font-family: 'Open Sans', sans-serif;
                  }")),
  titlePanel(tags$b(class = "title", "How do you like your coffee?", style = "font-size:40px;"), windowTitle = "Starbucks Drink Options"),
  br(),
  p(class = "description", "Do you often find yourself in the Starbucks line overwhelmed by the number of drink options? Do you stumble up to the barista with no idea of what to order? Well, fear no more! This app was created to help you decide what to order at Starbucks without breaking a sweat! Simply, enter your preferences below and choose from the list of suggestions!"),
  p(class = "note", tags$em("NOTE: Information regarding the sugar and caffeine content of the drink options is also visualized for the health-conscious coffee-lover!")),
  sidebarLayout(
    sidebarPanel(
      img(src = "starbucks_logo.png", align = "center"),
      selectInput("sizeInput", "What size drink would you like?",
                  choices = c("Short", "Tall", "Grande", "Venti", "Trenta", "Solo", "Doppio", "Triple", "Quad", "1 Scoop", "1 Shot")),
      radioButtons("tempInput", "Would you like a hot or a cold drink?",
                   choices = c("Hot", "Cold"),
                   inline = TRUE),
      checkboxGroupInput("milkInput", "What kind of milk would you like in your drink? (Suggestion: Don't get 2% or whole milk if you're lactose-intolerant)",
                         choices = c("No Milk", "Non-Fat Milk", "Soy Milk", "Coconut Milk", "2% Milk", "Whole Milk"),
                         selected = "Whole Milk"),
      radioButtons("whipInput", "Would you like whipped cream on your drink?",
                   choices = c("Yes", "No"),
                   selected = "No",
                   inline = TRUE),
      submitButton("Show My Drink Options!")
    ),
    mainPanel(
      h2(textOutput("number_of_results")),
      plotOutput("sugarplot"),
      plotOutput("caffeineplot"),
      br(),
      br(),
      DT::dataTableOutput("results"),
      downloadButton("download_data", label = "Download Table")
    )
  )
)

server <- function(input, output) {
  output$sugarplot <- renderPlot({
    sugar_filtered <-
      starbucks %>%
      filter(size == input$sizeInput,
             temp == input$tempInput,
             whip == input$whipInput) %>%
      filter(milk %in% input$milkInput)
    ggplot(sugar_filtered, aes(x = product_name)) +
      geom_col(aes(y = sugar_g), color = "#006241", fill = "#006241") +
      labs(x = "Name of Drink", y = "Sugar Content (g)") +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      theme(axis.text.x = element_text(size = 9), axis.title = element_text(face="bold"))
  })
  output$caffeineplot <- renderPlot({
    caffeine_filtered <-
      starbucks %>%
      filter(size == input$sizeInput,
             temp == input$tempInput,
             whip == input$whipInput) %>%
      filter(milk %in% input$milkInput)
    ggplot(caffeine_filtered, aes(x = product_name)) +
      geom_col(aes(y = caffeine_mg), color = "#fbbc05", fill = "#fbbc05") +
      labs(x = "Name of Drink", y = "Caffeine Content (mg)") +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      theme(axis.text.x = element_text(size = 9), axis.title = element_text(face="bold"))
  })
  output$results <- DT::renderDataTable({
    filtered_table <-
      starbucks %>%
      filter(size == input$sizeInput,
             temp == input$tempInput,
             whip == input$whipInput) %>%
      filter(milk %in% input$milkInput) %>%
      rename("Name of Drink" = product_name,
             "Size of Drink" = size,
             "Type of Milk" = milk,
             "Temperature" = temp,
             "Whipped Cream?" = whip,
             "Serving Size (mL)" = serv_size_m_l,
             "Calories" = calories,
             "Total Fat (g)" = total_fat_g,
             "Saturated Fat (g)" = saturated_fat_g,
             "Trans Fat (g)" = trans_fat_g,
             "Cholesterol (mg)" = cholesterol_mg,
             "Sodium (mg)" = sodium_mg,
             "Total Carbohydrates (g)" = total_carbs_g,
             "Fiber (g)" = fiber_g,
             "Sugar (g)" = sugar_g,
             "Caffeine (mg)" = caffeine_mg
             )
    filtered_table
  })
  output$number_of_results <- renderText({
    filtered_table <-
      starbucks %>%
      filter(size == input$sizeInput,
             temp == input$tempInput,
             whip == input$whipInput) %>%
      filter(milk %in% input$milkInput) %>%
      rename("Name of Drink" = product_name,
             "Size of Drink" = size,
             "Type of Milk" = milk,
             "Temperature" = temp,
             "Whipped Cream?" = whip,
             "Serving Size (mL)" = serv_size_m_l,
             "Calories" = calories,
             "Total Fat (g)" = total_fat_g,
             "Saturated Fat (g)" = saturated_fat_g,
             "Trans Fat (g)" = trans_fat_g,
             "Cholesterol (mg)" = cholesterol_mg,
             "Sodium (mg)" = sodium_mg,
             "Total Carbohydrates (g)" = total_carbs_g,
             "Fiber (g)" = fiber_g,
             "Sugar (g)" = sugar_g,
             "Caffeine (mg)" = caffeine_mg
      )
    number_rows <- nrow(filtered_table)
    paste("You can choose from", print(number_rows), "drink options!")
  })
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Starbucks Drink Options-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      filtered_table <-
        starbucks %>%
        filter(size == input$sizeInput,
               temp == input$tempInput,
               whip == input$whipInput) %>%
        filter(milk %in% input$milkInput) %>%
        rename("Name of Drink" = product_name,
               "Size of Drink" = size,
               "Type of Milk" = milk,
               "Temperature" = temp,
               "Whipped Cream?" = whip,
               "Serving Size (mL)" = serv_size_m_l,
               "Calories" = calories,
               "Total Fat (g)" = total_fat_g,
               "Saturated Fat (g)" = saturated_fat_g,
               "Trans Fat (g)" = trans_fat_g,
               "Cholesterol (mg)" = cholesterol_mg,
               "Sodium (mg)" = sodium_mg,
               "Total Carbohydrates (g)" = total_carbs_g,
               "Fiber (g)" = fiber_g,
               "Sugar (g)" = sugar_g,
               "Caffeine (mg)" = caffeine_mg)
      write.csv(filtered_table, file)
    }
  )
}
shinyApp(ui = ui, server = server)
