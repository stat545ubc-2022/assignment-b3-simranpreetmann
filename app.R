library(shiny)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(fontawesome)

starbucks <- read.csv("starbucks_data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("How do you like your coffee?", windowTitle = "Starbucks Drink Options"),
  sidebarLayout(
    sidebarPanel(
      img(src = "starbucks_logo.png"),
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
      DT::dataTableOutput("results")
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
      geom_col(aes(y = sugar_g), color = "forestgreen", fill = "forestgreen") +
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
      geom_col(aes(y = caffeine_mg), color = "wheat3", fill = "wheat3") +
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
}
shinyApp(ui = ui, server = server)
