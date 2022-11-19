library(shiny)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(fontawesome)

starbucks <- read.csv("starbucks_data.csv", stringsAsFactors = FALSE)

# Features of This Shiny App:
## Feature # 1 (Lines 35-54 & Lines 98-125): The first feature included in this app is the ability to sort the table containing data on Starbucks drinks according to the individual's preferences (i.e., size, temperature, type of milk, whipped cream or no whipped cream). This feature is important for this app because it returns a shortened list of drinks that match the individual's preferences, thus, making the task of choosing a drink much less daunting!
## Feature # 2 (Lines 62 & 99): This feature relies on the DT package to turn the static table into an interactive one. This is useful for this app because it allows the user to further sort the drinks according to nutritional information (i.e., saturated fat). Although this could've been done by including more widgets in the sidebar, doing so would've required the user to decide on a large set of parameters which can be quite daunting. Additionally, the user wouldn't have been able to see a neat, organized table with the drinks that have the lowest amount of saturated fat, for example, at the top!
## Feature # 3 (Lines 64 & Line 159-191): This feature allows the user to download the table as a csv file. This feature is particularly useful because it allows the user to save their drink options to refer to when ordering Starbucks the next time! I, for example, always get a hot, grande drink with whole milk and whip, and, so, having the option to save the table which contains the drink options available to me according to these preferences would be really useful!
## Feature # 4 (Line 39): This feature adds an image of the Starbucks logo to the app. This is useful because it makes the app more aesthetically-pleasing and, thus, improves the overall user experience!
## Feature # 5 (line 60 & Lines 131-160): This feature returns a line of text which tells the user how many drink options they have according to the inputted preferences. This feature is useful because if the user sees that the number of drink options matching their criteria is too great, they may consider altering the preferences to, for example, only include one kind of milk. Overall, this feature provides a nice foreshadowing for what the user can expect in the table!

ui <- fluidPage(
  tags$style(
    ".note {color: #d62b1f;} #element {color: #d62b1f;}",
    ".title {color: #00704A;} #element {color: #00704A;}"
  ),
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
        choices = c("Short", "Tall", "Grande", "Venti", "Trenta", "Solo", "Doppio", "Triple", "Quad", "1 Scoop", "1 Shot")
      ),
      radioButtons("tempInput", "Would you like a hot or a cold drink?",
        choices = c("Hot", "Cold"),
        inline = TRUE
      ),
      checkboxGroupInput("milkInput", "What kind of milk would you like in your drink? (Suggestion: Don't get 2% or whole milk if you're lactose-intolerant)",
        choices = c("No Milk", "Non-Fat Milk", "Soy Milk", "Coconut Milk", "2% Milk", "Whole Milk"),
        selected = "Whole Milk"
      ),
      radioButtons("whipInput", "Would you like whipped cream on your drink?",
        choices = c("Yes", "No"),
        selected = "No",
        inline = TRUE
      ),
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
      filter(
        size == input$sizeInput,
        temp == input$tempInput,
        whip == input$whipInput
      ) %>%
      filter(milk %in% input$milkInput)
    ggplot(sugar_filtered, aes(x = product_name)) +
      geom_col(aes(y = sugar_g), color = "#006241", fill = "#006241") +
      labs(x = "Name of Drink", y = "Sugar Content (g)") +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      theme(axis.text.x = element_text(size = 9), axis.title = element_text(face = "bold"))
  })
  output$caffeineplot <- renderPlot({
    caffeine_filtered <-
      starbucks %>%
      filter(
        size == input$sizeInput,
        temp == input$tempInput,
        whip == input$whipInput
      ) %>%
      filter(milk %in% input$milkInput)
    ggplot(caffeine_filtered, aes(x = product_name)) +
      geom_col(aes(y = caffeine_mg), color = "#fbbc05", fill = "#fbbc05") +
      labs(x = "Name of Drink", y = "Caffeine Content (mg)") +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      theme(axis.text.x = element_text(size = 9), axis.title = element_text(face = "bold"))
  })
  output$results <- DT::renderDataTable({
    filtered_table <-
      starbucks %>%
      filter(
        size == input$sizeInput,
        temp == input$tempInput,
        whip == input$whipInput
      ) %>%
      filter(milk %in% input$milkInput) %>%
      rename(
        "Name of Drink" = product_name,
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
      filter(
        size == input$sizeInput,
        temp == input$tempInput,
        whip == input$whipInput
      ) %>%
      filter(milk %in% input$milkInput) %>%
      rename(
        "Name of Drink" = product_name,
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
      paste("Starbucks Drink Options-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      filtered_table <-
        starbucks %>%
        filter(
          size == input$sizeInput,
          temp == input$tempInput,
          whip == input$whipInput
        ) %>%
        filter(milk %in% input$milkInput) %>%
        rename(
          "Name of Drink" = product_name,
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
      write.csv(filtered_table, file)
    }
  )
}
shinyApp(ui = ui, server = server)
