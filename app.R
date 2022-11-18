library(shiny)
starbucks <- read.csv("starbucks_data.csv", stringsAsFactors = FALSE)
print(starbucks)
ui <- fluidPage(
  titlePanel("What makes a coffee espresso-ly nutritious?", windowTitle = "Nutritional Analysis of Starbucks Drinks"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sizeInput", "What size drink would you like?", c("Short" = "short",
                                          "Tall" = "tall",
                                          "Grande" = "grande",
                                          "Venti" = "venti",
                                          "Trenta" = "trenta",
                                          "Solo" = "solo",
                                          "Doppio" = "doppio",
                                          "Triple" = "triple",
                                          "Quad" = "quad",
                                          "1 Scoop" = "1 scoop",
                                          "1 Shot" = "1 shot")),
      radioButtons("tempInput", "Would you like a hot or a cold drink?", c("Hot" = "hot",
                                                                                "Cold" = "cold"),
                   inline = TRUE),
      checkboxGroupInput("milkInput", "What kind of milk would you like in your drink? (Suggestion: Don't get 2% or whole milk if you're lactose-intolerant)", c("No Milk" = "no milk",
                                                         "Non-Fat Milk" = "non-fat milk",
                                                         "Soy Milk" = "soy milk",
                                                         "Coconut Milk" = "coconut milk",
                                                         "2% Milk" = "two-percent milk",
                                                         "Whole Milk" = "whole milk")),
      radioButtons("whipInput", "Would you like whipped cream on your drink?", c("Yes" = "with whip",
                                                                                       "No" = "without whip"),
                   inline = TRUE),
      sliderInput("calorieInput", "It's always good to be conscious of your caffeine intake. What is the caffeine range of your potential drink?", min = 0, max = 375, value = c(10,100), post = "mg")
    ),
    mainPanel("results go here")
  )
)
server <- function(input, output) {}
shinyApp(ui = ui, server = server)
