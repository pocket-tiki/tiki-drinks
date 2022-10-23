#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(here)
library(DT)

#xxx...from category specific checkbox groups, combine all checked to filter

data <- read_csv(here('pocket_tiki_data.csv')) %>% 
  rename(ingredients_string = ingredients_list) %>% 
  mutate(ingredients_list = as.list(strsplit(ingredients_string, ", ")))

ing_categories <- read_csv(here("ingredient_categories.csv"))

all_ingredients <- as_tibble(unique(unlist(strsplit(data$ingredients_string, ", ")))) %>% 
  rename(ingredient = value) %>% 
  left_join(ing_categories, by = 'ingredient')

fruit <- all_ingredients %>% filter(category == "fruit")
misc <- all_ingredients %>% filter(category == "misc")
juice <- all_ingredients %>% filter(category == "juice")
syrup <- all_ingredients %>% filter(category == "syrup")
liqueur <- all_ingredients %>% filter(category == "liqueur")
spirit <- all_ingredients %>%  filter(category == "spirit")
bitters <- all_ingredients %>% filter(category == "bitters")
mixer <- all_ingredients %>% filter(category == "mixer")
spice <- all_ingredients %>% filter(category == "spice")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    #titlePanel(""),

    # Sidebar with a slider input for number of bins 
    fluidRow(
      column(2,
             checkboxGroupInput(inputId = "spirit_checkbox", label = "spirits",
                                choices = spirit$ingredient,
                                selected = spirit$ingredient[spirit$initially_selected == "yes"]),
             checkboxGroupInput(inputId = "spice_checkbox", label = "spices",
                                choices = spice$ingredient,
                                selected = spice$ingredient[spice$initially_selected == "yes"])
             ),
      column(2,
             checkboxGroupInput(inputId = "liqueur_checkbox", label = "liqueurs",
                                choices = liqueur$ingredient,
                                selected = liqueur$ingredient[liqueur$initially_selected == "yes"]),
             checkboxGroupInput(inputId = "bitters_checkbox", label = "bitters",
                                choices = bitters$ingredient,
                                selected = bitters$ingredient[bitters$initially_selected == "yes"]),
             checkboxGroupInput(inputId = "misc_checkbox", label = "misc",
                                choices = misc$ingredient,
                                selected = misc$ingredient[misc$initially_selected == "yes"])
             ),
      column(2,
             checkboxGroupInput(inputId = "juice_checkbox", label = "jucies",
                               choices = juice$ingredient,
                               selected = juice$ingredient[juice$initially_selected == "yes"]),
             checkboxGroupInput(inputId = "fruit_checkbox", label = "fruits",
                                choices = fruit$ingredient,
                                selected = fruit$ingredient[fruit$initially_selected == "yes"]),
             checkboxGroupInput(inputId = "syrup_checkbox", label = "syrups",
                                choices = syrup$ingredient,
                                selected = syrup$ingredient[syrup$initially_selected == "yes"]),
             
             checkboxGroupInput(inputId = "mixer_checkbox", label = "mixers",
                                choices = mixer$ingredient,
                                selected = mixer$ingredient[mixer$initially_selected == "yes"])
             ),
      column(6,
             #textOutput(outputId = "available_ingredients_text"),
             DT::dataTableOutput(outputId = "drinks_you_can_make_table")
             # full, unfiltered table
             # DT::datatable(data,
             #               options = list(pageLength = 10),
             #               rownames = FALSE,
             #               colnames = c("Cocktail Name", "Ingredients List", "Recipe", "Glass Type", "Source"),
             #               filter = 'top')
             )
    ) # end fluid row
    ) # end fluid page
     
         
          # checkboxGroupInput(inputId = "ingredients_you_have", label = "select ingredients that you have",
          #                    choices = all_ingredients,
          #                    # selected = all_ingredients
          #                    # selected = c("rum", "lime juice", "Demerara syrup")
          #                    selected = all_ingredients[1:160])
    

        # Show a plot of the generated distribution
   
          # p("missing ingredients"),
          # textOutput(outputId = "mis_ing"),
          # # put table here
          # DT::dataTableOutput(outputId = "drinks_you_can_make_table")
           

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  missing_ingredients <- reactive({
    setdiff(all_ingredients, input$ingredients_you_have)
  })
  
  output$mis_ing <- renderPrint({
    missing_ingredients()
  })
  
  available_ingredients <- reactive({
    as.list(c(input$spirit_checkbox, input$spice_checkbox, input$liqueur_checkbox, input$bitters_checkbox, input$misc_checkbox, input$juice_checkbox, input$fruit_checkbox, input$syrup_checkbox, input$mixer_checkbox))
  })
  
  output$available_ingredients_text <- renderText(as.character(available_ingredients()))
  
  # function to determine if there is at least 1 ingredient in the recipe that is not available
  can_you_make_drink <- reactive({
    function(df) {
      ing_unlist <- unlist(df$ingredients_list)
      ing_list <- as.list(ing_unlist)
      sd <- setdiff(ing_list, available_ingredients())
      
      if (length(sd) > 0) {
        return("can't make")
      } else {
        return("can make")
      }
    }
  })
    
  results <- reactive({
    apply(data, 1, can_you_make_drink())
  })
    
  data_for_table <- reactive({
    data %>% 
      cbind(result = results()) %>% 
      filter(result == "can make")
  })
    
  
  # drinks_you_can_make <- reactive({
  #     data %>%
  #     filter(!grepl(paste(missing_ingredients(), collapse = "|"), ingredients_list))
  #     })
  
  rum_drinks <- data %>% 
    filter(grepl("rum", ingredients_list))
  
  drinks_you_can_make <- reactive({
    data %>% 
      filter(all(ingredients_list %in% available_ingredients()))
  })

  output$drinks_you_can_make_table <- DT::renderDataTable({
    DT::datatable(data_for_table(),
                  options = list(pageLength = 10),
                  rownames = FALSE,
                  colnames = c("Cocktail Name", "Ingredients List", "Recipe", "Glass Type", "Source"),
                  filter = 'top')
  })
  
  
  output$data_table <- DT::renderDataTable({
    DT::datatable(data,
                  options = list(pageLength = 10),
                  rownames = FALSE,
                  colnames = c("Cocktail Name", "Ingredients List", "Recipe", "Glass Type", "Source"),
                  filter = 'top')
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
