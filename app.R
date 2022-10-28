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
library(shinyWidgets)
library(shinydashboard)

#xxx...from category specific checkbox groups, combine all checked to filter

data <- read_csv(here('pocket_tiki_data.csv')) %>% 
  rename(ingredients_string = ingredients_list) %>% 
  mutate(ingredients_list = as.list(strsplit(ingredients_string, ", "))) %>% 
  mutate(num_ing = lengths(ingredients_list))

ing_categories <- read_csv(here("ingredient_categories.csv"))

all_ingredients <- as_tibble(unique(unlist(strsplit(data$ingredients_string, ", ")))) %>% 
  rename(ingredient = value) %>% 
  left_join(ing_categories, by = 'ingredient')
# Tue Oct 25 15:16:21 2022 ------------------------------

fruit <- all_ingredients %>% filter(category == "fruit")
misc <- all_ingredients %>% filter(category == "misc" | is.na(category))
juice <- all_ingredients %>% filter(category == "juice")
syrup <- all_ingredients %>% filter(category == "syrup")
liqueur <- all_ingredients %>% filter(category == "liqueur")
spirit <- all_ingredients %>%  filter(category == "spirit")
bitters <- all_ingredients %>% filter(category == "bitters")
mixer <- all_ingredients %>% filter(category == "mixer")
spice <- all_ingredients %>% filter(category == "spice")

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Pocket Tiki"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(h3("Spirits"),
               pickerInput(
                 inputId = "spirit_picker",
                 choices = spirit$ingredient,
                 selected = spirit$ingredient[spirit$initially_selected == "yes"],
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE)),
      menuItem(h3("Liqueurs"),
               pickerInput(
                 inputId = "liqueur_picker",
                 choices = liqueur$ingredient,
                 selected = liqueur$ingredient[liqueur$initially_selected == "yes"],
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE)),
      menuItem(h3("Juices"),
               pickerInput(
                 inputId = "juice_picker",
                 choices = juice$ingredient,
                 selected = juice$ingredient[juice$initially_selected == "yes"],
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE)),
      menuItem(h3("Fruit"),
               pickerInput(
                 inputId = "fruit_picker",
                 choices = fruit$ingredient,
                 selected = fruit$ingredient[fruit$initially_selected == "yes"],
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE)),
      menuItem(h3("Syrup"),
               pickerInput(
                 inputId = "syrup_picker", label = h4("Syrups"),
                 choices = syrup$ingredient,
                 selected = syrup$ingredient[syrup$initially_selected == "yes"],
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE)),
      menuItem(h3("Mixers"),
               pickerInput(
                 inputId = "mixer_picker",
                 choices = mixer$ingredient,
                 selected = mixer$ingredient[mixer$initially_selected == "yes"],
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE)),
      menuItem(h3("Bitters"),
               pickerInput(
                 inputId = "bitters_picker", label = h4("Bitters"),
                 choices = bitters$ingredient,
                 selected = bitters$ingredient[bitters$initially_selected == "yes"],
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE)),
      menuItem(h3("Spices"),
               pickerInput(
                 inputId = "spice_picker", label = h4("Spices"),
                 choices = spice$ingredient,
                 selected = spice$ingredient[spice$initially_selected == "yes"],
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE)),
      menuItem(h3("Miscellaneous"),
               pickerInput(
                 inputId = "misc_picker", label = h4("Miscellaneous"),
                 choices = misc$ingredient,
                 selected = misc$ingredient[misc$initially_selected == "yes"],
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE))
    ) # end sidebar menu
  ), # end dashboard sidebar
  dashboardBody(
    #DT::dataTableOutput(outputId = "drinks_you_can_make_table"),
    tabsetPanel(
      tabPanel(title = "Drinks Made With Only Selected Ingredients",
               DT::dataTableOutput(outputId = "drinks_you_can_make_table")),
      tabPanel(title = "All Drinks",
               textOutput(outputId = "mis_ing"),
               DT::dataTableOutput(outputId = "missing_ingredient_table"))
    )
  ) # end dashboard body
) # end dashboard page

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  available_ingredients <- reactive({
    as.list(c(input$spirit_picker, input$spice_picker, input$liqueur_picker, input$bitters_picker, input$misc_picker, input$juice_picker, input$fruit_picker, input$syrup_picker, input$mixer_picker))
  })
  
  missing_ingredients <- reactive({
    setdiff(all_ingredients$ingredient, available_ingredients())
  })
  
  output$mis_ing <- renderText({
    as.character(missing_ingredients())
  })
  
  #output$available_ingredients_text <- renderText(as.character(available_ingredients()))
  
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
      filter(result == "can make") %>% 
      select(1:5)
  })
  
  output$drinks_you_can_make_table <- DT::renderDataTable({
    DT::datatable(data_for_table(),
                  options = list(pageLength = 25),
                  rownames = FALSE,
                  colnames = c("Cocktail Name", "Ingredients List", "Recipe", "Glass Type", "Source"),
                  filter = 'top')
  })
  
  # number_available_ingredients <- reactive({
  #   function(df){
  #     ing_unlist_DT2 <- unlist(df$ingredients_list)
  #     ing_list_DT2 <- as.list(ing_unlist_DT2)
  #     num_ing_avail <- sum(ing_list_DT2 %in% available_ingredients())
  #     return(num_ing_avail)
  #   }
  # })
  # 
  # result_DT2 <- reactive({
  #   apply(data, 1, number_available_ingredients())
  # })
  
  # data_for_table2 <- reactive({
  #   data %>% 
  #     cbind(num_ingredients_available = result_DT2()) %>% 
  #     filter(num_ingredients_available > 0)
  #     #select(1:5)
  # })
  # 
  # output$drinks_with_at_least_1_ingredient_table <- DT::renderDataTable({
  #   DT::datatable(data_for_table2(),
  #                 options = list(pageLength = 25),
  #                 rownames = FALSE,
  #                 #colnames = c("Cocktail Name", "Ingredients List", "Recipe", "Glass Type", "Source"),
  #                 filter = 'top')
  # })
  
  number_missing_ingredients_fun <- reactive({ function(df){
    ing_unlist_1 <- unlist(df$ingredients_list)
    ing_list_1 <- as.list(ing_unlist_1)
    num_missing <- sum(!(ing_list_1 %in% available_ingredients()))
    return(num_missing)
  }
  })
  
  num_missing_result <- reactive({
    apply(data, 1, number_missing_ingredients_fun())
  })
  
  missing_ingredients_fun <- reactive({
    function(df){
      ing_unlist_2 <- unlist(df$ingredients_list)
      ing_list_2 <- as.list(ing_unlist_2)
      missing_ing <- toString(setdiff(ing_list_2, available_ingredients()))
      return(missing_ing)
    }
  })
  
  missing_ing_result <- reactive({
    apply(data, 1, missing_ingredients_fun())
  })
  
  data_for_table3 <- reactive({
    data %>% 
      cbind(num_ingredients_missing = num_missing_result()) %>% 
      mutate(x_of_y = paste(num_ingredients_missing, "of", num_ing)) %>% 
      cbind(missing_ingredients_list = missing_ing_result()) %>% 
      select(1:5, 9, 10) %>% 
      select(1, 2, 6, 7, 3, 4, 5)
  })

  
  output$missing_ingredient_table <- DT::renderDataTable({
    DT::datatable(data_for_table3(),
                  options = list(pageLength = 25),
                  rownames = FALSE,
                  colnames = c("Cocktail Name", "Ingredients List", "Number of Available Ingredients", "Missing Ingredients", "Recipe", "Glass Type", "Source"),
                  filter = 'top')
  })
  

} # end server function

# Run the application 
shinyApp(ui = ui, server = server)
