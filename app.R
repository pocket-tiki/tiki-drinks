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
library(sass)
library(gt)
library(gtExtras)

smugglers_cove_data <- read_csv(here('smugglers_cove.csv')) %>% 
  rename(ingredients_string = ingredients_list) %>% 
  mutate(ingredients_list = as.list(strsplit(ingredients_string, ", "))) %>% 
  mutate(num_ing = lengths(ingredients_list)) %>% 
  mutate(ingredients_linebreak = str_replace_all(ingredients_string, ", ", ",<br>"))

easy_tiki_data <- read_csv(here('easy_tiki.csv')) %>% 
  rename(ingredients_string = ingredients_list) %>% 
  mutate(ingredients_list = as.list(strsplit(ingredients_string, ", "))) %>% 
  mutate(num_ing = lengths(ingredients_list)) %>% 
  mutate(ingredients_linebreak = str_replace_all(ingredients_string, ", ", ",<br>"))

data <- rbind(smugglers_cove_data, easy_tiki_data) %>% 
  arrange(cocktail_name)

ing_categories <- read_csv(here("ingredient_categories.csv"))

all_ingredients <- as_tibble(unique(unlist(strsplit(data$ingredients_string, ", ")))) %>% 
  rename(ingredient = value) %>% 
  arrange(ingredient) %>% 
  left_join(ing_categories, by = 'ingredient')

spirit <- all_ingredients %>%  filter(category == "spirit")
spirit_choices <- spirit$ingredient
# spirit_initial <- na.omit(spirit$ingredient[spirit$initially_selected == "yes"])
spirit_initial <- na.omit(spirit$ingredient[spirit$current_party_initially_selected == "yes"])

liqueur <- all_ingredients %>% filter(category == "liqueur")
liqueur_choices <- liqueur$ingredient
# liqueur_initial <- na.omit(liqueur$ingredient[liqueur$initially_selected == "yes"])
liqueur_initial <- na.omit(liqueur$ingredient[liqueur$current_party_initially_selected == "yes"])
  
juice <- all_ingredients %>% filter(category == "juice")
juice_choices <- juice$ingredient
# juice_initial <- na.omit(juice$ingredient[juice$initially_selected == "yes"])
juice_initial <- na.omit(juice$ingredient[juice$current_party_initially_selected == "yes"])
  
fruit <- all_ingredients %>% filter(category == "fruit")
fruit_choices <- fruit$ingredient
# fruit_initial <- na.omit(fruit$ingredient[fruit$initially_selected == "yes"])
fruit_initial <- na.omit(fruit$ingredient[fruit$current_party_initially_selected == "yes"])

syrup <- all_ingredients %>% filter(category == "syrup")
syrup_choices <- syrup$ingredient
# syrup_initial <- na.omit(syrup$ingredient[syrup$initially_selected == "yes"])
syrup_initial <- na.omit(syrup$ingredient[syrup$current_party_initially_selected == "yes"])

mixer <- all_ingredients %>% filter(category == "mixer")
mixer_choices <- mixer$ingredient
# mixer_initial <- na.omit(mixer$ingredient[mixer$initially_selected == "yes"])
mixer_initial <- na.omit(mixer$ingredient[mixer$current_party_initially_selected == "yes"])

bitters <- all_ingredients %>% filter(category == "bitters")
bitters_choices <- bitters$ingredient
# bitters_initial <- na.omit(bitters$ingredient[bitters$initially_selected == "yes"])
bitters_initial <- na.omit(bitters$ingredient[bitters$current_party_initially_selected == "yes"])

spice <- all_ingredients %>% filter(category == "spice")
spice_choices <- spice$ingredient
# spice_initial <- na.omit(spice$ingredient[spice$initially_selected == "yes"])
spice_initial <- na.omit(spice$ingredient[spice$current_party_initially_selected == "yes"])

misc <- all_ingredients %>% filter(category == "misc" | is.na(category))
misc_choices <- misc$ingredient
# misc_initial <- na.omit(misc$ingredient[misc$initially_selected == "yes"])
misc_initial <- na.omit(misc$ingredient[misc$current_party_initially_selected == "yes"])


# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Pocket Tiki", 
                  titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem(h3("Spirits"),
               awesomeCheckboxGroup(
                 inputId = "spirit_checkbox", label = "", 
                 choices = spirit_choices,
                 selected = spirit_initial
               )),
      menuItem(h3("Liqueurs"),
               awesomeCheckboxGroup(
                 inputId = "liqueur_checkbox", label = "", 
                 choices = liqueur_choices,
                 selected = liqueur_initial
               )),
      menuItem(h3("Juice"),
               awesomeCheckboxGroup(
                 inputId = "juice_checkbox", label = "", 
                 choices = juice_choices,
                 selected = juice_initial
               )),
      menuItem(h3("Fruit"),
               awesomeCheckboxGroup(
                 inputId = "fruit_checkbox", label = "", 
                 choices = fruit_choices,
                 selected = fruit_initial
               )),
      menuItem(h3("Syrup"),
               awesomeCheckboxGroup(
                 inputId = "syrup_checkbox", label = "", 
                 choices = syrup_choices,
                 selected = syrup_initial
               )),
      menuItem(h3("Mixers"),
               awesomeCheckboxGroup(
                 inputId = "mixer_checkbox", label = "", 
                 choices = mixer_choices,
                 selected = mixer_initial
               )),
      menuItem(h3("Bitters"),
               awesomeCheckboxGroup(
                 inputId = "bitters_checkbox", label = "", 
                 choices = bitters_choices,
                 selected = bitters_initial
               )),
      menuItem(h3("Spices"),
               awesomeCheckboxGroup(
                 inputId = "spice_checkbox", label = "", 
                 choices = spice_choices,
                 selected = spice_initial
               )),
      menuItem(h3("Miscellaneous"),
               awesomeCheckboxGroup(
                 inputId = "misc_checkbox", label = "", 
                 choices = misc_choices,
                 selected = misc_initial
               ))
    ) # end sidebar menu
  ), # end dashboard sidebar
  dashboardBody(
    tags$head(
      tags$style(sass(sass_file("styles/tiki_sass.scss")))
    ),
    tabsetPanel(
      tabPanel(title = h4("Drinks Made With Only Selected Ingredients"),
               DT::dataTableOutput(outputId = "drinks_you_can_make_table")),
      tabPanel(title = h4("All Drinks"),
               DT::dataTableOutput(outputId = "missing_ingredient_table"))
    )
  ) # end dashboard body
) # end dashboard page

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  available_ingredients <- reactive({
    as.list(c(input$spirit_checkbox, input$liqueur_checkbox, input$juice_checkbox, input$fruit_checkbox, input$syrup_checkbox, input$mixer_checkbox, input$bitters_checkbox, input$spice_checkbox, input$misc_checkbox))
  })
  
  missing_ingredients <- reactive({
    setdiff(all_ingredients$ingredient, available_ingredients())
  })
  
  output$mis_ing <- renderText({
    as.character(missing_ingredients())
  })
  
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
      select(1, 8, 3:5)
  })
  
  output$drinks_you_can_make_table <- DT::renderDataTable({
    DT::datatable(data_for_table(),
                  options = list(pageLength = 50),
                  rownames = FALSE,
                  colnames = c("Cocktail Name", "Ingredients List", "Recipe", "Glass Type", "Source"),
                  filter = 'top',
                  escape = FALSE)
  })
  
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
  
  data_for_missing_table <- reactive({
    data %>% 
      cbind(num_ingredients_missing = num_missing_result()) %>% 
      mutate(x_of_y = paste(num_ingredients_missing, "of", num_ing)) %>%
      cbind(missing_ingredients_list = missing_ing_result()) %>% 
      mutate(missing_ingredients_list = str_replace_all(missing_ingredients_list, ", ", ",<br>")) %>% 
      select(1, 8, 10, 11, 3:5)
  })

  
  output$missing_ingredient_table <- DT::renderDataTable({
    DT::datatable(data_for_missing_table(),
                  options = list(pageLength = 25),
                  rownames = FALSE,
                  colnames = c("Cocktail Name", "Ingredients List", "Number of Missing Ingredients", "Missing Ingredients", "Recipe", "Glass Type", "Source"),
                  filter = 'top',
                  escape = FALSE)
  })
  

} # end server function

# Run the application 
shinyApp(ui = ui, server = server)
