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


#xxx...from category specific checkbox groups, combine all checked to filter

data <- read_csv(here('pocket_tiki_data.csv')) %>% 
  rename(ingredients_string = ingredients_list) %>% 
  mutate(ingredients_list = as.list(strsplit(ingredients_string, ", "))) %>% 
  mutate(num_ing = lengths(ingredients_list)) %>% 
  arrange(cocktail_name)

ing_categories <- read_csv(here("ingredient_categories.csv"))

all_ingredients <- as_tibble(unique(unlist(strsplit(data$ingredients_string, ", ")))) %>% 
  rename(ingredient = value) %>% 
  arrange(ingredient) %>% 
  left_join(ing_categories, by = 'ingredient')

spirit <- all_ingredients %>%  filter(category == "spirit")
spirit_choices <- spirit$ingredient
spirit_initial <- na.omit(spirit$ingredient[spirit$initially_selected == "yes"])

liqueur <- all_ingredients %>% filter(category == "liqueur")
liqueur_initial <- liqueur$ingredient[liqueur$initially_selected == "yes"]
  
juice <- all_ingredients %>% filter(category == "juice")
juice_initial <- juice$ingredient[juice$initially_selected == "yes"]
  
fruit <- all_ingredients %>% filter(category == "fruit")
fruit_initial <- fruit$ingredient[fruit$initially_selected == "yes"]

syrup <- all_ingredients %>% filter(category == "syrup")
syrup_initial <- syrup$ingredient[syrup$initially_selected == "yes"]

mixer <- all_ingredients %>% filter(category == "mixer")
mixer_initial <- mixer$ingredient[mixer$initially_selected == "yes"]

bitters <- all_ingredients %>% filter(category == "bitters")
bitters_initial <- bitters$ingredient[bitters$initially_selected == "yes"]

spice <- all_ingredients %>% filter(category == "spice")
spice_initial <- spice$ingredient[spice$initially_selected == "yes"]

misc <- all_ingredients %>% filter(category == "misc" | is.na(category))
misc_initial <- misc$ingredient[misc$initially_selected == "yes"]

myChoices = names(mtcars)
initialChoices = c("mpg", "cyl")

# Define UI for application that draws a histogram

ui <- dashboardPage(
  dashboardHeader(title = "Pocket Tiki"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(h3("Spirits"),
               sliderTextInput(
                 inputId = "spiritsSliderText", 
                 label = "", 
                 grid = TRUE, force_edges = FALSE,
                 choices = c("Select None", "Default Selection", "Select All"),
                 selected = "Default Selection"),
               awesomeCheckboxGroup(
                 inputId = "spirit_checkbox", label = "", 
                 choices = spirit_choices,
                 selected = spirit_initial
               )),
      menuItem(h3("xSpirits"),
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
                 multiple = TRUE)),
      menuItem(h3("test...xxx"),
               awesomeCheckboxGroup(
                 inputId = "Id044",
                 label = "", 
                 choices = misc$ingredient,
                 selected = misc$ingredient[misc$initially_selected == "yes"]
               )),
      menuItem(h3("test...select all"),
               sliderTextInput(
                 inputId = "mySliderText", 
                 label = "Your choice:", 
                 grid = TRUE, 
                 force_edges = FALSE,
                 choices = c("Select Name", "Default Selection", "Select All"),
                 selected = "Default Selection"),
               awesomeCheckboxGroup(
                 inputId = "mtcars",
                 label = "mtcars column variables", 
                 choices = myChoices,
                 selected = initialChoices
               ))
    ) # end sidebar menu
  ), # end dashboard sidebar
  dashboardBody(
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #387467; # header bar (top left corner)
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #1ea896; # top left corner hover
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #387467; # header
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #1ea896; # overflow sidebae
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #387467; # selected category
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #1ea896; # sidebar
                              color: #ffffff; # behind dropdowns
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ff715b; #hover category
                              }
        /* toggle button when hovered  */                    
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #ffffff;
                              }
                              '))),
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "shinydashboard_default.css")),
    tabsetPanel(
      tabPanel(title = "Drinks Made With Only Selected Ingredients",
               DT::dataTableOutput(outputId = "drinks_you_can_make_table")),
      tabPanel(title = "All Drinks",
               #textOutput(outputId = "mis_ing"),
               DT::dataTableOutput(outputId = "missing_ingredient_table")),
      tabPanel(title = "test",
               tableOutput(outputId = "test_table"))
    )
  ) # end dashboard body
) # end dashboard page

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  test_selected <- reactive({
    if (input$mySliderText == "Default Selection") {
      return(initialChoices)
    } else if (input$mySliderText == "Select All") {
      return(myChoices)
    } else {
      NULL
    }
  })
  
  # observe({
  #   updateAwesomeCheckboxGroup(
  #     session, 'mtcars', choices = myChoices,
  #     selected = test_selected()
  #   )
  # })
  
  spirit_selected <- reactive({
    if (input$spiritsSliderText == "Default Selection") {
      #return(c("gin", "pisco"))
      #return(na.omit(spirit_initial))
      return(spirit_initial)
      #return(spirit$ingredient[spirit$initially_selected == "yes"])
    } else if (input$spiritsSliderText == "Select All") {
      return(spirit_choices)
    } else {
      NULL
    }
  })

  observe({
    updateAwesomeCheckboxGroup(
      session, inputId = 'spirit_checkbox',
      #choices = spirit_choices,
      selected = spirit_selected()
    )
  })
  
  # available_ingredients <- reactive({
  #   as.list(c(input$spirit_picker, input$spice_picker, input$liqueur_picker, input$bitters_picker, input$misc_picker, input$juice_picker, input$fruit_picker, input$syrup_picker, input$mixer_picker))
  # })
  
  available_ingredients <- reactive({
    as.list(c(input$spirit_checkbox, input$spice_picker, input$liqueur_picker, input$bitters_picker, input$misc_picker, input$juice_picker, input$fruit_picker, input$syrup_picker, input$mixer_picker))
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
  
  data_for_test_table <- reactive({
    data %>% 
      cbind(result = results()) %>% 
      filter(result == "can make") %>% 
      select(1:5) %>% 
      mutate(ingredients_linebreak = str_replace_all(ingredients_string, ", ", "<br>"))
  })
  
  output$test_table <- renderTable({
    data_for_test_table() %>% 
      gt() %>% 
      gt_theme_nytimes()
  })
  

} # end server function

# Run the application 
shinyApp(ui = ui, server = server)
