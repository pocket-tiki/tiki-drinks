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
               # radioGroupButtons(
               #   inputId = "spiritRadioButtons",
               #   label = "", 
               #   choices = c("Default", "Select All", "Select None"),
               #   # Add a class to the buttons, you can use Bootstrap status like 'info', 'primary', 'danger' (red), 'warning' or 'success' (green). Or use an arbitrary strings to add a custom class, e.g. : with status = "custom-class", buttons will have class btn-custom-class.
               #   selected = "Default",
               #   individual = TRUE,
               #   checkIcon = list(
               #     yes = tags$i(class = "fa fa-circle", 
               #                  style = "color: #1ea896"),
               #     no = tags$i(class = "fa fa-circle-o", 
               #                 style = "color: #1ea896")),
               #   #status = "info",
               #   size = "normal" # “xs”, “sm”, “normal”, “lg”
               # ),
               # tags$script("$(\"input:radio[name='spiritRadioButtons'][value='Default']\").parent().css('background-color', '#DE6B63');"),
               # tags$script("$(\"input:radio[name='spiritRadioButtons'][value='Select All']\").parent().css('background-color', '#EDB6B2');"),
               # tags$script("$(\"input:radio[name='spiritRadioButtons'][value='Select None']\").parent().css('background-color', '#E7E7E7');"),
               awesomeCheckboxGroup(
                 inputId = "spirit_checkbox", label = "", 
                 choices = spirit_choices,
                 selected = spirit_initial
               )),
      menuItem(h3("Liqueurs"),
               # radioGroupButtons(
               #   inputId = "liqueurRadioButtons",
               #   label = "", 
               #   choices = c("Default", "Select All", "Select None"),
               #   # Add a class to the buttons, you can use Bootstrap status like 'info', 'primary', 'danger' (red), 'warning' or 'success' (green). Or use an arbitrary strings to add a custom class, e.g. : with status = "custom-class", buttons will have class btn-custom-class.
               #   selected = "Default",
               #   individual = TRUE,
               #   checkIcon = list(
               #     yes = tags$i(class = "fa fa-circle", 
               #                  style = "color: #1ea896"),
               #     no = tags$i(class = "fa fa-circle-o", 
               #                 style = "color: #1ea896")),
               #   #status = "info",
               #   size = "normal"
               # ),
               awesomeCheckboxGroup(
                 inputId = "liqueur_checkbox", label = "", 
                 choices = liqueur_choices,
                 selected = liqueur_initial
               )),
      menuItem(h3("Juice"),
               # radioGroupButtons(
               #   inputId = "juiceRadioButtons",
               #   label = "", 
               #   choices = c("Default", "Select All", "Select None"),
               #   selected = "Default",
               #   individual = TRUE,
               #   checkIcon = list(
               #     yes = tags$i(class = "fa fa-circle", 
               #                  style = "color: #1ea896"),
               #     no = tags$i(class = "fa fa-circle-o", 
               #                 style = "color: #1ea896")),
               #   #status = "info"
               # ),
               awesomeCheckboxGroup(
                 inputId = "juice_checkbox", label = "", 
                 choices = juice_choices,
                 selected = juice_initial
               )),
      menuItem(h3("Fruit"),
               # radioGroupButtons(
               #   inputId = "fruitRadioButtons",
               #   label = "", 
               #   choices = c("Default", "Select All", "Select None"),
               #   selected = "Default",
               #   individual = TRUE,
               #   checkIcon = list(
               #     yes = tags$i(class = "fa fa-circle", 
               #                  style = "color: #1ea896"),
               #     no = tags$i(class = "fa fa-circle-o", 
               #                 style = "color: #1ea896")),
               #   #status = "info"
               # ),
               awesomeCheckboxGroup(
                 inputId = "fruit_checkbox", label = "", 
                 choices = fruit_choices,
                 selected = fruit_initial
               )),
      menuItem(h3("Syrup"),
               # radioGroupButtons(
               #   inputId = "syrupRadioButtons",
               #   label = "", 
               #   choices = c("Default", "Select All", "Select None"),
               #   selected = "Default",
               #   individual = TRUE,
               #   checkIcon = list(
               #     yes = tags$i(class = "fa fa-circle", 
               #                  style = "color: #1ea896"),
               #     no = tags$i(class = "fa fa-circle-o", 
               #                 style = "color: #1ea896")),
               #   #status = "info"
               # ),
               awesomeCheckboxGroup(
                 inputId = "syrup_checkbox", label = "", 
                 choices = syrup_choices,
                 selected = syrup_initial
               )),
      menuItem(h3("Mixers"),
               # radioGroupButtons(
               #   inputId = "mixerRadioButtons",
               #   label = "", 
               #   choices = c("Default", "Select All", "Select None"),
               #   selected = "Default",
               #   individual = TRUE,
               #   checkIcon = list(
               #     yes = tags$i(class = "fa fa-circle", 
               #                  style = "color: #1ea896"),
               #     no = tags$i(class = "fa fa-circle-o", 
               #                 style = "color: #1ea896")),
               #   #status = "info"
               # ),
               awesomeCheckboxGroup(
                 inputId = "mixer_checkbox", label = "", 
                 choices = mixer_choices,
                 selected = mixer_initial
               )),
      menuItem(h3("Bitters"),
               # radioGroupButtons(
               #   inputId = "bittersRadioButtons",
               #   label = "", 
               #   choices = c("Default", "Select All", "Select None"),
               #   selected = "Default",
               #   individual = TRUE,
               #   checkIcon = list(
               #     yes = tags$i(class = "fa fa-circle", 
               #                  style = "color: #1ea896"),
               #     no = tags$i(class = "fa fa-circle-o", 
               #                 style = "color: #1ea896")),
               #   #status = "info"
               # ),
               awesomeCheckboxGroup(
                 inputId = "bitters_checkbox", label = "", 
                 choices = bitters_choices,
                 selected = bitters_initial
               )),
      menuItem(h3("Spices"),
               # radioGroupButtons(
               #   inputId = "spiceRadioButtons",
               #   label = "", 
               #   choices = c("Default", "Select All", "Select None"),
               #   selected = "Default",
               #   individual = TRUE,
               #   checkIcon = list(
               #     yes = tags$i(class = "fa fa-circle", 
               #                  style = "color: #1ea896"),
               #     no = tags$i(class = "fa fa-circle-o", 
               #                 style = "color: #1ea896")),
               #   #status = "info"
               # ),
               awesomeCheckboxGroup(
                 inputId = "spice_checkbox", label = "", 
                 choices = spice_choices,
                 selected = spice_initial
               )),
      menuItem(h3("Miscellaneous"),
               # radioGroupButtons(
               #   inputId = "miscRadioButtons",
               #   label = "", 
               #   choices = c("Default", "Select All", "Select None"),
               #   selected = "Default",
               #   individual = TRUE,
               #   checkIcon = list(
               #     yes = tags$i(class = "fa fa-circle", 
               #                  style = "color: #1ea896"),
               #     no = tags$i(class = "fa fa-circle-o", 
               #                 style = "color: #1ea896")),
               #   #status = "info"
               # ),
               awesomeCheckboxGroup(
                 inputId = "misc_checkbox", label = "", 
                 choices = misc_choices,
                 selected = misc_initial
               ))
      # menuItem(h3("xMiscellaneous"),
      #          pickerInput(
      #            inputId = "misc_picker", label = h4("Miscellaneous"),
      #            choices = misc_choices,
      #            selected = misc_initial,
      #            options = list(`actions-box` = TRUE),
      #            multiple = TRUE)),
     
      # menuItem(h3("test...select all")
               # sliderTextInput(
               #   inputId = "mySliderText", 
               #   label = "Your choice:", 
               #   grid = TRUE, 
               #   force_edges = FALSE,
               #   choices = c("Select Name", "Default Selection", "Select All"),
               #   selected = "Default Selection")
               # awesomeCheckboxGroup(
               #   inputId = "mtcars",
               #   label = "mtcars column variables", 
               #   choices = myChoices,
               #   selected = initialChoices
               # )
               # )
    ) # end sidebar menu
  ), # end dashboard sidebar
  dashboardBody(
    # tags$head(
    #   tags$link(
    #     rel = "stylesheet",
    #     type = "text/css",
    #     #href = "shinydashboard_default.css"
    #     href = "tiki.css")
    # ),
    tags$head(
      tags$style(sass(sass_file("styles/tiki_sass.scss")))
    ),
    # tags$head(tags$style(HTML('
    #     .irs-grid-text {
    #     position: absolute;
    #     bottom: 0;
    #     left: 0;
    #     white-space: nowrap;
    #     text-align: center;
    #     font-size: 19px;
    #     line-height: 9px;
    #     padding: 0 3px;}
    #                           '))),
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "shinydashboard_default.css")),
    tabsetPanel(
      tabPanel(title = h4("Drinks Made With Only Selected Ingredients"),
               DT::dataTableOutput(outputId = "drinks_you_can_make_table")),
      tabPanel(title = h4("All Drinks"),
               #textOutput(outputId = "mis_ing"),
               DT::dataTableOutput(outputId = "missing_ingredient_table"))
      # tabPanel(title = h4("test"),
      #          DT::dataTableOutput(outputId = "test_table"))
    )
  ) # end dashboard body
) # end dashboard page

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # observe({
  #   updateAwesomeCheckboxGroup(
  #     session, 'mtcars', choices = myChoices,
  #     selected = test_selected()
  #   )
  # })
   
  # spirit_selected <- reactive({
  #   if (input$spiritRadioButtons == "Default") {
  #     return(spirit_initial)
  #   } else if (input$spiritRadioButtons == "Select All") {
  #     return(spirit_choices)
  #   } else if (input$spiritRadioButtons == "Select None") {
  #     return(NA)
  #   }
  # })
  # 
  # observe({
  #   updateAwesomeCheckboxGroup(
  #     session, inputId = 'spirit_checkbox',
  #     selected = spirit_selected()
  #   )
  # })
  # 
  # liqueur_selected <- reactive({
  #   if (input$liqueurRadioButtons == "Default") {
  #     return(liqueur_initial)
  #   } else if (input$liqueurRadioButtons == "Select All") {
  #     return(liqueur_choices)
  #   } else if (input$liqueurRadioButtons == "Select None") {
  #     return(NA)
  #   }
  # })
  
  # observe({
  #   updateAwesomeCheckboxGroup(
  #     session, inputId = 'liqueur_checkbox',
  #     selected = liqueur_selected()
  #   )
  # })
  # 
  # juice_selected <- reactive({
  #   if (input$juiceRadioButtons == "Default") {
  #     return(juice_initial)
  #   } else if (input$juiceRadioButtons == "Select All") {
  #     return(juice_choices)
  #   } else if (input$juiceRadioButtons == "Select None") {
  #     return(NA)
  #   }
  # })
  # 
  # observe({
  #   updateAwesomeCheckboxGroup(
  #     session, inputId = 'juice_checkbox',
  #     selected = juice_selected()
  #   )
  # })
  
  # fruit_selected <- reactive({
  #   if (input$fruitRadioButtons == "Default") {
  #     return(fruit_initial)
  #   } else if (input$fruitRadioButtons == "Select All") {
  #     return(fruit_choices)
  #   } else if (input$fruitRadioButtons == "Select None") {
  #     return(NA)
  #   }
  # })
  # 
  # observe({
  #   updateAwesomeCheckboxGroup(
  #     session, inputId = 'fruit_checkbox',
  #     selected = fruit_selected()
  #   )
  # })
  # 
  # syrup_selected <- reactive({
  #   if (input$syrupRadioButtons == "Default") {
  #     return(syrup_initial)
  #   } else if (input$syrupRadioButtons == "Select All") {
  #     return(syrup_choices)
  #   } else if (input$syrupRadioButtons == "Select None") {
  #     return(NA)
  #   }
  # })
  
  # observe({
  #   updateAwesomeCheckboxGroup(
  #     session, inputId = 'syrup_checkbox',
  #     selected = syrup_selected()
  #   )
  # })
  # 
  # mixer_selected <- reactive({
  #   if (input$mixerRadioButtons == "Default") {
  #     return(mixer_initial)
  #   } else if (input$mixerRadioButtons == "Select All") {
  #     return(mixer_choices)
  #   } else if (input$mixerRadioButtons == "Select None") {
  #     return(NA)
  #   }
  # })
  # 
  # observe({
  #   updateAwesomeCheckboxGroup(
  #     session, inputId = 'mixer_checkbox',
  #     selected = mixer_selected()
  #   )
  # })
  # 
  # bitters_selected <- reactive({
  #   if (input$bittersRadioButtons == "Default") {
  #     return(bitters_initial)
  #   } else if (input$bittersRadioButtons == "Select All") {
  #     return(bitters_choices)
  #   } else if (input$bittersRadioButtons == "Select None") {
  #     return(NA)
  #   }
  # })
  
  # observe({
  #   updateAwesomeCheckboxGroup(
  #     session, inputId = 'bitters_checkbox',
  #     selected = bitters_selected()
  #   )
  # })
  # 
  # spice_selected <- reactive({
  #   if (input$spiceRadioButtons == "Default") {
  #     return(spice_initial)
  #   } else if (input$spiceRadioButtons == "Select All") {
  #     return(spice_choices)
  #   } else if (input$spiceRadioButtons == "Select None") {
  #     return(NA)
  #   }
  # })
  # 
  # observe({
  #   updateAwesomeCheckboxGroup(
  #     session, inputId = 'spice_checkbox',
  #     selected = spice_selected()
  #   )
  # })
  
  # misc_selected <- reactive({
  #   if (input$miscRadioButtons == "Default") {
  #     return(misc_initial)
  #   } else if (input$miscRadioButtons == "Select All") {
  #     return(misc_choices)
  #   } else if (input$miscRadioButtons == "Select None") {
  #     return(NA)
  #   }
  # })
  # 
  # observe({
  #   updateAwesomeCheckboxGroup(
  #     session, inputId = 'misc_checkbox',
  #     selected = misc_selected()
  #   )
  # })
  
  available_ingredients <- reactive({
    as.list(c(input$spirit_checkbox, input$liqueur_checkbox, input$juice_checkbox, input$fruit_checkbox, input$syrup_checkbox, input$mixer_checkbox, input$bitters_checkbox, input$spice_checkbox, input$misc_checkbox))
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
      select(1, 8, 3:5)
  })
  
  output$drinks_you_can_make_table <- DT::renderDataTable({
    DT::datatable(data_for_table(),
                  options = list(pageLength = 25),
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
                  colnames = c("Cocktail Name", "Ingredients List", "Number of Available Ingredients", "Missing Ingredients", "Recipe", "Glass Type", "Source"),
                  filter = 'top',
                  escape = FALSE)
  })
  
  # data_for_test_table <- reactive({
  #   data %>% 
  #     cbind(result = results()) %>% 
  #     filter(result == "can make") %>% 
  #     select(1:5) %>% 
  #     mutate(ingredients_linebreak = str_replace_all(ingredients_string, ", ", "<br>"))
  # })
  # 
  # output$test_table <- DT::renderDataTable({
  #   DT::datatable(data_for_test_table(),
  #                 options = list(pageLength = 25),
  #                 rownames = FALSE,
  #                 filter = 'top',
  #                 escape = FALSE
  #   )
  # })
  

} # end server function

# Run the application 
shinyApp(ui = ui, server = server)
