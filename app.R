library(shiny)
library(shinythemes)
library(tidyverse)
library(ggraph)
library(tidygraph)
library(magrittr)

pleasure_ego_trim <- readRDS("pleasure_ego_trim.rds")

pleasure_nodes <- pleasure_ego_trim %>%
  activate(nodes) %>%
  as.tibble()

app_edges <- pleasure_ego_trim %>%
  activate(edges) %>%
  as.tibble() %>%
  left_join(pleasure_nodes, by = c("from" = "id")) %>%
  select(-from) %>%
  rename(from = name) %>%
  left_join(pleasure_nodes, by = c("to" = "id")) %>%
  select(-to) %>%
  rename(to = name) %>%
  rename(edge_weight = weight)

ui <- fluidPage(theme = shinytheme("darkly"),
                
  tags$div(uiOutput("check_box_selections"), id = "check_box_selections_div"),
  
  tags$div(uiOutput("prompt_1_render"), id = "prompt_1_div"),
  
  tags$div(uiOutput("action_1_render"), id = "action_1_div"),
  
  tags$div(uiOutput("prompt_2_render"), id = "prompt_2_div"),
  
  tags$div(uiOutput("action_2_render"), id = "action_2_div"),
  
  tags$div(uiOutput("prompt_3_render"), id = "prompt_3_div"),
  
  tags$div(uiOutput("action_3_render"), id = "action_3_div"),
  
  tags$div(uiOutput("prompt_4_render"), id = "prompt_4_div"),
  
  tags$div(uiOutput("action_4_render"), id = "action_4_div"),
  
  htmlOutput("explanation"),
  
  plotOutput("shown_graph"),
  
  uiOutput("get_the_item")
)

server <- function(input, output, session) {
  
  #section 0
  
  random_selection <- reactive({
    pleasure_ego_trim %>%
      activate(nodes) %>%
      group_by(community) %>%
      sample_n(1) %>%
      ungroup() %>%
      as.tibble() %>%
      pull(name)
  })
  
  output$check_box_selections <- renderUI({
    
    checkboxGroupInput("starting_pleasures",
                       "Select the three that give you the most pleasure",
                       random_selection())
  })
  
  observe({
    if(length(input$starting_pleasures) == 3)
      removeUI(
        selector = "#check_box_selections_div"
      )
  })
  
  #section 1
  
  selected_1 <- reactive({
    input$starting_pleasures
    })

  anti_selected_1 <- reactive({
    setdiff(random_selection(), selected_1())
    })
  
  suggestion_1 <- reactive({
    app_edges %>%
      filter(from %in% selected_1()) %>%
      group_by(to) %>%
      summarise(from_weight = sum(edge_weight)) %>%
      ungroup() %>%
      filter(!to %in% selected_1()) %>%
      filter(!to %in% anti_selected_1()) %>%
      top_n(1) %>%
      select(to) %>%
      slice(1) %>%
      as.character()
  })
  
  output$prompt_1_render <- renderUI({
    if(length(input$starting_pleasures) == 3){
      
      radioButtons("prompt_1", paste0(
        "Because you said '", 
        paste(selected_1()[1:length(selected_1())-1], collapse = "', '"),
        "' and '", selected_1()[length(selected_1())], 
        "' give you pleasure more than '",
        paste(anti_selected_1()[1:length(anti_selected_1())-1], collapse = "', '"),
        "' and '", anti_selected_1()[length(anti_selected_1())], 
        "', we recommend you try '", suggestion_1(), "'. Does it give you pleasure?"),  
                   choiceNames = c("Yes", "No"), choiceValues = c(1, 0))
      
    }
  })
  
  output$action_1_render <- renderUI({
    if(length(input$starting_pleasures) == 3){
      
      actionButton("goButton_1", "Enter")
      
    }
  })
  
  observeEvent(input$goButton_1, {
    removeUI(
      selector = "#prompt_1_div"
    )
  })
  
  observeEvent(input$goButton_1, {
    removeUI(
      selector = "#action_1_div"
    )
  })
  
  #section 2
  
  selected_2 <- reactive({
    if(input$prompt_1 == 1){
      c(selected_1(), suggestion_1())
    } else {
      selected_1()
    }
  })
  
  anti_selected_2 <- reactive({
    if(input$prompt_1 == 1){
      anti_selected_1()
    } else {
      c(anti_selected_1(), suggestion_1())
    }
  })
  
  suggestion_2 <- reactive({
    
      req(input$goButton_1)
      
      app_edges %>%
        filter(from %in% selected_2()) %>%
        group_by(to) %>%
        summarise(from_weight = sum(edge_weight)) %>%
        ungroup() %>%
        filter(!to %in% selected_2()) %>%
        filter(!to %in% anti_selected_2()) %>%
        top_n(1) %>%
        select(to) %>%
        slice(1) %>%
        as.character()
    
  })
  
  output$prompt_2_render <- renderUI({
    req(input$goButton_1)
    
    radioButtons("prompt_2", paste0(
      "Because you said '", 
      paste(selected_2()[1:length(selected_2())-1], collapse = ", "),
      "' and '", selected_2()[length(selected_2())], 
      "' give you pleasure, more than '",
      paste(anti_selected_2()[1:length(anti_selected_2())-1], collapse = "', '"),
      "' and '", anti_selected_2()[length(anti_selected_2())], 
      "' we recommend you try '", suggestion_2(), "'. Does it give you pleasure?"),  
      choiceNames = c("Yes", "No"), choiceValues = c(1, 0))
    
  })
  
  output$action_2_render <- renderUI({
    req(input$goButton_1)
    
    actionButton("goButton_2", "Enter")
    
  })
  
  observeEvent(input$goButton_2, {
    removeUI(
      selector = "#prompt_2_div"
    )
  })
  
  observeEvent(input$goButton_2, {
    removeUI(
      selector = "#action_2_div"
    )
  })
  
  #section 3
  
  selected_3 <- reactive({
    if(input$prompt_2 == 1){
      c(selected_2(), suggestion_2())
    } else {
      selected_2()
    }
  })
  
  anti_selected_3 <- reactive({
    if(input$prompt_2 == 1){
      anti_selected_2()
    } else {
      c(anti_selected_2(), suggestion_2())
    }
  })
  
  suggestion_3 <- reactive({
    
    req(input$goButton_2)
    
    app_edges %>%
      filter(from %in% selected_3()) %>%
      group_by(to) %>%
      summarise(from_weight = sum(edge_weight)) %>%
      ungroup() %>%
      filter(!to %in% selected_3()) %>%
      filter(!to %in% anti_selected_3()) %>%
      top_n(1) %>%
      select(to) %>%
      slice(1) %>%
      as.character()
    
  })
  
  output$prompt_3_render <- renderUI({
    req(input$goButton_2)
    
    radioButtons("prompt_3", paste0(
      "Because you said '", 
      paste(selected_3()[1:length(selected_3())-1], collapse = ", "),
      "' and '", selected_3()[length(selected_3())], 
      "' give you pleasure, more than '",
      paste(anti_selected_3()[1:length(anti_selected_3())-1], collapse = "', '"),
      "' and '", anti_selected_3()[length(anti_selected_3())], 
      "' we recommend you try '", suggestion_3(), "'. Does it give you pleasure?"),  
      choiceNames = c("Yes", "No"), choiceValues = c(1, 0))
    
  })
  
  output$action_3_render <- renderUI({
    req(input$goButton_2)
    
    actionButton("goButton_3", "Enter")
    
  })
  
  observeEvent(input$goButton_3, {
    removeUI(
      selector = "#prompt_3_div"
    )
  })
  
  observeEvent(input$goButton_3, {
    removeUI(
      selector = "#action_3_div"
    )
  })  
  
  #section 4
  
  selected_4 <- reactive({
    if(input$prompt_3 == 1){
      c(selected_3(), suggestion_3())
    } else {
      selected_3()
    }
  })
  
  anti_selected_4 <- reactive({
    if(input$prompt_3 == 1){
      anti_selected_3()
    } else {
      c(anti_selected_3(), suggestion_3())
    }
  })
  
  suggestion_4 <- reactive({
    
    req(input$goButton_3)
    
    app_edges %>%
      filter(from %in% selected_4()) %>%
      group_by(to) %>%
      summarise(from_weight = sum(edge_weight)) %>%
      ungroup() %>%
      filter(!to %in% selected_4()) %>%
      filter(!to %in% anti_selected_4()) %>%
      top_n(1) %>%
      select(to) %>%
      slice(1) %>%
      as.character()
    
  })
  
  output$prompt_4_render <- renderUI({
    req(input$goButton_3)
    
    radioButtons("prompt_4", paste0(
      "Because you said '", 
      paste(selected_4()[1:length(selected_4())-1], collapse = ", "),
      "' and '", selected_4()[length(selected_4())], 
      "' give you pleasure, more than '",
      paste(anti_selected_4()[1:length(anti_selected_4())-1], collapse = "', '"),
      "' and '", anti_selected_4()[length(anti_selected_4())], 
      "' we recommend you try '", suggestion_4(), "'. Does it give you pleasure?"),  
      choiceNames = c("Yes", "No"), choiceValues = c(1, 0))
    
  })
  
  output$action_4_render <- renderUI({
    req(input$goButton_3)
    
    actionButton("goButton_4", "Enter")
    
  })
  
  observeEvent(input$goButton_4, {
    removeUI(
      selector = "#prompt_4_div"
    )
  })
  
  observeEvent(input$goButton_4, {
    removeUI(
      selector = "#action_4_div"
    )
  })  
  
  # section 5
  
  selected_5 <- reactive({
    if(input$prompt_4 == 1){
      c(selected_4(), suggestion_4())
    } else {
      selected_4()
    }
  })
  
  anti_selected_5 <- reactive({
    if(input$prompt_4 == 1){
      anti_selected_4()
    } else {
      c(anti_selected_4(), suggestion_4())
    }
  })
  
  output$explanation <- renderText({
    
    req(input$goButton_4)

      HTML(paste0(
        "<B>It's your pleasure...</B><br/>",
        "Because you said '", 
        paste(selected_5()[1:length(selected_5())-1], collapse = ", "),
        "' and '", selected_5()[length(selected_5())], 
        "' give you pleasure, more than '",
        paste(anti_selected_5()[1:length(anti_selected_5())-1], collapse = "', '"),
        "' and '", anti_selected_5()[length(anti_selected_5())], "', here is a network map
               of your pleasures.<br/>The options you chose are highlighted, the options you chose against
               have been removed. The connecting lines show the links made by other participants. Importantly, the vast sea of possible pleasures that the algorithm never steered you
               toward are shown faintly. Remember that there are more pleasures you can choose than those presented by an algorithm.<br/>This art project is based on research about word associations. To find out more and to participate in the project visit <a href = 'https://smallworldofwords.org/en'> https://smallworldofwords.org/en</a>.<br/>You can download the network and share it by clicking the button bellow.<br/>")
      )
    
  })
  
  plotInput = function() {
    
    ggraph(filter(pleasure_ego_trim, !name %in% anti_selected_5()), layout = 'graphopt', charge = .05) + 
      geom_edge_link(alpha = .1, colour = "white") + 
      geom_node_text(aes(label = if_else(name %in% selected_5(), as.character(name), ""), colour = community), nudge_y = 100) + 
      geom_node_point(aes(alpha = if_else(name %in% selected_5(), 1, 0), colour = community, size = centrality)) +
      guides(colour = FALSE, alpha = FALSE, size = FALSE) + 
      theme(panel.background = element_rect(fill = "black"), 
            plot.background = element_rect(fill = "black"), 
            plot.title = element_text(colour = "white", 
                                      size = rel(2), 
                                      hjust = 0.5),
            plot.caption = element_text(colour = "white", 
                                        size = rel(1)),
            panel.grid = element_blank(), 
            panel.border = element_blank(), 
            axis.ticks = element_blank(), 
            axis.text = element_blank()) + 
      labs(title = "I found my pleasure at MOD.ORG.AU", 
           caption = "Full code and explanation available at ofchurches.rbind.io")
    
  }
  
  output$shown_graph <- renderPlot({
    
    req(input$goButton_4)
    
    plotInput()
    
  })
  
  output$get_the_item <- renderUI({
    req(input$goButton_4)
    downloadButton('download_item', label = 'Download my pleasures') })
  
  output$download_item <- downloadHandler(
    
    filename = 'pleasure.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotInput(), device = device)
    })
  
}

shinyApp(ui, server)