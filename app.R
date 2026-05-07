# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(bslib)

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

library(palmerpenguins)
library(visNetwork)
library(bslib)#this is for background 

ui <-fluidPage(
  theme = bs_theme(
    bg = "#000000",
    fg = "#ff2787",
    primary = "#ffffff",
    base_font = font_google("Roboto")
  ),
  
  titlePanel("Monster High Network"),
  page_sidebar(
    title = "by Bianca Cano for Social Network 0226 (Work in progress)", 
    sidebar = sidebar ("Menu options"), 
    card(
      card_header("Project information"), "This is an Undirected Monster High Network based on movies and webisodes during the Generation 1 era, except Great Scarier reef. This network contains 113 nodes and 500 edges. And has attributes such as gender and the type of monster they are. "),
    card(
      card_header("Dynamic Demo 1"), "you could put a caption like so",
      selectInput("select", 
                  "select an option", #the a and b down below determines output
                  choices = list("Option A" = "A", 
                                 "Option B" = "B"),
                  selected =1), 
      textOutput("ourVariable")
    ), 
    
    card(card_header("here's a network!"),
         selectInput("size",
                     "choose a centrality measure", 
                     choices = list("Degree Centrality" = "degree", 
                                    "Betweenness Centrality" = "betweenness"), 
                     selected = 1), 
         plotOutput("example_network"), height = "400px"),
    
    
    #card(
   #   card_header("Monster Type"),
    #  "Distribution of different monster types in the network",
     # radioButtons("monster_plot_type",
      #             "Plot Type:",
       #            choices = c("Count" = "count",
        #                       "Average Degree by Type" = "avg_degree",
         #                      "Average Betweenness by Type" = "avg_betweenness"),
          #         selected = "count"),
   #   plotOutput("monster_type_plot"),
    #  height = "500px"
    #),
  # new code ends here  
    
    
    
    card(card_header("Monster High Interactive Network"), 
         "Choose your centrality measure and see what ghouls are connected by clicking on any of the nodes", 
         radioButtons("size_by", "Centrality Measure", 
                      choices = c("Degree" = "degree", 
                                  "Betweenness Centrality" = "betweenness"), 
                      selected = "degree"),
         visNetworkOutput("int_network"), height = "600px")
  )
)


# Section 2. The server section defines how our app works. Here's where we will put all the network analysis. 

server <- function(input, output) {
  
  # CARD 1 
  
  output$ourVariable <- renderText({
    paste("Our selected option is", input$select)
  })
  
  # let's create a simple example network with 10 nodes and calulate the degree centrality
  
  
  
  
  
  
  
  
  # CARD 2 
  
  MH_nodes <- read.csv("MonsterHigh_nodes.csv")
  MH_edges <- read.csv("MonsterHigh_edges.csv")
  
  MH_net <- tbl_graph(nodes = MH_nodes, 
                      edges= MH_edges,
                      directed = FALSE) 
  MH_net
  
  MH_net <- MH_net |> activate(nodes) |> 
    mutate(degree = centrality_degree( loops = TRUE),
           betweeness_raw = centrality_betweenness(normalized = FALSE), 
           betweeness_norm = centrality_betweenness(normalized = TRUE),
           closeness = centrality_closeness(), 
           eigenvector = centrality_eigen(weights = Weight))
  
  network <- reactive({
    #ex_net <- play_gnp(n = 10, p = 0.5, directed = FALSE)
    
    MH_net <- MH_net |> 
      as_tbl_graph()|> 
      activate(nodes) |> 
      mutate(
        degree = centrality_degree(), 
        betweenness = centrality_betweenness())
    
    MH_net
  })
  
  # now let's get it visualized and reactive to our choice from above! 
  
  output$example_network <- renderPlot({
    MH_net <- network() 
    
    p<- ggraph(MH_net, layout = "auto") +
      geom_edge_link(alpha = 0.3, color = "grey80") + 
      geom_node_point(aes(size = .data[[input$size]]),
                      color = "pink") + 
      scale_size_continuous(range = c(.5, 10)) + 
      labs(Nodes = input$size) + 
      theme_graph()
    #don't forget to call it or it will not work
    p
  })
  
  output$example_network <- renderPlot({
    MH_net <- network() 
    
    p<- ggraph(MH_net, layout = "auto") +
      geom_edge_link(alpha = 0.3, color = "grey80") + 
      geom_node_point(aes(size = .data[[input$size]]),
                      color = "pink") + 
      scale_size_continuous(range = c(.5, 10)) + 
      labs(Nodes = input$size) + 
      theme_graph()
    #don't forget to call it or it will not work
    p
  })
  
  # NEW: BARPLOT OUTPUT
  
#  output$attribute_barplot <- renderPlot({
    # Get node data with all attributes
 #   node_data <- MH_net |>
#      activate(nodes) |>
 #     as_tibble() |>
  #    mutate(node_name = if("Label" %in% names(.)) Label else name)
    
    # Filter to top N if checkbox is selected
  #  if (input$show_top_n) {
  #    node_data <- node_data |>
   #     arrange(desc(.data[[input$bar_attribute]])) |>
    #    slice_head(n = 10)
   # }
    
    # Create barplot
  #  ggplot(node_data, aes(x = reorder(node_name, .data[[input$bar_attribute]]), 
     #                     y = .data[[input$bar_attribute]])) +
   #   geom_col(fill = "hotpink", color = "black", alpha = 0.8) +
    #  coord_flip() +
     # labs(
      #  x = "Node",
       # y = input$bar_attribute,
  #      title = paste("Node", input$bar_attribute)
   #   ) +
    #  theme_minimal() +
     # theme(
      #  plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
       # axis.text = element_text(size = 10),
      #  axis.title = element_text(size = 12, face = "bold")
      #)
#  })
  
  
  
  

  
  
  
  # CARD 3 
  
  # we're going to use another example network like from above but visNetwork requires separate edge and nodes lists 
  
  network2 <- reactive({
    
    MH_net <- MH_net |> 
      as_tbl_graph()|> 
      activate(nodes) |> 
      mutate(
        degree = centrality_degree(), 
        betweenness = centrality_betweenness())
    
    MH_nodes <- MH_net |> 
      activate(nodes) |> 
      as_tibble() |> 
      rowid_to_column("id") |> 
      mutate(value = if (input$size_by == "degree") degree else betweenness,
             label = Label) # have to give size based on "value" for visNetwork
    
    MH_edges <- MH_net |> 
      activate(edges) |> 
      as_tibble() |> 
      rename(from = 1, to =2 )|>
      mutate(
        value = Weight
      )
    
    list(nodes = MH_nodes, edges = MH_edges)
  })
  
  output$int_network <- renderVisNetwork({
    net2 <- network2()
    nodes <- net2$nodes
    edges <- net2$edges 
    
    
    visNetwork(nodes, edges) |> 
      
      visNodes( 
               borderWidth = 2,
               font = list(
                 size = 14,
                 color ="hotpink",
                 background = "white"
                 ),
               color = list(
                 background= "pink", 
                 border = "black", 
                 highlight = "hotpink"
                                   
                 ))|>
      
      visEdges(
        smooth = TRUE,
        color = list(color = "grey", highlight = "hotpink")) |> 
      
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE), 
        nodesIdSelection = FALSE) |>
      
      visInteraction(
        dragNodes = TRUE, 
        dragView = TRUE, 
        zoomView = TRUE,
        ) |> 
      
      visPhysics(solver= "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -200),stabilization = TRUE)
    
  })
}
  

# Run the application 
shinyApp(ui = ui, server = server)
