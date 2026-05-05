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


library(visNetwork)

ui <-fluidPage(
  
  titlePanel("Monster High Network"),
  
  page_sidebar(
    title = "subtitle here", 
    sidebar = sidebar ("Menu options"), 
    card(
      card_header("Here's where I would introduce your project"), "put some content here"),
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
    
    
    card(card_header("An interactive network?!"), 
         "we can use the package VisNetwork to make it happen", 
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
      rename(from = 1, to =2 )
    
    list(nodes = MH_nodes, edges = MH_edges)
  })
  
  output$int_network <- renderVisNetwork({
    net2 <- network2()
    nodes <- net2$nodes
    edges <- net2$edges 
    
    
    visNetwork(nodes, net2$edges) |> 
      
      visNodes(label =NULL, # this makes labels seen only when clicked 
               title = nodes$Label, 
               borderWidth = 1,
               font = list(
                 size = 14,
                 color ="hotpink",
                 background = "white"
                 ),
               color = list(
                 background= "pink", 
                 border = "black", 
                 highlight =  "hotpink"))|>
      
      visEdges(
        color = list(color = "grey", highlight = "black")) |> 
      
      visOptions(
        highlightNearest = list(enabled = TRUE, hover = TRUE), 
        nodesIdSelection = FALSE) |>
      
      visInteraction(
        dragNodes = TRUE, 
        dragView = TRUE, 
        zoomView = TRUE) |> 
      
      visPhysics(solver= "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -200),stabilization = TRUE)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
