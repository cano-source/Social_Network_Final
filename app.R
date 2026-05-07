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
      card_header("Ghouls Gender"), "Distribution of character genders in the Monster High network",
      plotOutput("gender_barplot"),
      height = "400px"
      
    ),
  
    
    card(card_header("Monster High Interactive Network"), 
         "Choose your centrality measure and see what ghouls are connected by clicking on any of the nodes. The edges are already weighted for you.", 
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
  
  output$gender_barplot <- renderPlot({
    node_data <- MH_net|>
      activate(nodes)|>
      as_tibble()
    
    gender_counts <- node_data |>
      count(Gender) |>
      arrange(desc(n))
    
    ggplot(gender_counts, aes(x= reorder(Gender, n), y= n))+
      geom_col(fill = "hotpink", color = "pink", alpha = 0.8) +
      geom_text(aes(label = n), hjust = -0.3, size = 5, fontface = "bold", color= "hotpink") +
      coord_flip() +
      labs(
        x = "Gender",
        y = "Count",
        title = "Ghoul's Genders in Monster High Network") +
      theme_minimal()+
      theme( #this all helps it look more appealing 
        plot.background = element_rect(fill= "#000000", color=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
    
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14, color="hotpink"),
        axis.text = element_text(size = 11, color = "hotpink"),
        axis.title = element_text(size = 12, face = "bold", color= "hotpink")
      ) +
      ylim(0, max(gender_counts$n) * 1.1)
    
  })
  
  # CARD 2 
  
  
  # CARD 3 
  
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
