# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)
library(bslib)#this is for background 

ui <-fluidPage(
  theme = bs_theme(
    bg = "#000000",
    fg = "hotpink",
    primary = "#ffffff",
    base_font = font_google("Roboto")
  ),
  
  titlePanel("Monster High Network"),
  page_sidebar(
    title = "by Bianca Cano for Social Network 0226", 
    card(
      card_header("Project information"), 
      p("This is an Undirected Monster High Network based on movies and webisodes during the Generation 1 era, except Great Scarier reef. This network contains 113 nodes and 500 edges. And has attributes such as gender and the type of monster they are. This network was built by watching the movies in order that corresponded to Generation 1. 
      I took notes on paper whenever characters mention someone else by name, if they shared the same scene, or if they were visible in the background. This did not include the openings to the movies as it was often very repetitive, as well as if it was too hard to identify characters since the scene would be zoomed out or if the way the scene was framed only showed a part of the character such as legs and arms. 
      It did count if one characters face was visible and the other was “turned around” facing them. For characters that had siblings or were dating if it was known that they called each other brother/sister or a common nickname I would also tally it. There are some characters that are strictly background characters or go from background characters to a recurring character. 
      So, to deal with this a character would not get tallied until they spoke or were brought up by another character. I also had to reassess if the characters where in the same spot if it should be counted for an additional tally. In which I changed it so going further a tally didn’t count unless it was either a change in the conversation topic or they took different angles.
      A big help with this project was the Monster High Wiki page that helped me asses what type of monster the characters where as well as some of the names for certain characters I forgot."),
      p("Characters. Monster High Wiki, Fandom, https://monsterhigh.fandom.com/wiki/Characters"),
      card_header("Data Analysis"),  
      p("Something you will see throughout this project will be how the ghouls would interact with one another throughout the show and movie.
        The main spotlight for this project is the last network that showcases degree in centrality and betweenness centrality. It shows us what ghoul has more direct connections through the size of the node. 
        This graph also showcases the weight between characters in this context being when a character was next to another in a scene or they said their name. In the context of monster high we mainly follow 4 ghouls in each movie and 
        in the show, which is why when you look at the network 4 characters will have big nodes while the rest vary in size. And while it doesn't work as I expected it to work, the community cluster network allows us to see if 
        communities are formed based off one gender and another monster type. But because many of these characters are connected with one another it makes it a bit harder to interpret because there are various characters that are part of many communities.
        However it is interesting that each community looking at the gender one, has one person designated to connecting people which goes based of people that haven't been grouped yet. This is interesting because while many might expect for the same monsters to be grouped
        into communities we don't really see this in the network we see them fairly more dispersed. As for gender it does seem to be definitely more communities where gender is more prominent but this might be due to the fact that there are more female characters then male characters. 
        "),
      p("Some of these graphs take a minute to load so please be patient with them. Thank You!!!"),
      
    ),
    
    card(
      card_header("Ghouls Gender and Type of Monster Barplot: "), "This is a distribution of characters gender and monster type in the Monster High network. Toggle between Gender and Monster Type, to see how many Female and Male characters there are in the Monster High network as well
      as what type of monster is often more created through the storyline, movies, and show",
      radioButtons("plot_type", "Choose attribute:",
                   choices = c("Gender" = "gender",
                               "Monster Type" = "monster"),
                   selected = "gender",
                   inline = TRUE),
      plotOutput("attribute_barplot"),
      height = "850px"
      
    ),
    
    card( 
      card_header("Community Cluster Network Visualization: "), 
      p("Explore how gender and monster types impact detected community cluster groups, Communities will show as the boarder colors of the nodes and Node inside color will represent gender/monster type within each community.
      Toggle between Gender and Monster Type, to see if there is a connection between communities and gender/Monster Types. Or in other words are more groups likely to be formed based on similar gender/monster_type."),
      p("For gender: if you double click on a specific community it will disperse all the respective members in that community. After if you double click on a character it will close the community again. There is also a reinitialize clustering button that will 
      reset the network and get you back to the start. Legend to the side to help you distinguish what color is for what gender after a community is double clicked but this can also be zoomed out if it causes confusion.
      "),
      p("For Monster Type: Looking from afar allows for you to see the different communities based on color, but once you zoom into a specific node if you double click it will show the nodes name, Community number, and the type of monster they are.
        This also allows you to see if a node might be in another group as once you click a node it highlights the nodes they are connected to."
      ),
          radioButtons("comm_attributes", "Color by: ", 
                       choices = c("Gender"= "gender",
                                   "Monster Type" = "monster")),
        visNetworkOutput("community_network", height = "700px")
        ),
      
    
    card(card_header("Monster High Interactive Network: "), 
         "Toggle between degree centrality and betweenness centrality, to see what ghouls are connected by clicking on any of the nodes. The edges are weighted to showcase what ghouls have a stronger connection, as they had more screen time or meantioned each other more frequently.
         This demonstartes how monster high focuses on 4 main characters and has built various other charcaters around them. As when you toggle between centralities our main 4 keep their nodes big, showing that there isn't just one main ghoul who runs the movies and show.", 
         radioButtons("size_by", "Centrality Measure", 
                      choices = c("Degree" = "degree", 
                                  "Betweenness Centrality" = "betweenness"), 
                      selected = "degree"),
         visNetworkOutput("int_network"), height = "900px")
  ))


# Section 2. The server section defines how our app works. Here's where we will put all the network analysis. 

server <- function(input, output) {
  
  MH_nodes <- read.csv("MonsterHigh_nodes.csv")
  MH_edges <- read.csv("MonsterHigh_edges.csv")
  
  monster_colors <- c(
    "W" = "#654321", "H" = "#FADED5", "V" = "#FF0090", "M" = "#1F7483", "T" = "#8C83D8","G" = "#B3CCE7", "S" = "#59BFC6",
    "WCH" = "#C6DC66", "SP" = "#D9EBD3", "E" = "#CE4628", "RV" = "#CDE8EB", "O" = "#C8C2E4", "Z"= "#2263AB", "P" = "#94C862",
    "MY" = "#E1A91B", "GG" = "#EE99C4", "MX" = "#A32856", "R"= "#E992A5", "I" = "#AD2B22", "D" = "#C46D50", "A"= "#4C64AF", 
    "SW" = "#A5DBD4", "GAR" = "#C8C2C3", "HY" = "#E2E119", "IN" = "#0C3148", "RB" = "#BA8647", "GN" = "#F9C8CB", "YT"= "#B7E0F6", 
    "SK" = "#F7923C", "WC"= "#EB5232", "PT" = "#036CB9"
  )
  
  monster_labels <- c(
    "W" = "Wolf", "H" = "Human","V" = "Vampire", "M" = "Mummy", "T" = "Troll", "G" = "Ghost", "S" = "Sea", "WCH" = "Witch", 
    "SP" = "Special", "E" = "Element", "RV" = "River", "O" = "Ogres", "Z"= "Zombies", "P" = "Plant", "MY" = "Mythological",
    "GG" = "Gorgan", "MX" = "Maricoxi", "R"= "Rodent", "I" = "Insect", "D" = "Deer", "A"= "Alien", "SW" = "Swamp", "GAR" = "Gargoyle", 
    "HY" = "Hybrid", "IN" = "Invisible", "RB" = "Robot", "GN" = "Genie", "YT"= "Yeti", "SK" = "Skeleton", "WC"= "Werecat","PT" = "Pet"
  )
  
  MH_net <- tbl_graph(nodes = MH_nodes, 
                      edges= MH_edges,
                      directed = FALSE) 
  MH_net
  
  MH_net <- MH_net |> 
    activate(nodes) |> 
    mutate(
      degree = centrality_degree( loops = TRUE),
      betweeness_raw = centrality_betweenness(normalized = FALSE), 
      betweeness_norm = centrality_betweenness(normalized = TRUE),
      closeness = centrality_closeness(), 
      eigenvector = centrality_eigen(weights = Weight))
  
  #Card 1
  output$attribute_barplot <- renderPlot({
    node_data <- MH_net|>
      activate(nodes)|>
      as_tibble()
    
   if (input$plot_type == "gender") {
    counts <- node_data |>
      count(Gender) |>
      arrange(desc(n))
    
    ggplot(counts, aes(x= reorder(Gender, n), y= n, fill = Gender))+
      geom_col(color = "pink", alpha = 0.8) +
      geom_text(aes(label = n), hjust = -0.3, size = 5, fontface = "bold", color= "hotpink") +
      scale_fill_manual(values = c("F"= "#EE008E", "M"= "#02AFF0", "UN" ="#FFFFFF" ))+
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
        axis.title = element_text(size = 12, face = "bold", color= "hotpink"),
        legend.position = "none"
      ) +
      ylim(0, max(counts$n) * 1.1)
    
   }else {
     #Monster_type
    counts <- node_data |>
      count(Monster_Type) |>
      arrange(desc(n))
    
    counts <-counts |>
      mutate(Monster_Label = monster_labels[Monster_Type])
    
    ggplot(counts, aes(x= reorder(Monster_Label, n), y= n, fill = Monster_Type))+
      geom_col(color = "black", alpha = 0.8) +
      geom_text(aes(label = n), hjust = -0.3, size = 5, fontface = "bold", color= "hotpink") +
      scale_fill_manual(
        values = monster_colors
      )+
      coord_flip()+
      labs(
        x = "Monster Type",
        y = "Count",
        title = "Monster Types in Monster High Network") +
      theme_minimal()+
      theme(
        plot.background = element_rect(fill= "#000000", color=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14, color="hotpink"),
        axis.text = element_text(size = 11, color = "hotpink"),
        axis.title = element_text(size = 12, face = "bold", color= "hotpink"),
        legend.position = "none"
      ) +
      ylim(0, max(counts$n) * 1.1)
  }
  })
  
  # CARD 2
  #had help from shiny AI since i had originally had it as a normal network and had 
  #issues transforming it to a VisNetwork
  
  output$community_network <- renderVisNetwork({
    g <- MH_net |> as.igraph()
    communities <-cluster_louvain(g)
    
    nw_comm <- MH_net|>
      activate(nodes)|>
      mutate( community= as.factor(membership(communities)))
    
    num_communities <- length(unique(membership(communities)))
    community_colors <- rainbow(num_communities)
    names(community_colors) <- 1:num_communities
    
    nodes_df <- nw_comm|>
      activate(nodes)|>
      as_tibble()|>
      mutate(
        id = row_number(),
        label = Label,
        group = community,
        borderWidth = 2, 
        color.border = community_colors[as.numeric(community)]
      )
    edges_df <- nw_comm|>
      activate(edges)|>
      as_tibble()|>
      rename(from =1, to =2)
      
    if (input$comm_attributes == "gender"){
      nodes_df <- nodes_df|>
        mutate(
          color.background = case_when(
            Gender == "F"~"#EE008E",
            Gender == "M" ~ "#02AFF0",
            Gender == "UN"~ "#FFFFFF",
            TRUE ~ "#CCCCCC"
          ),
          title = paste0("Community: ", community, "<br>Gender: ", Gender)
        )
          visNetwork(nodes_df, edges_df)|>
            visNodes(shape = "dot", size= 20, borderWidth = 3,
                     font = list(
                       size = 30,
                       color ="hotpink",
                       background = "white"
                     ))|>
            visEdges(color = list(color="pink", opacity = 0.15), width = 0.5)|>
            visPhysics(solver= "forceAtlas2Based",
                       forceAtlas2Based = list(gravitationalConstant = -200),stabilization = TRUE)|>
            visLayout(randomSeed = 123)|>
            visOptions(
              highlightNearest = list(enabled = TRUE, hover = TRUE),
              nodesIdSelection = FALSE
            )|>
            visClusteringByGroup(
              groups = unique(nodes_df$group),
              label = "Community",
              shape = "database",
              color = community_colors,
              force = FALSE)|>
            
            visLegend(
              addNodes = list(
                list(label = "Female", color = "#EE008E", shape ="dot", size = 15),
                list(label = "Male", color = "#02AFF0", shape ="dot", size = 15),
                list(label = "Unkown", color = "#FFFFFF", shape ="dot", size = 15)
              ),
           useGroups = FALSE,
           width = 0.15)
          
      
    } else{
      nodes_df <- nodes_df|>
        mutate(color.background = monster_colors[Monster_Type],
               title= paste0("<b>", Label, "</b><br>", "Community: ", community, 
                             "<br>", "Type: ", monster_labels[Monster_Type]))
      visNetwork(nodes_df, edges_df)|>
        visNodes(shape = "dot", size= 20, borderWidth = 3,
                 font = list(
                   size = 30,
                   color ="hotpink",
                   background = "white"
                 ))|>
        visEdges(color = list(color="pink", opacity = 0.15), width = 0.5)|>
        visPhysics(solver= "forceAtlas2Based",
                   forceAtlas2Based = list(gravitationalConstant = -200),stabilization = TRUE)|>
        visLayout(randomSeed = 123)|>
        visOptions(
          highlightNearest = list(enabled = TRUE, hover = TRUE),
          nodesIdSelection = FALSE
        )|>
        visGroups(groupname = "monster_labels", color= monster_colors)
    }
      })
  
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
               borderWidth = 1,
               font = list(
                 size = 30,
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
        zoomView = TRUE
        ) |> 
      
      visPhysics(solver= "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -200),stabilization = TRUE)
  })
}

  

# Run the application 
shinyApp(ui = ui, server = server)
