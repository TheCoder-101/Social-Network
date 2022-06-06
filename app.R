# - Original -
# -Libraries-
library(shiny)
library(shinydashboard)
library(igraph)
library(fontawesome)
library(visNetwork)
library(dplyr)
#library(xlsx)
#library(readxl)
#library(motifr)
#library(graphlayouts)
#library(oaqc)
#library(qgraph)

# -Data-
setwd("C:/Users/rohit/Downloads/Internship/R Shiny/Project") # SET THIS FIRST
actoractorEdgelist <- read.csv("Reduced_Actor-Actor Edgelist.csv", header=TRUE)
actorissueEdgelist <- read.csv("Reduced_Actor-Issue Edgelist.csv", header=TRUE)
actorissueEdgelist <- actorissueEdgelist[!apply(actorissueEdgelist == "", 1, all),]
issueissueEdgelist <- read.csv("USETHIS_issue-issue_data.csv", header=TRUE, nrows = 750)

# Issue Names for Some do NOT align with those in the other edge/node lists (Ex. Invasives) (IssueIssueEdgeList)
actordescription <- read.csv("actor_descriptions.csv", header=TRUE)

# -Variables-
uniqueOrgs <- unique(unlist(actorissueEdgelist[1], use.names=FALSE)) # Length: 100
uniquePolicies <- unique(unlist(actorissueEdgelist[2], use.names=FALSE)) # Length: 19
allNodes <- c(uniqueOrgs, uniquePolicies)

nodeEdge1 <- c( 
  unlist(actoractorEdgelist[1], use.names=FALSE), 
  unlist(actorissueEdgelist[1], use.names=FALSE),
  gsub('\\s+', '', (unlist(issueissueEdgelist[1], use.names=FALSE)))
)

nodeEdge2 <- c(
  unlist(actoractorEdgelist[2], use.names=FALSE), 
  unlist(actorissueEdgelist[2], use.names=FALSE),
  gsub('\\s+', '', (unlist(issueissueEdgelist[2], use.names=FALSE)))
)

# -Tab 1 Nodes/Edges-
nodes <- data.frame(
  id = allNodes, 
  label = allNodes, 
  group = c(rep("Organization", (length(uniqueOrgs))), rep("Issue", ((length(uniquePolicies)))))
)

edges <- data.frame(
  from = nodeEdge1, 
  to = nodeEdge2,
  color = c(
    rep("gray", (length(unlist(actoractorEdgelist[1])))), 
    rep("turquoise", (length(unlist(actorissueEdgelist[1])))),
    rep("lightgreen", (length(unlist(issueissueEdgelist[1]))))
  ),
  width = c(
    rep(0.5, (length(unlist(actoractorEdgelist[1])))), 
    rep(0.5, (length(unlist(actorissueEdgelist[1])))),
    unlist(issueissueEdgelist[3], use.names=FALSE)[1:(length(unlist(issueissueEdgelist[1])))]
  ),
  selectionwidth = c(
    rep(0.5, (length(unlist(actoractorEdgelist[1])))), 
    rep(0.5, (length(unlist(actorissueEdgelist[1])))),
    unlist(issueissueEdgelist[3], use.names=FALSE)[1:(length(unlist(issueissueEdgelist[1])))]
  ),
  id = c(paste(nodeEdge1, nodeEdge2, 1:1739, sep="_"))
  #group = c(rep("Organization", (length(actoractorEdgelist[1]))), rep("Issue", ((length(actorissueEdgelist[1])))))
)

# -Shiny UI-
ui <- dashboardPage(
  dashboardHeader(title = "Social Network"),
  dashboardSidebar(
    sidebarMenu(id = "tab", style = "position:fixed;width:220px;",
                # -Sidebar-
                #menuItem("Tab 2", tabName = "tab2", icon = icon("project-diagram")), 
                menuItem("Tab 1", tabName = "tab1", icon = icon("project-diagram")), 
                selectInput("filter", "Filter :", rbind(c("N/A"), nodes$id)),
                checkboxInput("checkOrg", "Show Organizations", c(TRUE, FALSE)),
                checkboxInput("checkIss", "Show Issues", c(TRUE, FALSE)),
                actionButton("reset", "Reset Graphs")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
    tabItems(
      tabItem(tabName = "tab1",
              # -Row 1-
              fluidRow(
                column(width = 3, 
                       box(title = "Instructions", width = NULL, "This map is a full-sandbox network that shows every organization and issue, and all connections that exist between them."),
                       box(title = "Description", width = NULL, htmlOutput('description1'))
                ),
                box(width = 6, height = 850, visNetworkOutput("network_proxy_tab1", height = 800)),
                box(title = "Table", width = 3, DT::dataTableOutput('table1'), downloadButton("downloadTable1", "Download")),
              ),
              
              # -Row 2-
              fluidRow(
                column(width = 3,
                       box(title = "Instructions", width = NULL, "This map is a is based off the selected filter, or the the selected node in the graph above. This network only shows organizations, issues, and their edges, that are connected to the filtered node."),
                       box(title = "Description", width = NULL, htmlOutput('description2'))
                ),
                box(width = 6, height = 850, visNetworkOutput("network_proxy_tab2", height = 800)),
                box(title = "Table", width = 3, DT::dataTableOutput('table2'), downloadButton("downloadTable2", "Download"))
              ),
              
              # -Row 3-
              fluidRow(
                column(width = 3,
                       box(title = "Instructions", width = NULL, "This map is a is based off the selected filter, or the the selected node in the graph above. This network shows all organizations, issues, and their edges, that are connected to the filtered node, and categorizes them by color. green edges represent non-existant edges, Orange edges represent already existing edges, while Green edges represent edges between organizations and issues as before."),
                       box(title = "Description", width = NULL)
                ),
                box(width = 6, height = 850, visNetworkOutput("network_proxy_tab3", height = 800)),
                box(title = "Table", width = 3, DT::dataTableOutput('tableBest3'), downloadButton("downloadTable3", "Download"))
              )
      ),
      tabItem(tabName = "tab2"
              
      )
    )
  )
)

# -Shiny Server-
server <- function(input, output, session) {
  # -Tab 1 Graph-
  output$network_proxy_tab1  <- renderVisNetwork({
    # -Create Filtered Node Set-
    nodes1 <- cbind(nodes)
    if(!input$checkOrg) nodes1 <- nodes1[nodes1$id %in% uniquePolicies, ]
    if(!input$checkIss) nodes1 <- nodes1[nodes1$id %in% uniqueOrgs, ]
    
    visNetwork(nodes1, edges, background = "white") %>%
      
      # -Node Groups-
      visGroups(groupname = "Organization", color = list(border = "blue", background = "blue", highlight = "black")) %>%
      visGroups(groupname = "Issue", color = list(border = "green", background = "green", highlight = "black")) %>%
      
      # -Current Node/Edge Functions-
      visEvents(select = "function(data) {
      Shiny.onInputChange('current_node_id', data.nodes);
      Shiny.onInputChange('current_edges_id', data.edges);
      ;}")  %>%
      
      # -Modifications-
      visNodes(size = 10) %>%
      visEdges(smooth = list(enabled = TRUE, type = "horizontal")) %>%
      visIgraphLayout(randomSeed = 1, layout = "layout_in_circle") %>%
      visInteraction(dragNodes = FALSE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, algorithm = "hierarchical", hover = FALSE)) %>%
      visConfigure(enabled = FALSE) # DEVS
  })
  
  # -Tab 2 Graph-
  output$network_proxy_tab2 <- renderVisNetwork({
    if(!input$filter == "N/A" & !input$filter == "")
    {
      # -Create Filtered Node Set-
      nodesA <- nodes[nodes$id %in% current_connected_nodes$A, ]
      nodesB <- data.frame(
        id = input$filter, 
        label = input$filter, 
        group = "Selected"
      )
      
      nodes2 <- rbind(nodesA, nodesB)
      
      if(!input$checkOrg) nodes2 <- nodes2[nodes2$id %in% uniquePolicies, ]
      if(!input$checkIss) nodes2 <- nodes2[nodes2$id %in% uniqueOrgs, ]
      
      visNetwork(nodes2, edges, background = "white") %>%
        
        # -Node Groups-
        visGroups(groupname = "Organization", color = list(border = "blue", background = "blue", highlight = "black")) %>%
        visGroups(groupname = "Issue", color = list(border = "green", background = "green", highlight = "black")) %>%
        visGroups(groupname = "Selected", color = list(border = "yellow", background = "yellow", highlight = "black")) %>%
        
        visEvents(select = "function(data) {
        Shiny.onInputChange('current_node_id2', data.nodes);
        Shiny.onInputChange('current_edges_id2', data.edges);
        ;}")  %>%
        
        # -Modifications-
        visNodes(size = 10) %>%
        visEdges(smooth = list(enabled = TRUE, type = "horizontal")) %>%
        visIgraphLayout(randomSeed = 1, layout = "layout_in_circle") %>%
        visInteraction(dragNodes = FALSE) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, algorithm = "hierarchical")) %>%
        visConfigure(enabled = FALSE) # DEVS
    }
  })
  
  # - Tab 3 Graph-
  output$network_proxy_tab3  <- renderVisNetwork({
    if(!input$filter == "N/A" & !input$filter == "" & input$filter %in% uniqueOrgs)
    {
      # -Create Filtered Node Set-
      nodesa <- nodes[nodes$id %in% current_connected_nodes$A, ]
      nodesA <- nodesa[nodesa$id %in% uniquePolicies, ] # Policy Nodes
      
      nodesb <- vector()
      for(x in 1:length(nodesA$id))
      {
        nodesb <- append(nodesb, actorissueEdgelist[nodesA$id[x] == actorissueEdgelist[2], 1])
      }
      OrgByIssueCount <- as_data_frame(table(nodesb))
      OrgByIssueCount['n'] <- OrgByIssueCount['n']
      
      nodesb <- unique(nodesb)
      nodesb <- nodesb[nodesb != input$filter]
      
      nodesB <- data.frame( # Organization Nodes
        id = nodesb,
        label = nodesb,
        group = "Organization"
      )
      
      nodesC <- data.frame( # Selected Node
        id = input$filter, 
        label = input$filter, 
        group = "Selected"
      )
      
      nodes2 <- rbind(nodesA, nodesB, nodesC)
      
      if(!input$checkOrg) nodes2 <- nodes2[nodes2$id %in% uniquePolicies, ]
      if(!input$checkIss) nodes2 <- nodes2[nodes2$id %in% uniqueOrgs, ]
      
      #for(x in)
      
      df2$A <- nodes2
      
      setA <- nodesa[nodesa$id %in% uniqueOrgs, ]
      setB <- nodesB[!(nodesB$id %in% setA$id), ]
      
      df3$A <- OrgByIssueCount[OrgByIssueCount$nodesb %in% setB$id,]
      
      newEdges <- data.frame(
        from = c(rep(input$filter, length(setA$id)+length(setB$id)), actorissueEdgelist[,1]),
        to = c(setA$id, setB$id, actorissueEdgelist[,2]),
        color = c(
          rep("darkgreen", length(setA$id)),
          rep("red", length(setB$id)),
          rep("lightgreen", length(actorissueEdgelist[,1]))
        )
      )
      
      visNetwork(nodes2, newEdges, background = "white") %>%
        # -Node Groups-
        visGroups(groupname = "Organization", color = list(border = "blue", background = "blue", highlight = "black")) %>%
        visGroups(groupname = "Issue", color = list(border = "green", background = "green", highlight = "black")) %>%
        visGroups(groupname = "Selected", color = list(border = "yellow", background = "yellow", highlight = "black")) %>%
        
        
        visEvents(select = "function(data) {
        Shiny.onInputChange('current_node_id3', data.nodes);
        Shiny.onInputChange('current_edges_id3', data.edges);
        ;}")  %>%
        
        # -Modifications-
        visNodes(size = 10) %>%
        visEdges(smooth = list(enabled = TRUE, type = "horizontal")) %>%
        visIgraphLayout(randomSeed = 1, layout = "layout_in_circle") %>%
        visInteraction(dragNodes = FALSE) %>%
        visOptions(highlightNearest = list(enabled = TRUE, degree = 1, algorithm = "hierarchical")) %>%
        visConfigure(enabled = FALSE) # DEVS
    }
  })
  
  # -Table-
  output$table1 <- DT::renderDataTable(DT::datatable(nodes, options = list(lengthMenu = 16, pageLength = 16)))
  output$table2 <- DT::renderDataTable(DT::datatable(df1$A, options = list(lengthMenu = 16, pageLength = 16)))
  output$table3 <- DT::renderDataTable(DT::datatable(df2$A, options = list(lengthMenu = 16, pageLength = 16)))
  output$tableBest3 <- DT::renderDataTable(DT::datatable(df3$A, options = list(lengthMenu = 16, pageLength = 16)))
  
  # Variables
  current_connected_nodes <- reactiveValues(A = NULL) # Connected Nodes
  df1 <- reactiveValues(A = NULL) # Data Table Variable
  df2 <- reactiveValues(A = NULL) # Data Table Variable
  df3 <- reactiveValues(A = NULL) # Data Table Variable
  desc1 <- reactiveValues(A = FALSE)
  desc2 <- reactiveValues(A = FALSE)
  
  # Inputs
  resetCheck <- reactive({ list(input$reset) })
  
  # Set: 'current_connected_nodes' / 'df1'
  observe({
    if(length(input$filter) == 1 & !input$filter == "N/A") {
      # Set 'current_connected_nodes'
      visConnectedNodesA <- nodeEdge2[nodeEdge1 %in% input$filter]
      visConnectedNodesB <- nodeEdge1[nodeEdge2 %in% input$filter]
      
      current_connected_nodes$A <- unique(c(visConnectedNodesA, visConnectedNodesB))
      
      # Set 'df1'
      nodesA <- nodes[nodes$id %in% current_connected_nodes$A, ]
      nodesB <- data.frame(id = input$filter, label = input$filter, group = "Selected")
      nodesAB <- rbind(nodesB, nodesA)
      
      if(!input$checkOrg) nodesAB <- nodesAB[nodesAB$id %in% uniquePolicies, ]
      if(!input$checkIss) nodesAB <- nodesAB[nodesAB$id %in% uniqueOrgs, ]
      
      df1$A <- nodesAB
    }
    else
    {
      df1$A <- NULL
    }
  })
  
  # Set 'df2'
  observe({
    if(input$filter == "N/A")
    {
      df2 <- NULL
      df3 <- NULL
    }
  })
  
  # Update: Graph from Filter Box
  observe({
    visNetworkProxy("network_proxy_tab1") %>%
      visUnselectAll() %>%
      visSelectNodes(id = input$filter) %>%
      visFocus(id = input$filter, scale = 1, locked = FALSE, animation = list(duration = 500))
    visNetworkProxy("network_proxy_tab3") %>%
      visUnselectAll() %>%
      visSelectNodes(id = input$filter) %>%
      visFocus(id = input$filter, scale = 1, locked = FALSE, animation = list(duration = 500))
  })
  
  # Update: Filter Box from Graph 1
  observe({
    if(is.null(input$current_node_id)) updateSelectInput(session, "filter", selected = "N/A")
    else updateSelectInput(session, "filter", selected = input$current_node_id)
  })
  
  # Update: Filter Box from Graph 2
  observe({
    if(is.null(input$current_node_id2)) updateSelectInput(session, "filter", selected = "N/A")
    else updateSelectInput(session, "filter", selected = input$current_node_id2)
  })
  
  # Update: Description Box from Issue-Issue Edge
  output$description1 <- renderUI({
    if(!is.null(input$current_node_id)) actordescription[actordescription$actor_list == input$current_node_id, 3]
    else if(!is.null(input$current_edges_id)) 
    {
      node1 <- sub("\\_.*", "", input$current_edges_id)
      node2 <- sub("\\_.*", "", substr(input$current_edges_id, nchar(node1)+2, nchar(input$current_edges_id)))
      
      newList <- data.frame(gsub('\\s+', '', (unlist(issueissueEdgelist[, 1], use.names=FALSE))),
                            gsub('\\s+', '', (unlist(issueissueEdgelist[, 2], use.names=FALSE))), issueissueEdgelist[, 4])
      newList[newList[1] == node1 & newList[2] == node2, 3]
    }
  })
  
  output$description2 <- renderUI({
    if(!is.null(input$current_node_id2)) actordescription[actordescription$actor_list == input$current_node_id2, 3]
    else if(!is.null(input$current_edges_id2)) 
    {
      node1 <- sub("\\_.*", "", input$current_edges_id2)
      node2 <- sub("\\_.*", "", substr(input$current_edges_id2, nchar(node1)+2, nchar(input$current_edges_id2)))
      
      newList <- data.frame(gsub('\\s+', '', (unlist(issueissueEdgelist[, 1], use.names=FALSE))),
                            gsub('\\s+', '', (unlist(issueissueEdgelist[, 2], use.names=FALSE))), issueissueEdgelist[, 4])
      newList[newList[1] == node1 & newList[2] == node2, 3]
    }
  })
  
  output$description3 <- renderUI({
    if(!is.null(input$current_node_id3)) actordescription[actordescription$actor_list == input$current_node_id3, 3]
    else if(!is.null(input$current_edges_id3)) 
    {
      node1 <- sub("\\_.*", "", input$current_edges_id3)
      node2 <- sub("\\_.*", "", substr(input$current_edges_id3, nchar(node1)+2, nchar(input$current_edges_id3)))
      
      newList <- data.frame(gsub('\\s+', '', (unlist(issueissueEdgelist[, 1], use.names=FALSE))),
                            gsub('\\s+', '', (unlist(issueissueEdgelist[, 2], use.names=FALSE))), issueissueEdgelist[, 4])
      newList[newList[1] == node1 & newList[2] == node2, 3]
    }
  })
  
  # Observe: Reset 
  observeEvent(resetCheck(), {
    updateSelectInput(session, "filter", selected = "N/A")
    updateSelectInput(session, "checkOrg", selected = TRUE)
    updateSelectInput(session, "checkIss", selected = TRUE)
    visNetworkProxy("network_proxy_tab1") %>%
      visFit(animation = list(duration=650))
    visNetworkProxy("network_proxy_tab3") %>%
      visFit(animation = list(duration=650))
  })
  
  output$downloadTable1 <- downloadHandler(
    filename = function() {
      paste("table1", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(nodes, file, row.names = FALSE)
    }
  )
  
  output$downloadTable2 <- downloadHandler(
    filename = function() {
      paste("table2", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df1$A, file, row.names = FALSE)
    }
  )
  
  output$downloadTable3 <- downloadHandler(
    filename = function() {
      paste("table3", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(df3$A, file, row.names = FALSE)
    }
  )
}
# -Run-
shinyApp(ui = ui, server = server)