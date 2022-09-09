# - Original -

# -Libraries-
library(shiny)
library(shinydashboard)
library(visNetwork)
library(igraph)

# -Data-
actoractorEdgelist <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/epn_aa_edgelist.csv"), header=TRUE)
actorissueEdgelist <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/epn_ai_edgelist.csv"), header=TRUE)
issueissueEdgelist <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/ii_edgelist_weights.csv"), header=TRUE)
actordescription <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/epn_actorattributes.csv"), header=TRUE)

# -Variables-
uniqueOrganizations <- unique(unlist(actorissueEdgelist[1], use.names=FALSE)) # Length: 100
uniquePolicies <- unique(unlist(actorissueEdgelist[2], use.names=FALSE)) # Length: 19
uniqueElements <- c(uniqueOrganizations, uniquePolicies)

nodeEdgeFrom <- c( 
  unlist(actoractorEdgelist[1], use.names=FALSE), 
  unlist(actorissueEdgelist[1], use.names=FALSE),
  gsub('\\s+', '', (unlist(issueissueEdgelist[1], use.names=FALSE)))
)

nodeEdgeTo <- c(
  unlist(actoractorEdgelist[2], use.names=FALSE), 
  unlist(actorissueEdgelist[2], use.names=FALSE),
  gsub('\\s+', '', (unlist(issueissueEdgelist[2], use.names=FALSE)))
)

edgeLength = length(nodeEdgeFrom)

# -Tab 1 Nodes/Edges-
allNodes <- data.frame(
  id = uniqueElements, 
  label = uniqueElements, 
  group = c(rep("Organization", (length(uniqueOrganizations))), rep("Issue", (length(uniquePolicies))))
)

allEdges <- data.frame(
  from = nodeEdgeFrom, 
  to = nodeEdgeTo,
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
  id = c(paste(nodeEdgeFrom, nodeEdgeTo, 1:edgeLength, sep="_"))
)

# -Shiny UI-
ui <- dashboardPage(
  dashboardHeader(title = "Network Tool"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar", style = "position:fixed;width:220px;",
                
      # -Tab 1-
      selectInput("filter", "Select Organization or Issue:", rbind(c("N/A"), allNodes$id)),
      checkboxInput("checkOrg", "Show Organizations", c(TRUE, FALSE)),
      checkboxInput("checkIss", "Show Issues", c(TRUE, FALSE)),
      hr(),
      actionButton("reset", "Reset Networks")
    )
  ),
  dashboardBody(
    
    # -Row 1-
    fluidRow(
      column(width = 3, 
        box(title = "Instructions: Full Network", width = NULL, "This network is a 'full-sandbox' network that shows every organization and issue, and all connections that exist between them. Clicking on any node will select that organization/issue in the drop down menu."),
        box(title = "Description of Selected Node or Linkage", width = NULL, htmlOutput('description1'))
      ),
      box(width = 6, height = 850, visNetworkOutput("network_proxy_tab1", height = 800)),
      box(title = "Full Network Table", width = 3, DT::dataTableOutput('table1')),
    ),
    
    # -Row 2-
    fluidRow(
      column(width = 3,
        box(title = "Instructions: Filtered Network", width = NULL, "This network only displays all connected nodes to the selected organization/issue. This effectively shows a sub-network of the network above (Full Network)."),
        box(title = "Description of Selected Node or Linkage", width = NULL, htmlOutput('description2'))
      ),
      box(width = 6, height = 850, visNetworkOutput("network_proxy_tab2", height = 800)),
      box(title = "Filtered Network Table", width = 3, DT::dataTableOutput('table2'))
    ),
    
    # -Row 3-
    fluidRow(
      column(width = 3,
        box(title = "Instructions: Gaps Network", width = NULL, "This network requires an organization to be selected from the drop down menu. This network shows all issues connected to the selected organization (from the drop down menu), and it shows all organizations connected to that set of issues (regardless of their connection to the selected organization). The network can be further filtered to only include organizations connected to a certain issue, which can be selected in the drop down menu below. The table on the right displays the amount of issues an organization and the selected organization have in common (Frequency), and it can be sorted by selecting the column header."),
        box(title = "Description of Selected Node or Linkage", width = NULL, htmlOutput('description3')),
        box(width = NULL, selectInput("issueFilter", "Filter Issue :", "N/A", selected = "N/A")),
        box(width = NULL, selectInput("orgFilter", "Filter Organization :", "N/A", selected = "N/A"))
      ),
      box(width = 6, height = 850, visNetworkOutput("network_proxy_tab3", height = 800)),
      box(title = "Gaps Network Table", width = 3, DT::dataTableOutput('table3'))
    )
  )
)

# -Shiny Server-
server <- function(input, output, session) {
  
  # -Tab 1 Graph-
  output$network_proxy_tab1  <- renderVisNetwork({
    
    # -Create Filtered Node Set-
    nodes1 <- allNodes
    if(!input$checkOrg) nodes1 <- nodes1[nodes1$id %in% uniquePolicies, ]
    if(!input$checkIss) nodes1 <- nodes1[nodes1$id %in% uniqueOrganizations, ]
    
    visNetwork(nodes1, allEdges, background = "white") %>%
      
    # -Node Groups-
    visGroups(groupname = "Organization", color = list(border = "blue", background = "blue", highlight = "black")) %>%
    visGroups(groupname = "Issue", color = list(border = "green", background = "green", highlight = "black")) %>%
    
    # -Current Node/Edge Variables-
    visEvents(select = "function(data) {
    Shiny.onInputChange('current_node_id1', data.nodes);
    Shiny.onInputChange('current_edges_id1', data.edges);
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
      connectedNodesA <- allNodes[allNodes$id %in% currentConnectedNodes$N, ]
      selectedNodeB <- data.frame(
        id = input$filter, 
        label = input$filter, 
        group = "Selected"
      )
      
      nodes2 <- rbind(connectedNodesA, selectedNodeB)
      
      if(!input$checkOrg) nodes2 <- nodes2[nodes2$id %in% uniquePolicies, ]
      if(!input$checkIss) nodes2 <- nodes2[nodes2$id %in% uniqueOrganizations, ]
      
      visNetwork(nodes2, allEdges, background = "white") %>%
        
      # -Node Groups-
      visGroups(groupname = "Organization", color = list(border = "blue", background = "blue", highlight = "black")) %>%
      visGroups(groupname = "Issue", color = list(border = "green", background = "green", highlight = "black")) %>%
      visGroups(groupname = "Selected", color = list(border = "yellow", background = "yellow", highlight = "black")) %>%
      
      # -Current Node/Edge Variables-
      visEvents(select = "function(data) {
      Shiny.onInputChange('current_node_id2', data.nodes);
      Shiny.onInputChange('current_edges_id2', data.edges);
      ;}")  %>%
      
      # -Modifications-
      visNodes(size = 10) %>%
      visEdges(smooth = list(enabled = TRUE, type = "horizontal")) %>%
      visIgraphLayout(randomSeed = 1, layout = "layout_nicely") %>%
      visInteraction(dragNodes = FALSE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, algorithm = "hierarchical")) %>%
      visConfigure(enabled = FALSE) # DEVS
    }
  })
  
  # - Tab 3 Graph-
  output$network_proxy_tab3  <- renderVisNetwork({
    if(!input$filter == "N/A" & !input$filter == "" & input$filter %in% uniqueOrganizations)
    {
      # -Create Filtered Node Set-
      connectedNodesa <- allNodes[allNodes$id %in% currentConnectedNodes$N, ]
      connectedIssuesA <- NULL
      
      if(!input$issueFilter == "N/A") connectedIssuesA <- connectedNodesa[connectedNodesa$id %in% input$issueFilter, ] 
      else connectedIssuesA <- connectedNodesa[connectedNodesa$id %in% uniquePolicies, ] 

      orgsWithIssuesb <- vector() # Vector of all Instances of Organizations with Issues in [connectedIssuesA]
      for(x in 1:length(connectedIssuesA$id))
      {
        orgsWithIssuesb <- append(orgsWithIssuesb, actorissueEdgelist[connectedIssuesA$id[x] == actorissueEdgelist[2], 1])
      }
      
      if(!input$orgFilter == "N/A") orgsWithIssuesb <- orgsWithIssuesb[orgsWithIssuesb %in% input$orgFilter]
      OrgByIssueCount <- as.data.frame(table(orgsWithIssuesb)) # For Graph 3 Table
      
      orgsWithIssuesb <- unique(orgsWithIssuesb) # [orgsWithIssuesb] filters to unique Instances of Organizations
      orgsWithIssuesb <- orgsWithIssuesb[orgsWithIssuesb != input$filter]
      
      orgNodesWithIssuesB <- data.frame( # Organization Nodes
        id = orgsWithIssuesb,   
        label = orgsWithIssuesb,
        group = "Organization"
      )
      
      selectedNodeC <- data.frame( # Selected Node
        id = input$filter, 
        label = input$filter, 
        group = "Selected"
      )
      
      connectedNodesGraph3$A <- connectedIssuesA$id
      connectedNodesGraph3$B <- orgNodesWithIssuesB$id
      nodes3 <- rbind(connectedIssuesA, orgNodesWithIssuesB, selectedNodeC)
      
      if(!input$checkOrg) nodes3 <- nodes3[nodes3$id %in% uniquePolicies, ]
      if(!input$checkIss) nodes3 <- nodes3[nodes3$id %in% uniqueOrganizations, ]
      
      # -Create New Edge Set-
      connectedOrgEdges <- connectedNodesa[connectedNodesa$id %in% uniqueOrganizations, ]
      unconnectedOrgEdges <- orgNodesWithIssuesB[!(orgNodesWithIssuesB$id %in% connectedOrgEdges$id), ]
      
      newEdges <- data.frame(
        from = c(rep(input$filter, length(connectedOrgEdges$id)+length(unconnectedOrgEdges$id)), actorissueEdgelist[,1]),
        to = c(connectedOrgEdges$id, unconnectedOrgEdges$id, actorissueEdgelist[,2]),
        color = c(
          rep("darkgreen", length(connectedOrgEdges$id)),
          rep("red", length(unconnectedOrgEdges$id)),
          rep("lightgreen", length(actorissueEdgelist[,1]))
        )
      )
      
      # -Table and Graph-
      tableDataFrame$table3 <- OrgByIssueCount[OrgByIssueCount$orgsWithIssuesb %in% unconnectedOrgEdges$id,]
      visNetwork(nodes3, newEdges, background = "white") %>%
      
      # -Node Groups-
      visGroups(groupname = "Organization", color = list(border = "blue", background = "blue", highlight = "black")) %>%
      visGroups(groupname = "Issue", color = list(border = "green", background = "green", highlight = "black")) %>%
      visGroups(groupname = "Selected", color = list(border = "yellow", background = "yellow", highlight = "black")) %>%
      
      # -Current Node/Edge Variables-
      visEvents(select = "function(data) {
      Shiny.onInputChange('current_node_id3', data.nodes);
      Shiny.onInputChange('current_edges_id3', data.edges);
      ;}")  %>%
      
      # -Modifications-
      visNodes(size = 10) %>%
      visEdges(smooth = list(enabled = TRUE, type = "horizontal")) %>%
      visIgraphLayout(randomSeed = 1, layout = "layout_in_circle") %>%
      visInteraction(dragNodes = FALSE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, algorithm = "hierarchical"), manipulation = TRUE) %>%
      visConfigure(enabled = FALSE) # DEVS
    }
  })
  
  # -Table-
  output$table1 <- DT::renderDataTable(DT::datatable(tableDataFrame$table1, colnames = c("Abrv."), rownames = FALSE, selection = 'single', options = list(lengthChange = FALSE, pageLength = 16)))
  output$table2 <- DT::renderDataTable(DT::datatable(tableDataFrame$table2, colnames = c("Abrv."), rownames = FALSE, selection = 'single', options = list(lengthChange = FALSE, pageLength = 16)))
  output$table3 <- DT::renderDataTable(DT::datatable(tableDataFrame$table3, colnames = c("Abrv.", "Gaps"), rownames = FALSE, selection = 'single', options = list(lengthChange = FALSE, pageLength = 16)))
  
  # Variables
  currentConnectedNodes <- reactiveValues(N = NULL) # Connected Nodes
  tableDataFrame <- reactiveValues(table1 = subset(allNodes, select = -c(label, group)), table2 = NULL, table3 = NULL)
  connectedNodesGraph3 <- reactiveValues(A = NULL, B = NULL) # A: Issues, B: Organizations
  
  # Inputs
  resetCheck <- reactive({ list(input$reset) })
  
  # Set: [currentConnectedNodes]
  observe({
    if(length(input$filter) == 1 & !input$filter == "N/A") {
      connectedNodesTo <- nodeEdgeTo[nodeEdgeFrom %in% input$filter]
      connectedNodesFrom <- nodeEdgeFrom[nodeEdgeTo %in% input$filter]
      currentConnectedNodes$N <- unique(c(connectedNodesTo, connectedNodesFrom))
    }
    else {
      currentConnectedNodes$N <- NULL
    }
  })
  
  # Set: [tableDataFrame$table2]
  observe({
    if(length(input$filter) == 1 & !input$filter == "N/A") {
      nodesA <- allNodes[allNodes$id %in% currentConnectedNodes$N, ]
      nodesB <- data.frame(id = input$filter, label = input$filter, group = "Selected")
      nodesAB <- rbind(nodesB, nodesA)
      
      nodesAB <- subset(nodesAB, select = -c(label, group))
      tableDataFrame$table2 <- nodesAB
    }
    else {
      tableDataFrame$table2 <- NULL
    }
  })
  
  # Set: [tableDataFrame$table3]
  observe({
    if(input$filter == "N/A")
    {
      tableDataFrame$table3 <- NULL
    }
  })
  
  # Update: Graph 1 from Filter Box
  observe({
    visNetworkProxy("network_proxy_tab1") %>%
      visUnselectAll() %>%
      visSelectNodes(id = input$filter) %>%
      visFocus(id = input$filter, scale = 1, locked = FALSE, animation = list(duration = 500))
  })
  
  # Update: Filter Box from Graph 1
  observe({
    if(is.null(input$current_node_id1)) updateSelectInput(session, "filter", selected = "N/A")
    else updateSelectInput(session, "filter", selected = input$current_node_id1)
  })

  # Update: Graph 1 Description Box from Issue-Issue Edge
  output$description1 <- renderUI({
    if(!is.null(input$current_node_id1)) actordescription[actordescription$actor_list == input$current_node_id1, 3]
    else if(!is.null(input$current_edges_id1)) 
    {
      node1 <- sub("\\_.*", "", input$current_edges_id1)
      node2 <- sub("\\_.*", "", substr(input$current_edges_id1, nchar(node1)+2, nchar(input$current_edges_id1)))
      
      newList <- data.frame(gsub('\\s+', '', (unlist(issueissueEdgelist[, 1], use.names=FALSE))),
                            gsub('\\s+', '', (unlist(issueissueEdgelist[, 2], use.names=FALSE))), issueissueEdgelist[, 4])
      newList[newList[1] == node1 & newList[2] == node2, 3]
    }
  })
  
  # Update: Graph 2 Description Box from Issue-Issue Edge
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
  
  # Update: Graph 3 Description Box from Issue-Issue Edge
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
  
  # Update: Graph 3 Issue/Organization Filter Box 
  observeEvent(input$filter,{
    updateSelectInput(session, "issueFilter", choices = "N/A", selected = "N/A")
    updateSelectInput(session, "orgFilter", choices = "N/A", selected = "N/A")
  })
  # Update: Graph 3 Issue/Organization Filter Box N/A Reset
  observe({
    if(input$issueFilter == "N/A") updateSelectInput(session, "issueFilter", choices = c("N/A", connectedNodesGraph3$A))
    if(input$orgFilter == "N/A") updateSelectInput(session, "orgFilter", choices = c("N/A", connectedNodesGraph3$B))
  })
  
  # Observe: Graph 1
  observeEvent(input$table1_cell_clicked, {
    a <- tableDataFrame$table1[input$table1_rows_selected,1]
    if(length(a) == 1)
    {
      visNetworkProxy("network_proxy_tab1") %>%
        visFocus(id = a, scale = 1, locked = FALSE, animation = list(duration = 500))
    }
  })
  # Observe: Graph 2
  observeEvent(input$table2_cell_clicked, {
    a <- tableDataFrame$table2[input$table2_rows_selected,1]
    if(length(a) == 1)
    {
      visNetworkProxy("network_proxy_tab2") %>%
        visFocus(id = a, scale = 2, locked = FALSE, animation = list(duration = 500))
    }
  })
  # Observe: Graph 3
  observeEvent(input$table3_cell_clicked, {
    a <- tableDataFrame$table3[input$table3_rows_selected,1]
    if(length(a) == 1)
    {
      visNetworkProxy("network_proxy_tab3") %>%
        visFocus(id = a, scale = 1, locked = FALSE, animation = list(duration = 500))
    }
  })
  
  # Observe: Reset 
  observeEvent(resetCheck(), {
    updateSelectInput(session, "filter", selected = "N/A")
    updateSelectInput(session, "checkOrg", selected = TRUE)
    updateSelectInput(session, "checkIss", selected = TRUE)
    visNetworkProxy("network_proxy_tab1") %>%
      visFit(animation = list(duration=650))
  })
}
# -Run-
shinyApp(ui = ui, server = server)