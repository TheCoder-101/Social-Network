# - Original -

# -Libraries-
library(shiny)
library(shinydashboard)
library(visNetwork)
library(igraph)
library(tidyverse)

# -Data-
actoractorEdgelist <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/epn_aa_edgelist.csv"), header=TRUE)
actorissueEdgelist <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/epn_ai_edgelist.csv"), header=TRUE)
issueissueEdgelist <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/ii_edgelist_weights.csv"), header=TRUE)
actordescription <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/epn_actorattributes.csv"), header=TRUE)
issuedescription <- read.csv(url("https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/epn_issueattributes.csv"), header=TRUE)
names(issuedescription) <- c('full_issue', 'abbr_issue', 'projects')

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

# -Media-
dataURL = a("Data Entry Form", target="_blank", href="https://osu.az1.qualtrics.com/jfe/form/SV_cBxbSrIhRLx77lY")
channnelURL = a("Youtube Channel", target="_blank", href="https://www.google.com/")
coverVidURL = "https://www.youtube.com/embed/a7h6gI-yNpo"
imageURL = "https://students.cfaes.ohio-state.edu/sites/ap/files/events/SENR_CFAES.jpg"
legendURL = "https://raw.githubusercontent.com/SENS-Lab/ActorIssue_Network_Tool/main/NoS_Tool_Legend.png"

# -Shiny UI Variables-
abbrevNames <- c("N/A", actordescription[,2], issuedescription[,2])
fullNames <- c("N/A", actordescription[,1], issuedescription[,1])
names(abbrevNames) <- fullNames

abbrevNames2 <- c("N/A", actordescription[,2])
fullNames2 <- c("N/A", actordescription[,1])
names(abbrevNames2) <- fullNames2

# -Shiny UI-
ui <- dashboardPage(
  dashboardHeader(title = "CENT", titleWidth = 250),
  dashboardSidebar(
    sidebarMenu(id = "sidebar", style = "position: fixed;",
                
      # Tabs List
      menuItem("Home", tabName = "coverpage"),
      menuItem("Networks", tabName = "networkstab"),
      menuItem("Actor Profiles", tabName = "actorprofiletab")
    ),
    
    # Cover Page
    conditionalPanel(style = "position:fixed; width:230px; margin-top:125px;",
      condition = "input.sidebar == 'coverpage'"
    ),
    
    # Network Tab
    conditionalPanel(style = "position:fixed; width:230px; margin-top:125px;",
      condition = "input.sidebar == 'networkstab'",
      selectInput("filter", "Select Organization or Issue:", c(("N/A"), abbrevNames), selected = "N/A"),
      checkboxInput("checkOrg", "Show Organizations"),
      checkboxInput("checkIss", "Show Issues"),
      hr(),
      actionButton("reset", "Reset Networks")
    ),
    
    # Actor Profile Tab
    conditionalPanel(style = "position:fixed; width:230px; margin-top:125px;",
     condition = "input.sidebar == 'actorprofiletab'",
      selectInput("filterClone", "Select Organization or Issue:", c(("N/A"), abbrevNames2), selected = "")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
    tabItems(
      tabItem(tabName = "coverpage",
        # -Row 1-
        fluidRow(
          titlePanel(div(h1("Columbus Environmental Networking Tool (CENT)", align = "center"),
                         h4("A data hub for environmental collaboration in Central Ohio", align = "center"), 
          tags$div(style = "display: flex; justify-content: center")))
        ),
        
        fluidRow(
          tags$img(src = "", width = "130px", height = "170px", align = "center"), tags$br()
           ),
        
        fluidRow(
          column(width = 8, align = "center", offset = 2, length = 20, 
                 box(title = "About", width = NULL,
                     tags$div(tags$p(style="text-align: left; font-size:17px", "Columbus Environmental Networking Tool (CENT) is a one-stop-shop for enhancing collaboration between environmental organizations in Central Ohio. Designed by Columbus-based graduate students, CENT provides an up-to-date overview of the Columbus environmental landscape.", tags$b("We created CENT with one central objective in mind: to provide up-to-date data to hard-working Columbus environmental professionals to facilitate productive collaboration."), "To do this, we provide you with personalized partnership recommendations and other data. We strive to support collaborative decision-making capacity!"),
                              tags$p(style="text-align: left; font-size:17px", "We’re always looking to update our database and add in new organizations to the network. To do so, we need your help to complete a “Data Entry Form” below on this page."),
                              tags$p(style="text-align: left; font-size:17px", "Check out our 'Networks' tab to walk through the data! Please reach out to us if you have any questions, concerns, or ideas! Our contact information is below. Happy networking!")
                              
        )))),
        fluidRow(
          column(width = 6, align = "center",
                 box(title = "Frequently Asked Questions", width = NULL, 
                     tags$div(tags$p(style="text-align: left;", tags$b("Question:"), "What is a social network?", tags$br(), tags$b("Answer:"), "A social network is a way to represent a social system by using 'nodes' to represent social actors, and 'links' to represent the relationships between them. On this website, we use two different types of nodes: Organizations and Environmental Issues. The edges (lines) that connect nodes to each other represent collaboration between organizations (social-social), an organizating managing an environmental issue (social-ecological), or ecological connectivity between two environmental issues (ecological-ecological)."), tags$hr(),
                              tags$p(style="text-align: left;", tags$b("Question:"), "Where does this data come from?", tags$br(), tags$b("Answer:"), "All organizational data, including an organization's partners, issues, and description were collected during interviews with an organization representative. All organizations displayed have granted us permission to show their partnership and issue management data."), tags$hr(),
                              tags$p(style="text-align: left;", tags$b("Question:"), "My organization isn't on here, my organization's data is incorrect, or I want to take my organization off of this site. What do I do?", tags$br(), tags$b("Answer:"), "If you believe your organization's data is incorrect or if you'd like to add your organization to this website, follow the 'Data Entry Form' link in the box below. If you would like to remove your organization from this site, please email us directly at harrisonfried98@gmail.com."), tags$hr(),
                              tags$p(style="text-align: left;", tags$b("Question:"), "Do I need to be a network expert to use network data?", tags$br(), tags$b("Answer:"), "Not at all! We set up the instructions on the 'Networks' tab in a way that should allow you to follow along. If you need additional help navigating this tool, or brainstorming potential uses, reach out to us and we can set up a time to chat!"), tags$hr(),
                              tags$p(style="text-align: left;", tags$b("Question:"), "What is a 'collaborative gap' and why should I care?", tags$br(), tags$b("Answer:"), "Collaborative gaps occur when two organizations work on the same management issue but do not coordinate with each other. Closing collaborative gaps could prevent organizations from re-inventing the wheel and can make our efforts more efficient and synergistic."), tags$hr(),
                              tags$p(style="text-align: left;", tags$b("Question:"), "What is a 'coordination delegate' and how do I become one?", tags$br(), tags$b("Answer:"), "A coordination delegate is an employee of an organization who has volunteered themselves to be their organization's 'network representative'. These individuals can serve as the first point of contact when a prospective partner is interested in reaching out and their email is displayed on their organization's profile. You can indicate that you're interested in being your organization's coordination delegate by completing the 'Data Entry Form' below, or by emailing Harrison Fried directly."), tags$hr(),
                              tags$p(style="text-align: left;", tags$b("Question:"), "So... what's next?", tags$br(), tags$b("Answer:"), "We are always looking to add new features to CEN and we're working on a few at the moment. Stay tuned for updates!")
                              
                     ))),
          column(width = 6, box(title = "CENT at Environmental Professionals Network, 9/22", tags$iframe(src = coverVidURL, width = "100%", height = 750), width = NULL, height = 810))
        ),
        fluidRow(
          column(8, align = "center", offset = 2, box(title = "Update your organization's information!", width = NULL, HTML(paste(dataURL, sep = '<br/>'))))
        ),
        fluidRow(
          column(width = 8, align = "center", offset = 2, length = 20, 
                 box(title = "Contributors", width = NULL, 
                      tags$div(tags$p(style="text-align: left;", tags$img(src = "https://senr.osu.edu/sites/senr/files/styles/profile_image/public/profile_images/harrison_fried.JPG?itok=nJ7GUden", width = "130px", height = "170px", align = "right"), tags$em("Co-Founder and Main Contact"), tags$br(), tags$b("Harrison Fried, PhD Candidate, The Ohio State University"), tags$br(), "Harrison is passionate about the benefits of strategic collaboration for improving local environmental management. His research focuses on how people work together to solve complex environmental problems, such as climate change adaptation in Columbus. He approaches his research through a social-ecological network lens, focusing on complex social and ecological interconnections that can create barriers or opportunities for resilient management. Harrison plans to continue working on applied research that addresses complexity in modern-day governance through improved access to information. Harrison received his Bachelor of Science and Master of Science in Environment and Natural Resources from Ohio State University’s School of Environment and Natural Resources.", tags$br(), tags$em("Email: harrisonfried98@gmail.com"), tags$hr()), 
                               tags$p(style="text-align: left;", tags$img(src = "https://senr.osu.edu/sites/senr/files/styles/profile_image/public/profile_images/Kimberly%20Ordonez.JPG?itok=_PocXtDE", width = "130px", height = "170px", align = "right"), tags$em("Co-Founder"), tags$br(), tags$b("Kimberly Ordonez, PhD Candidate, The Ohio State University"), tags$br(),"Kimberly holds an interest in co-productive science, multi-scale behavioral adaptations to environmental pressures, and environmental justice. Her current projects include Climate Change related adaptations by environmental stakeholders in Ohio including Nongovernmental Organizations, private businesses, nonprofits, and federal offices. Her research goals involve expanding her studies into environmental justice and localized community acceptance of climate change adaptations. Kimberly hopes to one day work in a position of translational science, engaging communities, and scientists alike, to assist in fair and realistic behavioral changes for a positive socio-ecological outcome. She received her Bachelor of Science in Wildlife and Master of Science in Forestry from Purdue University.", tags$br(), tags$em("Email: ordonez.33@osu.edu"), tags$hr()), 
                               tags$p(style="text-align: left;", tags$img(src = "https://epn.osu.edu/sites/epn/files/imce/Rohit%20Basu.jpg", width = "130px", height = "170px", align = "right"), tags$em("Lead Programmer"), tags$br(), tags$b("Rohit Basu, Grove City High School"), tags$br(), "Rohit is a high school student in Grove City, Ohio. He initially got involved in social-ecological research during his sophomore year of high school. Rohit has applied his expansive computer programming and software knowledge to fully develop the CENT infrastructure. When he graduates high school, Rohit plans to continue his education at a university to study biomedical engineering."))))
          
        ),
        
        
        # fluidRow(
        #   column(8, align = "center", offset = 2, box(title = "Contributors", width = NULL, tags$b("This text is bold."))
        # ),
        # fluidRow(
        #   column(width = 6, align = "center", box(tags$iframe(src = coverVidURL, width = "100%", height = 300), width = NULL, height = 325))
        # ),
      ),
      tabItem(tabName = "networkstab",
        # -Row 1-
        fluidRow(
          box(width = NULL, align = 'center',
              tags$b("Explore the 3 network sections below by following the guide on the left of the page. For best results, we recommend using the full-screen option on a computer."))
        ),
              
        # -Row 1-
        fluidRow(
          column(width = 3, align = "center",
                 box(title = "Instructions: Network 1", width = NULL,
                     tags$b("Looking into a small part of the network."), tags$br(), tags$hr(),
                     tags$div(tags$p(style="text-align: center;", "In this first network, you will see only the nodes that have a connection to your selected organization/issue. In other words, you're looking at the personal network of your selection."),
                                tags$p(style="text-align: center;", tags$b("1) Select an organization or environmental issue from the drop-down menu to the left"), ", and then", tags$b("2) toggle the 'Show Organizations' and 'Show Issues' check-boxes"), "to see how the network changes! Try selecting your own organization, too!"),
                     )),
                 box(title = "Description of Selected Node/Link", width = NULL, htmlOutput('description2')),
                 tags$h3("Legend", width = "30%", height = "30%", align = "right"),
                 tags$img(src = legendURL, width = "60%", height = "60%", align = "right")
          ),
          box(width = 6, height = 850, visNetworkOutput("network_proxy_tab2", height = 800)),
          box(title = "Table: Network 1", width = 3, DT::dataTableOutput('table2')),
        ),
        
        # -Row 2-
        fluidRow(
          column(width = 3, align = "center",
                 box(title = "Instructions: Network 2", width = NULL,
                     tags$b("Taking a look at the whole Columbus network."), tags$br(), tags$hr(),
                     tags$div(tags$p(style="text-align: center;", "In this second network, all nodes are shown. The result is a complex circle of connections.", tags$b("3) Select an organization from the drop-down menu to the left"), "and it will automatically be highlighted on the network. Zoom in and out using your mouse or cursor to see different parts of the network."),
                              tags$p(style="text-align: center;", tags$b("4) Toggle off the 'Show Organizations' check-box to the left."), "You'll now see a network of 19 environmental issues.", tags$b("5) Click on an edge between a pair of issues"), "to learn more about how they are interdependent. We collected this data from a group of experts in Ohio."),
                     )),                 
                 box(title = "Description of Selected Node/Link", width = NULL, htmlOutput('description1')),
                 tags$h3("Legend", width = "30%", height = "30%", align = "right"),
                 tags$img(src = legendURL, width = "60%", height = "60%", align = "right")
          ),
          box(width = 6, height = 850, visNetworkOutput("network_proxy_tab1", height = 800)),
          box(title = "Table: Network 2", width = 3, DT::dataTableOutput('table1')),
        ),
        
        # -Row 3-
        fluidRow(
          column(width = 3, align = "center",
                 box(title = "Instructions: Network 3", width = NULL,
                     tags$b("Addressing your collaborative gaps."), tags$br(), tags$hr(),
                     tags$div(tags$p(style="text-align: center;", "In this third network, ", tags$b("6) select your organization from the drop-down menu."), "The result is a circle-shaped network of every organization who shares a management issue with your organization. If there is a red connection between your organization and another, it means that you do not collaborate despite sharing an issue in common. We refer to this as a 'collaborative gap'. Dark green connections indicate that there is collaboration."), 
                              tags$p(tags$b(" 7) Take a look at the 'Recommended Partners Table' to the right"), "to see which potential partners share the most issues in common (i.e., gaps)."),
                              tags$p(style="text-align: center;", "You will probably notice that this network is highly complex. To make it more simple,", tags$b("8) select a specific issue from the 'Filter Issue' drop-box below"), ", which will narrow down the network to a specific environmental issue and the organizations who manage it."),
                              tags$p(style="text-align: center;", "The purpose of this third network is to showcase the world of potential partners who work on an issue in common with you, because they might make good partners.", tags$b("9) Click on the node of a potential partner"), "to learn more about them, or visit their profile on the 'Actor Profiles' tab."),
                              tags$p(style="text-align: center;", tags$b("10) Write down a list of potential partners"), "that seem like a good fit and check out their profile on the 'Actor Profiles' tab. Once you're done,", tags$b("11) update your organization's data by completing the data entry form link on the 'Home' page.")),
                              
                               )),   
                 box(title = "Description of Selected Node/Edge", width = NULL, htmlOutput('description3')),
                 box(width = NULL, selectInput("issueFilter", "Filter Issue :", "N/A", selected = "N/A")),
                 box(width = NULL, selectInput("orgFilter", "Filter Organization :", "N/A", selected = "N/A")),
                 tags$h3("Legend", width = "30%", height = "30%", align = "right"),
                 tags$img(src = legendURL, width = "60%", height = "60%", align = "right")
          ),
          box(width = 6, height = 850, visNetworkOutput("network_proxy_tab3", height = 800)),
          box(title = "Table: Network 3", width = 3, tags$em("In this table, the most highly recommended partners for the selected organization will appear at the top. This is based on the number of collaborative gaps that can be closed through partnership."), DT::dataTableOutput('table3'))
        )
      ),
      tabItem(tabName = "actorprofiletab",
        column(width = 5,
          box(title = "", width = NULL, htmlOutput('logo')),
          box(title = "Coordination Delegate", width = NULL, htmlOutput('coordinatedelegate')),
          box(title = "Organization Description", width = NULL, htmlOutput('actordescription')),
          box(title = "Organization Type", width = NULL, htmlOutput('actortype')),
          box(title = "Website", width = NULL, uiOutput('actorwebsite')),
          box(title = "This Organization's Most Recent Data Update", width = NULL, htmlOutput('lastupdated'))
        ),
        box(width = 6, title = "Recommended Partners for this Organization", tags$em("In the table below, we recommend potential partners based upon who works on the greatest number of issues in common with you (i.e., 'gaps')."), DT::dataTableOutput('table4')),
        visNetworkOutput("network_proxy_tab4", height = 0)
      )
    )
  )
)

# -Shiny Server-
server <- function(input, output, session) {
  # --- Sidebar Tab 2 ---
  
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
      
      # -Current Node/Link Variables-
      visEvents(select = "function(data) {
      Shiny.onInputChange('current_node_id1', data.nodes);
      Shiny.onInputChange('current_edges_id1', data.edges);
      ;}")  %>%
      
      #visLegend(addEdges = legendEdges1) %>%
      
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
    if(identical(!input$filter == "N/A", !input$filter == ""))
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
  output$network_proxy_tab3 <- renderVisNetwork({
    if(identical(!input$filter == "N/A", !input$filter == "", input$filter %in% uniqueOrganizations))
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
      
      # -Table-
      temp <- OrgByIssueCount[OrgByIssueCount$orgsWithIssuesb %in% unconnectedOrgEdges$id,]
      actorNameCol <- actordescription[actordescription$actor_list %in% temp$orgsWithIssuesb, 1:2]
      temp <- merge(temp, actorNameCol, by.x = "orgsWithIssuesb", by.y = 'actor_list', all = TRUE)
      tableDataFrame$table3 <- temp[, c(1,3,2)]
      
      # -Graph-
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
  output$table1 <- DT::renderDataTable(DT::datatable(tableDataFrame$table1, colnames = c("Abbrev.","Name","Type"), rownames = FALSE, selection = 'single', options = list(lengthChange = FALSE, pageLength = 11)))
  output$table2 <- DT::renderDataTable(DT::datatable(tableDataFrame$table2, colnames = c("Abbrev.","Name","Type"), rownames = FALSE, selection = 'single', options = list(lengthChange = FALSE, pageLength = 11)))
  output$table3 <- DT::renderDataTable(DT::datatable(tableDataFrame$table3, colnames = c("Abbrev.","Name", "Gaps"), rownames = FALSE, selection = 'single', options = list(lengthChange = FALSE, pageLength = 11, order = list(2, 'desc'))))
  output$table4 <- DT::renderDataTable(DT::datatable(tableDataFrame$table4, colnames = c("Abbrev.","Name", "Gaps"), rownames = FALSE, selection = 'single', options = list(lengthChange = FALSE, pageLength = 11, order = list(2, 'desc'))))
  
  # Variables
  currentConnectedNodes <- reactiveValues(N = NULL, N2 = NULL) # Connected Nodes
  tableDataFrame <- reactiveValues(table1 = NULL, table2 = NULL, table3 = NULL)
  connectedNodesGraph3 <- reactiveValues(A = NULL, B = NULL) # A: Issues, B: Organizations
  
  # Inputs
  resetCheck <- reactive({ list(input$reset) })
  
  # Set: [currentConnectedNodes]
  observe({
    if(identical(length(input$filter) == 1, !input$filter == "N/A")) {
      connectedNodesTo <- nodeEdgeTo[nodeEdgeFrom %in% input$filter]
      connectedNodesFrom <- nodeEdgeFrom[nodeEdgeTo %in% input$filter]
      currentConnectedNodes$N <- unique(c(connectedNodesTo, connectedNodesFrom))
    }
    else {
      currentConnectedNodes$N <- NULL
    }
  })
  
  # Set: [tableDataFrame$table1]
  observe({
    actorNameCol <- actordescription[actordescription$actor_list %in% allNodes$id, c(1:2,4)]
    issueNameCol <- issuedescription[issuedescription$abbr_issue %in% allNodes$id, 1:2]
    if(nrow(issueNameCol) != 0) issueNameCol['type'] <- NA
    colnames(issueNameCol) <- c('ï..name', 'actor_list', 'type')
    temp <- bind_rows(actorNameCol, issueNameCol)
    
    tableDataFrame$table1 <- temp[, c(2,1,3)]
  })
  
  # Set: [tableDataFrame$table2]
  observe({
    if(identical(length(input$filter) == 1, !input$filter == "N/A")) {
      nodesA <- allNodes[allNodes$id %in% currentConnectedNodes$N, ]
      nodesB <- data.frame(id = input$filter, label = input$filter, group = "Selected")
      nodesAB <- rbind(nodesB, nodesA)
      
      nodesAB <- subset(nodesAB, select = -c(label, group))
      actorNameCol <- actordescription[actordescription$actor_list %in% nodesAB$id, c(1:2,4)]
      issueNameCol <- issuedescription[issuedescription$abbr_issue %in% nodesAB$id, 1:2]
      if(nrow(issueNameCol) != 0){
        issueNameCol['type'] <- NA
        colnames(issueNameCol) <- c('ï..name', 'actor_list', 'type')
      } 
      
      temp <- bind_rows(actorNameCol, issueNameCol)
      tableDataFrame$table2 <- temp[, c(2,1,3)]
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
  
  # Update: Graph 1 Description Box
  output$description1 <- renderUI({
    if(!is.null(input$current_node_id1))
    {
      if(input$current_node_id1 %in% uniquePolicies) issuedescription[issuedescription$abbr_issue == input$current_node_id1, 3]
      else actordescription[actordescription$actor_list == input$current_node_id1, 3]
    }
    else if(!is.null(input$current_edges_id1)) 
    {
      node1 <- sub("\\_.*", "", input$current_edges_id1)
      node2 <- sub("\\_.*", "", substr(input$current_edges_id1, nchar(node1)+2, nchar(input$current_edges_id1)))
      
      newList <- data.frame(gsub('\\s+', '', (unlist(issueissueEdgelist[, 1], use.names=FALSE))),
                            gsub('\\s+', '', (unlist(issueissueEdgelist[, 2], use.names=FALSE))), issueissueEdgelist[, 4])
      newList[newList[1] == node1 & newList[2] == node2, 3]
    }
  })
  
  # Update: Graph 2 Description Box
  output$description2 <- renderUI({
    if(!is.null(input$current_node_id2))
    {
      if(input$current_node_id2 %in% uniquePolicies) issuedescription[issuedescription$abbr_issue == input$current_node_id2, 3]
      else actordescription[actordescription$actor_list == input$current_node_id2, 3]
    }
    else if(!is.null(input$current_edges_id2)) 
    {
      node1 <- sub("\\_.*", "", input$current_edges_id2)
      node2 <- sub("\\_.*", "", substr(input$current_edges_id2, nchar(node1)+2, nchar(input$current_edges_id2)))
      
      newList <- data.frame(gsub('\\s+', '', (unlist(issueissueEdgelist[, 1], use.names=FALSE))),
                            gsub('\\s+', '', (unlist(issueissueEdgelist[, 2], use.names=FALSE))), issueissueEdgelist[, 4])
      newList[newList[1] == node1 & newList[2] == node2, 3]
    }
  })
  
  # Update: Graph 3 Description Box 
  output$description3 <- renderUI({
    if(!is.null(input$current_node_id3))
    {
      if(input$current_node_id3 %in% uniquePolicies) issuedescription[issuedescription$abbr_issue == input$current_node_id3, 3]
      else actordescription[actordescription$actor_list == input$current_node_id3, 3]
    }
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
  
  # --- Sidebar Tab 3 ---
  
  # Match filter inputs between Sidebar Tab 2&3
  observeEvent(input$filter, {
    if(any(uniqueOrganizations==input$filter)) {
      updateSelectInput(session, "filterClone", selected = input$filter)
    }
  })
  
  output$coordinatedelegate <- renderUI({
    actordescription[actordescription$actor_list %in% input$filterClone, "coordination_delegate"]
  })
  
  output$actordescription <- renderUI({
    actordescription[actordescription$actor_list %in% input$filterClone, "desc"]
  })
  
  output$actortype <- renderUI({
    actordescription[actordescription$actor_list %in% input$filterClone, "type"]
  })
  
  output$actorwebsite <- renderUI({
    if(input$filterClone != "N/A")
    {
      link <- actordescription[actordescription$actor_list %in% input$filterClone, "link"]
      tagList(a(link, href=link, style = "word-wrap: break-word;"))
    }
    else
    {}
  })
  
  output$lastupdated <- renderUI({
    actordescription[actordescription$actor_list %in% input$filterClone, "last_updated"]
  })
  
  output$logo <- renderText({
    link <- actordescription[actordescription$actor_list %in% input$filterClone, "logo"]
    paste(c('<img width = 100% height = 100% src="', link, '">'))
  })
  
  output$network_proxy_tab4 <- renderVisNetwork({
    if(identical(!input$filterClone == "N/A", !input$filterClone == "", input$filterClone %in% uniqueOrganizations))
    {
      
      # -Create Filtered Node Set-
      connectedNodesa <- allNodes[allNodes$id %in% currentConnectedNodes$N2, ]
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
      orgsWithIssuesb <- orgsWithIssuesb[orgsWithIssuesb != input$filterClone]
      
      orgNodesWithIssuesB <- data.frame( # Organization Nodes
        id = orgsWithIssuesb,   
        label = orgsWithIssuesb,
        group = "Organization"
      )
      
      selectedNodeC <- data.frame( # Selected Node
        id = input$filterClone, 
        label = input$filterClone, 
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
        from = c(rep(input$filterClone, length(connectedOrgEdges$id)+length(unconnectedOrgEdges$id)), actorissueEdgelist[,1]),
        to = c(connectedOrgEdges$id, unconnectedOrgEdges$id, actorissueEdgelist[,2]),
        color = c(
          rep("darkgreen", length(connectedOrgEdges$id)),
          rep("red", length(unconnectedOrgEdges$id)),
          rep("lightgreen", length(actorissueEdgelist[,1]))
        )
      )
      
      # -Table-
      temp <- OrgByIssueCount[OrgByIssueCount$orgsWithIssuesb %in% unconnectedOrgEdges$id,]
      actorNameCol <- actordescription[actordescription$actor_list %in% temp$orgsWithIssuesb, 1:2]
      temp <- merge(temp, actorNameCol, by.x = "orgsWithIssuesb", by.y = 'actor_list', all = TRUE)
      tableDataFrame$table4 <- temp[, c(1,3,2)]
      
      # -Graph-
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
  
  # Set: [currentConnectedNodes]
  observe({
    if(identical(length(input$filterClone) == 1, !input$filterClone == "N/A")) {
      connectedNodesTo <- nodeEdgeTo[nodeEdgeFrom %in% input$filterClone]
      connectedNodesFrom <- nodeEdgeFrom[nodeEdgeTo %in% input$filterClone]
      currentConnectedNodes$N2 <- unique(c(connectedNodesTo, connectedNodesFrom))
    }
    else {
      currentConnectedNodes$N2 <- NULL
    }
  })
}
# -Run-
shinyApp(ui = ui, server = server)