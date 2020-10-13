## app.R ##
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)  # load css 
library(rintrojs) # introBox
library(shinyMatrix) # transition matrix
library(shinyThings)
library(shinyWidgets)
library(igraph)
library(dplyr)
library(network)

library(visNetwork)
library(ggnetwork)
library(ggpubr)
# library(networkD3)

library(DiagrammeR)
library(dagitty)
library(texPreview)
library(shinyAce)
library(shinyBS)
library(ggdag)
library(reshape2)

source("R/module/clickpad.R")
source("R/columns.R")
source("R/node.R")
source("R/functions.R")
source("R/func/gMCP_xc2.R")


components <- list(toolbar = list())

# Components - examples ----
components$examples <- list()

components$examples$team <- tagList(
    h3("Development Team"),
    tags$ul(
        tags$li(tags$a(href = "https://www.mrc-bsu.cam.ac.uk/people/in-alphabetical-order/n-to-s/david-robertson/",
                       "David Robertson")),
        tags$li("Xijin Chen")
    ),
    ### TODO 
    p(
        "All code and detailed instructions for usage is available on GitHub",
        tags$a(
            href = "https://github.com/xijin0911")
    ),
    ### TODO 
    p(
        "If you have any questions or comments, we would love to hear them.",
        "You can email us at ",
        tags$a(href = "chenxijin2017@gmail.com", "chenxijin2017@gmail.com"),
    )
)


# Components - Build ----
components$build <- box(
    title = "Graphical approach for multiple test procedures",
    id = "build-box",
    width = 12,
    fluidRow(
        id = "shinydag-toolbar",
        tags$div(
            class = "col-xs-12 col-md-5 shinydag-toolbar-actions",
            tags$div(
                class = "col-xs-12 col-sm-6 col-md-12",
                id = "shinydag-toolbar-node-list-action",
                components$toolbar$node_list_action
            ),
            tags$div(
                class = "col-xs-12 col-sm-6 col-md-12",
                style = "padding: 10px",
                id = "shinydag-toolbar-clickpad-action",
                components$toolbar$clickpad_action
            )
        ),
        tags$div(
            class = "col-xs-12 col-md-7",
            components$toolbar$node_list_name
        )
    ),
    fluidRow(
        column(
            width = 12,
            tags$div(
                class = "pull-left",
                uiOutput("node_list_helptext")
            ),
            shinyThings::undoHistoryUI(
                id = "undo_rv", 
                class = "pull-right",
                back_text = "Undo",
                fwd_text = "Redo"
            )
        )
    ),
    fluidRow(
        column(
            width = 12,
            clickpad_UI("clickpad", height = "600px", width = "100%")
        )
    ),
    if (getOption("shinydag.debug", FALSE)) fluidRow(
        column(width = 12, shinyThings::undoHistoryUI_debug("undo_rv"))
    ),
    fluidRow(
        tags$div(
            class = class_3_col,
            selectInput("exposureNode", "Exposure", choices = c("None" = ""), width = "100%")
        ),
        tags$div(
            class = class_3_col,
            selectInput("outcomeNode", "Outcome", choices = c("None" = ""), width = "100%")
        ),
        tags$div(
            class = class_3_col,
            selectizeInput("adjustNode", "Adjust for...", choices = c("None" = ""), width = "100%", multiple = TRUE)
        )
    ),
    fluidRow(
        tags$div(
            class = "col-sm-12 col-md-9 col-lg-6",
            uiOutput("dagExposureOutcomeDiagnositcs")
        )
    )
)

# -----------------------------------------------------


ui <- dashboardPage(
    title = "shinyGraph",
    skin = "black",
    dashboardHeader(
        title = "GraphApp",
        tags$li(
            class = "dropdown",
            actionLink(
                inputId = "._bookmark_", 
                label = "Bookmark",
                icon = icon("link", lib = "glyphicon"),
                title = "Bookmark GraphApp's state and get a URL for sharing.",
                `data-toggle` = "tooltip",
                `data-placement` = "bottom"
            )
        ),
        tags$li(
            class = "dropdown",
            tags$a(
                href = "https://github.com/xijin0911/",
                title = "shinyDAG on GitHub",
                target = "_blank",
                icon("github")
            )
        )
    ),
    dashboardSidebar(
        sidebarMenu(
            id = "shinydag_page",
            menuItem("Tweak", tabName = "tweak", icon = icon("sliders")),
            menuItem("Graph", tabName = "graph", icon = icon("home")),
            menuItem("LaTeX", tabName = "latex", icon = icon("file-text-o")),
            menuItem("Examples", tabName = "examples", icon = icon("info"))
        )
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$script(src = "shinydag.js", async = TRUE),
        # todo: what these css can do
        includeCSS("www/AdminLTE.gerkelab.min.css"),
        includeCSS("www/_all-skins.gerkelab.min.css"),
        includeCSS("www/shinydag.css"),
        chooseSliderSkin("Flat", "#418c7a"),
        tabItems(
            #---
            # tweak
            #---
            tabItem(
                tabName="tweak", 
                ## 1. hypotheses & alpha 
                box(
                    width = 12,
                    
                    box(witdth = 6, collapsible = FALSE,
                    solidHeader = TRUE,
                    collapsed = TRUE,
                    numericInput(inputId="Number_Hypotheses",
                                 label="Number of Hypotheses:",
                                 value=3,step = 1,min = 1)),
                    box(witdth = 6, collapsible = FALSE,
                           solidHeader = TRUE,
                           collapsed = TRUE,
                        numericInput(inputId = "alpha", 
                                     label = HTML("&alpha;"),
                                     value = 0.05,step = 0.001,min = 0)),
                    uiOutput("uioutput_Tmatrix")),
                
                br(),
                br(),
                      box(width=12,
                          actionButton("TestButton", "Testing!"),
                          conditionalPanel(condition = "input.TestButton != 0",plotOutput("ResultPlot")),
                          br(),
                          br(),
                          p("Initial and final graph")),
                a(id = "Moreinformation", "More information about the result"),
                hidden(
                    div(id = "Moreinfor",
                        box(width=3,"Resulting weights",
                        tableOutput("extend1")),
                        box(width=6,"Resulting Transition Matrix",
                            tableOutput("extend2")),
                        box(width=3,"Resulting Adjusted p-values",
                            tableOutput("extend3"))
                    )
                )
                ),
            #---
            # graph
            #---
            tabItem(
                tabName = "graph",
                fluidPage(
#                    useShinyjs(),
                    fluidRow(
                      column(6,
                             selectInput(inputId = "Weighting_Strategy",
                                         label = "Weighting Strategy",                                
                                         choices = c(
                                           "Bonferroni-Holm procedure","Fixed sequence test","Fallback procedure"),
                                         selected = "Bonferroni-Holm procedure"),
                    visNetworkOutput("ini_network")),
                    column(6,
                           br(),
                           actionButton("TestButton2", "Testing!"),
                           visNetworkOutput("fin_network")
                           )
                    ),
                    a(id = "toggleAdvanced", "More"),
                    hidden(
                        div(id = "advanced",
                            box(width=6,
                            numericInput("age", "Weight Matrix", 30)),
                            box(width=6,
                            textInput("company", "Transition Matrix", "")
                        ))
                    )
                )
            ),
            #---
            # examplese
            #---
             tabItem(
                 tabName = "examples",
                box(
                    title = "Examples",
                    width = "16 col-md-8",
                   # examples_UI("example")
                ),
                 box(
                    width = "8 col-md-4",
                    components$examples$team
                )
             )
        )
    )
)

server <- function(input, output,session) {    
    # ---- graph - Xijin ----
    onclick("toggleAdvanced", toggle(id = "advanced", anim = TRUE))
    onclick("Moreinformation", toggle(id = "Moreinfor", anim = TRUE))
    
    
    output$ini_network <- renderVisNetwork({
      # -- different plots in common --
        num <- as.integer(input$Number_Hypotheses)
        names <- as.matrix(lapply(1:num, function(i) {
            paste0("H", i)
        }))
        # -- plot --
        if(input$Weighting_Strategy == "Bonferroni-Holm procedure"){
          nodes <- data.frame(id = names)
          net <- network(input$TransitionMatrixG,
                           directed = TRUE,
                           names.eval = "weights",
                           ignore.eval = FALSE)
          wide <- as.matrix(net)
          fromto <- melt(wide)
          fromto <- cbind(fromto,value=melt(input$TransitionMatrixG)[,"value"])
          colnames(fromto) <- c("from","to","trans","label")
          edges <- fromto[which(fromto$trans!=0),]  # weights!=0 ==> edges
          weights <- round(as.numeric(input$WeightPvalue[,2]),digits=2) 
          nodes$title  <- lapply(1:num, function(i) {
            paste0("H", i,":",weights[i])
          })
         netplot <-  visNetwork(nodes, edges, 
                     width="100%", height="800px") %>%
            visExport() %>%
            visEdges(arrows = 'to') %>% 
            visOptions(highlightNearest = TRUE,
                       manipulation = TRUE
                       #nodesIdSelection = list(enabled = TRUE, selected = "a")
            ) %>% 
         #   visInteraction(navigationButtons = TRUE)%>%
           visEvents(selectNode = "function(properties) {
      alert('p-value of '+ this.body.data.nodes.get(properties.nodes[0]).id) + 'is'+ this.body.data.weights[0].id;}")
        }
        if(input$Weighting_Strategy == "Fixed sequence test"){
          nodes <- data.frame(id = names)
          FS_TransitionMatrixG <- matrix(0, nrow = num, ncol = num)
          colnames(FS_TransitionMatrixG) <- names
          rownames(FS_TransitionMatrixG) <- names
          for (i in 1:(num-1)){
            FS_TransitionMatrixG[i,i+1] <- 1
          }
          net <- network(FS_TransitionMatrixG,
                           directed = TRUE,
                           names.eval = "weights",
                           ignore.eval = FALSE)
          wide <- as.matrix(net)
          fromto <- melt(wide)
          fromto <- cbind(fromto,value=melt(FS_TransitionMatrixG)[,"value"])
          colnames(fromto) <- c("from","to","trans","label")
          edges <- fromto[which(fromto$trans!=0),]  # weights!=0 ==> edges
          weights <- rep(0,num)
          weights[1] <- 1
          nodes$title  <- lapply(1:num, function(i) {
            paste0("H", i,":",weights[i])
          })
          netplot <-  visNetwork(nodes, edges,
                     width="100%", height="800px") %>%
            visExport() %>%
            visEdges(arrows = 'to') %>%
            visOptions(highlightNearest = TRUE,
                       manipulation = TRUE
                       #nodesIdSelection = list(enabled = TRUE, selected = "a")
            )
          #%>%
          #  visInteraction(navigationButtons = TRUE)
        }
        if(input$Weighting_Strategy == "Fallback procedure"){
          nodes <- data.frame(id = names)
          FP_TransitionMatrixG <- matrix(0, nrow = num, ncol = num)
          colnames(FP_TransitionMatrixG) <- names
          rownames(FP_TransitionMatrixG) <- names
          for (i in 1:(num-1)){
            FP_TransitionMatrixG[i,i+1] <- 1
          }
          net <- network(FP_TransitionMatrixG,
                         directed = TRUE,
                         names.eval = "weights",
                         ignore.eval = FALSE)
          wide <- as.matrix(net)
          fromto <- melt(wide)
          fromto <- cbind(fromto,value=melt(FP_TransitionMatrixG)[,"value"])
          colnames(fromto) <- c("from","to","trans","label")
          edges <- fromto[which(fromto$trans!=0),]  # weights!=0 ==> edges
          weights <- round(matrix(1/num,nrow=num,ncol=1),digits=2) 
          nodes$title  <- lapply(1:num, function(i) {
            paste0("H", i,":",weights[i])
          })
          netplot <-  visNetwork(nodes, edges,
                                 width="100%", height="800px") %>%
            visEdges(arrows = 'to',shadow = FALSE) %>%
            visOptions(highlightNearest = TRUE,
                       manipulation = TRUE
                       #nodesIdSelection = list(enabled = TRUE, selected = "a")
            ) %>%
    #        visInteraction(navigationButtons = TRUE) %>%
            visExport() 
        }
        netplot
    })
    
      
 #   finalPlot <-eventReactive(input$TestButton2,
                               #  {
                               #    num <- as.integer(input$Number_Hypotheses)
                               #    if(input$Weighting_Strategy == "Bonferroni-Holm procedure"){
                               #    net <- network(input$TransitionMatrixG,
                               #                   directed = TRUE,
                               #                   names.eval = "weights",
                               #                   ignore.eval = FALSE)
                               # #   net %v% "vertex.names"  <- rownames(input$TransitionMatrixG)
                               #    e <- network.edgecount(net)
                               #    WPmatrix <- input$WeightPvalue
                               #    # 
                               #    res <- gMCP_xc2(matrix=input$TransitionMatrixG,
                               #                    weights=as.numeric(input$WeightPvalue[,2]),
                               #                    pvalues=as.numeric(input$WeightPvalue[,3]),
                               #                    alpha = input$alpha,fweights = F)
                               #    res_pvalues <- res$pvalues
                               #    res_weights <- round(res$weights,digits = 2)
                               #    res_G <- round(res$G,digits = 2)
                               #    
                               #    res_net <- network(res_G,directed = TRUE,
                               #                       names.eval = "weights",ignore.eval = FALSE)
                               #  #  res_net %v% "vertex.names"  <- rownames(input$TransitionMatrixG)
                               #    e <- network.edgecount(res_net)
                               #  #  res_net %v% "Rejection" <- res$rejected
                               #    wide <- as.matrix(res_net)
                               #    fromto <- melt(wide)
                               #    fromto <- cbind(fromto,value=melt(input$TransitionMatrixG)[,"value"])
                               #    colnames(fromto) <- c("from","to","trans","label")
                               #    edges <- fromto[which(fromto$trans!=0),] 
                               #    names <- as.matrix(lapply(1:num, function(i) {
                               #      paste0("H", i)
                               #    }))
                               #    nodes <- data.frame(id = names)
                               #    nodes$title  <- lapply(1:num, function(i) {
                               #      paste0("H", i,":",weights[i])
                               #    })
                               #    visNetwork(nodes, edges,
                               #                           width="100%", height="800px") %>%
                               #      visExport() %>%
                               #      visEdges(arrows = 'to') %>%
                               #      visOptions(highlightNearest = TRUE,
                               #                 manipulation = TRUE
                               #                 #nodesIdSelection = list(enabled = TRUE, selected = "a")
                               #      )%>%
                               #     visInteraction(navigationButtons = TRUE)
                               #  })
    
                               #  }
                              # if(input$Weighting_Strategy == "Fixed sequence test"){
                              # }
                              # if(input$Weighting_Strategy == "Fallback procedure"){
                              # }
    
    output$fin_network <- renderVisNetwork(
      finalPlot()
    )
    
    output$uioutput_Tmatrix <- renderUI({
        num <- as.integer(input$Number_Hypotheses)
        df <- (1-diag(num))/(num-1)
        rownames(df) <- lapply(1:num, function(i) {
            paste0("H", i)
        })
        colnames(df) <- rownames(df)
        box(width = 12,
            box(title = "Transition matrix",
                status = "primary", 
                solidHeader = TRUE,
                width = 6, collapsible = TRUE,collapsed = TRUE,
                helpText(""),
                matrixInput(inputId = "TransitionMatrixG",
                            value = df,class = "numeric",
                            cols = list(
                                names = TRUE,extend = FALSE,
                                editableNames = TRUE,delta = 2),
                            rows = list(
                                names = TRUE, extend = FALSE,
                                editableNames = TRUE,delta = 1),
                            copy = TRUE,paste = TRUE)),
            box(title = "Weights and P-values",
                status = "primary",solidHeader = TRUE,
                width = 6,collapsible = TRUE,collapsed = TRUE,
                matrixInput(inputId = "WeightPvalue",
                            value = matrix(cbind(
                                        (lapply(1:num, function(i) {
                                          paste0("H", i)
                                        })),rep(1/num,num),rep(0.01,num)),
                                        nrow = num, ncol = 3,
                                        dimnames = list(NULL, c("Hypotheses", "Weights",'P-values'))),
                            cols = list(names = TRUE, extend = FALSE,
                                        editableNames = FALSE, delta = 2),
                            rows = list(
                                      names = FALSE, extend = FALSE,
                                      editableNames = TRUE, delta = 1),
                            copy = TRUE, paste = TRUE)
                    ),
            )
    })
    
    twoPlots <- eventReactive(input$TestButton,
                              {
                                net <- network(input$TransitionMatrixG,
                                               directed = TRUE,
                                               names.eval = "weights",
                                               ignore.eval = FALSE)
                                net %v% "vertex.names"  <- rownames(input$TransitionMatrixG)
                                e <- network.edgecount(net)
                                ###
                                a <-  ggplot(net, aes(x = x, y = y, xend = xend, yend = yend)) +
                                  geom_edges(arrow = arrow(length = unit(10, "pt"), type = "closed"),
                                             color = "grey50",
                                             curvature = 0.15) +
                                  geom_nodes(aes(x, y),color = "grey", size = 8) +
                                  geom_nodetext(aes(label = vertex.names)) +
                                  geom_edgetext_repel(aes(label = weights), color = "white", fill = "grey25",
                                                      box.padding = unit(0.25, "line")) +
                                  scale_color_brewer(palette = "Set2") +
                                  theme(margin = c(0.1,0.1,0.1,0.1))+
                                  #  scale_size_area("importance", breaks = 1:3, max_size = 9) +
                                  theme_blank()
                                
                                
                                ###
                                WPmatrix <- input$WeightPvalue
                                # 
                                res <- gMCP_xc2(matrix=input$TransitionMatrixG,
                                                weights=as.numeric(input$WeightPvalue[,2]),
                                                pvalues=as.numeric(input$WeightPvalue[,3]),
                                                alpha = input$alpha,fweights = F)
                                res_pvalues <- res$pvalues
                                res_weights <- round(res$weights,digits = 2)
                                res_G <- round(res$G,digits = 2)
                                
                                res_net <- network(res_G,directed = TRUE,
                                                   names.eval = "weights",ignore.eval = FALSE)
                                res_net %v% "vertex.names"  <- rownames(input$TransitionMatrixG)
                                e <- network.edgecount(res_net)
                                res_net %v% "Rejection" <- res$rejected
                                b <- ggplot(res_net, aes(x = x, y = y, xend = xend, yend = yend)) +
                                  geom_edges(arrow = arrow(length = unit(15, "pt"), type = "closed"),
                                             color = "grey50",
                                             curvature = 0.15) +
                                  geom_nodes(aes(x, y,color = Rejection), size = 12) +
                                  geom_nodetext(aes(label = vertex.names)) +
                                  scale_color_brewer(palette = "Set2") +
                                  theme_blank()
                                ggarrange(a,b,
                                          ncol = 2, nrow = 1)
                              })
    
    output$ResultPlot <- renderPlot(
      twoPlots()
    )
    
     output$extend1 <- renderTable(
        {
            net <- network(input$TransitionMatrixG,
                           directed = TRUE,
                           names.eval = "weights",
                           ignore.eval = FALSE)
            res <- gMCP_xc2(matrix = input$TransitionMatrixG,
                     weights = as.numeric(input$WeightPvalue[,2]),
                     pvalues = as.numeric(input$WeightPvalue[,3]),
                     alpha = input$alpha,fweights = F)
            data.frame(weights = res$weights)
        })
     output$extend2 <- renderTable(
         {
             net <- network(input$TransitionMatrixG,
                            directed = TRUE,
                            names.eval = "weights",
                            ignore.eval = FALSE)
             res <- gMCP_xc2(matrix = input$TransitionMatrixG,
                             weights = as.numeric(input$WeightPvalue[,2]),
                             pvalues = as.numeric(input$WeightPvalue[,3]),
                             alpha = input$alpha,fweights = F)
             result <- data.frame(res$G)
             colnames(result) <- rownames(input$TransitionMatrixG)
             result
         })
     output$extend3 <- renderTable(
         {
             net <- network(input$TransitionMatrixG,
                            directed = TRUE,
                            names.eval = "p-values",
                            ignore.eval = FALSE)
             res <- gMCP_xc2(matrix = input$TransitionMatrixG,
                             weights = as.numeric(input$WeightPvalue[,2]),
                             pvalues = as.numeric(input$WeightPvalue[,3]),
                             alpha = input$alpha,fweights = F)
             data.frame("Adjusted p-values" = res$adjpvalues)
         })
    
    # result <- eventReactive(input$TestButton,{
    #     resultG <- gMCP_xc2(matrix = input$TransitionMatrixG,
    #                         weights = rep(num,1/num),
    #                         pvalues= input$WeightPvalue[,3],
    #                         alpha = input$alpha,fweights = F)
    #     colnames(resultG$G) <- NULL
    #     resultG$G
    # })
    # output$resultMatrixG <- renderTable({
    #     result()
    # }) 

    
}

shinyApp(ui, server)


# Manipulate An Existing DataTables Instance
# https://rstudio.github.io/DT/shiny.html
