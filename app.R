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
library(shinythemes)
library(ggnetwork)
library(ggpubr)
library(shinyalert)
library(DT)
library(DiagrammeR)
library(dagitty)
library(texPreview)
library(shinyAce)
library(shinyBS)
library(ggdag)
library(reshape2)
library(data.table)
library(ggplot2)
library(gridExtra)
# the most recent version of visNetwork
# devtools::install_github("datastorm-open/visNetwork")
library(visNetwork)
# library(networkD3)
source("R/module/clickpad.R")
source("R/columns.R")
source("R/node.R")
source("R/components.R")
source("R/functions.R")
source("R/func/gMCP_xc2.R")
# source("R/func/modify_visNetwork.R")
source("R/func/generate_graph.R")
source("R/func/generate_data.R")
source("R/func/function_matrix.R")
source("R/func/graph_create.R")
# ui output
source("R/module/tabHome.R")
source("R/module/tabExample.R")
source("R/module/tabDraw.R")
# new added
library(gfpop)
library(htmlwidgets)
source("R/module/tabDrawer.R")
source("R/func/fct_visNetwork_helpers.R")
source("R/func/golem_utils_server.R")
source("R/func/golem_utils_ui.R")
source("R/func/fct_graph_helpers.R")
source("R/func/utils_general.R")


# -----------------------------------------------------
ui <- tagList(
  fluidPage(setBackgroundColor("AliceBlue"),
      theme = shinytheme("cerulean"),
      list(tags$head(
                     tags$style(HTML("
      .navbar .navbar-nav {float: right; 
                           color: white; 
                           font-size: 10px; 
                            } 
      .navbar .navbar-header {float: left; } 

  ")))),
      tags$head(
        tags$style(HTML("
      .shiny-output-error-myClass {
        color: red;
      }
    "))
      ),
      navbarPage(id = "tabs",
                 title=tags$em("GraphApp"),
                 collapsible = TRUE,
                 tabHome,
                 tabDraw,
                 tabExample,
                 tabDrawer
    )),
  br(),br(),br(),
  components$foot
)
# -----------------------------------------------------

init.nodes.df = data.frame(id = c("H1","H2","H3"),
                           label = c("H1","H2","H3"),  # label should be the same as 'id'
                           weight = c("0.3", "0.2","0.2"),
                           pvalue = c("0.01","0.01","0.01"),
                           stringsAsFactors = F)
init.edges.df = data.frame(id = c("e1","e2"),
                           from = c("H1","H2"), 
                           to = c("H2","H3"),
                           propagation = c("0.1","0.1"),
                           label = c("0.1","0.1"), # label should be the same as 'propagation'
                           stringsAsFactors = F)

server <- function(input, output,session) { 
  shinyjs::onclick("Moreinformation",
                   shinyjs::toggle(id = "moreinfor", anim = TRUE))
  values <- reactiveValues()
  values$num <- 3
  observeEvent(input$spec, {
    shinyalert("Number of hypotheses", 
               type = "input",
               inputType = "number",
               inputValue = "3",
               inputId = "num_alert",
               inputPlaceholder = "",
               confirmButtonText = "Yes", 
               # showCancelButton = TRUE,cancelButtonText = "No", 
               callbackR = modalCallback)
  })
  modalCallback <- function(value) {
    value$num <- input$num_alert
  }
   # graph_data initial setting -----------------------
    # ---------------- Draw Page output ----------------  
  graph_data = reactiveValues(
    nodes = init.nodes.df,
    edges = init.edges.df
  )
  
  output$editable_network <- renderVisNetwork({
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visExport() %>%
      visEdges(arrows = 'to') %>%
      visOptions(manipulation = list(enabled = T,
                                     editEdgeCols = c("propagation"),
                                     editNodeCols = c("id","weight", "pvalue"),
                                     addNodeCols = c("id","weight", "pvalue")
      ))
  })
  
  observeEvent(input$editable_network_graphChange, {
    if(input$editable_network_graphChange$cmd == "addNode") {
      temp = bind_rows(
        graph_data$nodes,
        data.frame(id = input$editable_network_graphChange$id,
                   label = input$editable_network_graphChange$id,
                   weight = input$editable_network_graphChange$weight,
                   pvalue = input$editable_network_graphChange$pvalue,
                   stringsAsFactors = F)
      )
      graph_data$nodes = temp
    }
    else if(input$editable_network_graphChange$cmd == "addEdge") {
      temp = bind_rows(
        graph_data$edges,
        data.frame(
          id = input$editable_network_graphChange$id,
          from = input$editable_network_graphChange$from,
          to = input$editable_network_graphChange$to,
          propagation = "0.2",
          label = "0.2",
          stringsAsFactors = F)
      )
      graph_data$edges = temp
    }
    else if(input$editable_network_graphChange$cmd == "editNode") {
      temp = graph_data$nodes
      temp$id[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$id
      temp$label[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$id
      temp$weight[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$weight
      temp$pvalue[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$pvalue
      graph_data$nodes = temp
    }
    else if(input$editable_network_graphChange$cmd == "editEdge") {
      temp = graph_data$edges
      temp$propagation[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$propagation
      temp$label[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$propagation
      
      graph_data$edges = temp
    }
    else if(input$editable_network_graphChange$cmd == "deleteElements") {
      for(node.id in input$editable_network_graphChange$nodes) {
        temp = graph_data$nodes
        temp = temp[temp$id != node.id,]
        graph_data$nodes = temp
      }
      for(edge.id in input$editable_network_graphChange$edges) {
        temp = graph_data$edges
        temp = temp[temp$id != edge.id,]
        graph_data$edges = temp
      }
    }
  })
  
  output$all_nodes = DT::renderDT({
    nodes_result = graph_data$nodes[,c("id","weight","pvalue")]
    colnames(nodes_result) = c("Test (id)","Weight","P-value")
    nodes_result
  },
  editable = TRUE,
  options = list("pageLength" = 4, dom = "tp", searching = F, scrollX = F))
  
  output$all_edges = DT::renderDT({
    graph_data$edges[,c("from","to","propagation")]
  },
  editable = TRUE,
  options = list("pageLength" = 4, dom = "tp", searching = F, scrollX = F))
  
  
    # ---------------- Example Page output ----------------
    df_create <- reactive({
      switch(input$Weighting_Strategy2,
             "Bonferroni-Holm procedure" = dfcreate(input$Number_Hypotheses,"Bonferroni-Holm procedure"),
             "Fixed sequence test" = dfcreate(input$Number_Hypotheses,"Fixed sequence test"),
             "Fallback procedure" = dfcreate(input$Number_Hypotheses,"Fallback procedure"),
             "Simple successive procedure" = dfcreate(input$Number_Hypotheses,"Simple successive procedure")
      )
    })
    
    wp_create <- reactive({
      switch(input$Weighting_Strategy2,
             "Bonferroni-Holm procedure" = wpcreat(input$Number_Hypotheses,"Bonferroni-Holm procedure"),
             "Fixed sequence test" = wpcreat(input$Number_Hypotheses,"Fixed sequence test"),
             "Fallback procedure" = wpcreat(input$Number_Hypotheses,"Fallback procedure"),
             "Simple successive procedure" = wpcreat(input$Number_Hypotheses,"Simple successive procedure")
      )
    })
    
    output$uioutput_Tmatrix <- renderUI({
      num <- as.integer(input$Number_Hypotheses)
      df <- df_create()
      rownames(df) <- lapply(1:num, function(i) {
        paste0("H", i)
      })
      colnames(df) <- rownames(df)
      wp <- wp_create()
      box(width = 10, style = "background-color: white;",
          box(title = div(HTML("Transition matrix <em>G</em>")),
              status = "primary", solidHeader = TRUE,width = 6, 
              withMathJax(helpText("The propagation of significance levels")),
              matrixInput(inputId = "TransitionMatrixG",
                          value = df,class = "numeric",
                          cols = list(names = TRUE,extend = FALSE,
                                      editableNames = FALSE,delta = 2),
                          rows = list(names = TRUE, extend = FALSE,
                                      editableNames = FALSE,delta = 1),
                          copy = TRUE,paste = TRUE),
              helpText("The values are between 0 and 1.")
          ),
          
          box(title = div(HTML("Weights <em>w</em> and <em>p</em>-values")),
              status = "primary",solidHeader = TRUE,width = 6,
              helpText(div(HTML("Initial weights and <em>p</em>-values"))),
              matrixInput(inputId = "WeightPvalue",
                          value = wp, class = "numeric",
                          cols = list(names = TRUE, extend = FALSE,
                                      editableNames = FALSE, delta = 2),
                          rows = list(names = TRUE, extend = FALSE,
                                      editableNames = FALSE, delta = 1),
                          copy = TRUE, paste = TRUE),
              helpText("The sum of weights are no more than 1.")),
          br(),
          helpText("Please click corresponding cell to edit before testing.")
      )
    })    
    
    
    twoPlots <- eventReactive(input$TestButton,
                              {
                                net <- network(input$TransitionMatrixG,
                                               directed = TRUE,
                                               names.eval = "weights",
                                               ignore.eval = FALSE)
                                num <- as.integer(input$Number_Hypotheses2)
                                net %v% "vertex.names"  <- rownames(input$TransitionMatrixG)
                                e <- network.edgecount(net)
                                
                                a <-  ggplot(net, aes(x = x, y = y, xend = xend, yend = yend)) +
                                  xlim(-0.02, 1.02) + ylim(-0.02, 1.02)+
                                  geom_edges(arrow = arrow(length = unit(20, "pt"), type = "closed"),
                                             color = "grey50",curvature = 0.15) +
                                  geom_nodes(aes(x, y),color = "grey",alpha = 0.5, size = 14) +
                                  geom_nodetext(aes(label = vertex.names)) +
                                  geom_edgetext_repel(aes(label = weights), color = "white", 
                                                      fill = "grey25",
                                                      box.padding = unit(0.25, "line")) +
                                  scale_color_brewer(palette = "Set2") +
                                  labs(title='Initial graph')+
                                  theme_blank()+
                                  theme(aspect.ratio=1,
                                        plot.title = element_text(size=15, face="bold.italic",
                                                                  margin = margin(10, 0, 10, 0)),
                                        plot.margin = margin(0.5,0.1,0.1,0.1))    # t r b l
                                  
                                res <- gMCP_xc2(matrix=input$TransitionMatrixG,
                                                weights=as.numeric(input$WeightPvalue[,"Weights"]),
                                                pvalues=as.numeric(input$WeightPvalue[,"P-values"]),
                                                alpha = input$alpha,fweights = F)
                                res_pvalues <- res$pvalues
                                res_weights <- round(res$weights,digits = 2)
                                res_G <- round(res$G,digits = 2)
                                res_adj <- data.frame("Hypothesis" = paste0("H", 1:input$Number_Hypotheses),
                                                      "Adjusted p-values" = res$adjpvalues,
                                                      check.names = FALSE)
                              
                                res_net <- network(res_G,directed = TRUE,
                                                   names.eval = "weights",ignore.eval = FALSE)
                                res_net %v% "vertex.names"  <- rownames(input$TransitionMatrixG)
                                e <- network.edgecount(res_net)
                                res_net %v% "Rejection" <- res$rejected
                                
                                b <- ggplot(res_net, aes(x = x, y = y, xend = xend, yend = yend)) +
                                  xlim(-0.02, 1.02) + ylim(-0.02, 1.02)+
                                  geom_edges(arrow = arrow(length = unit(20, "pt"), type = "closed"),
                                             color = "grey50",
                                             curvature = 0.15) +
                                  geom_nodes(aes(x, y,color = Rejection), alpha = 0.5,size = 14) +
                                  geom_nodetext(aes(label = vertex.names)) +
                                  scale_color_brewer(palette = "Set2") +
                                  labs(title='Final graph')+
                                  theme_blank()+
                                  theme(aspect.ratio=1,
                                        plot.title = element_text(size=15, face="bold.italic",
                                                                  margin = margin(10, 0, 10, 0)),
                                        plot.margin = margin(0.5,0.1,0.1,0.1))+
                                  annotation_custom(tableGrob(res_adj, rows=NULL,theme = grobtheme), 
                                                    # ttheme_minimal() could be transparent
                                                    xmin=1.06, xmax=1.15, ymin=1.01, ymax=1.06)
                                
                                ggarrange(a,b,ncol = 2, nrow = 1)
                              })
    
    output$ResultPlot <- renderPlot(
      twoPlots()
    )
    
     output$extend_weights <- renderTable(
        {
            net <- network(input$TransitionMatrixG,
                           directed = TRUE,
                           names.eval = "weights",
                           ignore.eval = FALSE)
            res <- gMCP_xc2(matrix = input$TransitionMatrixG,
                            weights=as.numeric(input$WeightPvalue[,1]),
                            pvalues=as.numeric(input$WeightPvalue[,2]),
                     alpha = input$alpha,fweights = F)
            data.frame(Hypothesis = paste0("H", 1:input$Number_Hypotheses),
                       Weights = res$weights)
        })
     
     output$extend_G <- renderTable(
         {
             net <- network(input$TransitionMatrixG,
                            directed = TRUE,
                            names.eval = "weights",
                            ignore.eval = FALSE)
             res <- gMCP_xc2(matrix = input$TransitionMatrixG,
                             weights=as.numeric(input$WeightPvalue[,1]),
                             pvalues=as.numeric(input$WeightPvalue[,2]),
                             alpha = input$alpha,fweights = F)
             result <- data.frame(res$G)
             colnames(result) <- rownames(input$TransitionMatrixG)
             rownames(result) <- rownames(input$TransitionMatrixG)
             result
         }, caption = "0 means no trasition.", caption.placement = "bottom")
     
     
     output$downloadData <- downloadHandler(
       filename = "file1.pdf",
       content = function(file) {
         file.copy("www/file1.pdf", file)
       }
     )
     
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

     # ---------------- Drawer Page output ----------------
     
     gfpop_data = reactiveValues(
       graphdata = gfpop::graph(penalty = as.double(15),
                                type = "std"),
       graphdata_visNetwork = graphdf_to_visNetwork(gfpop::graph(
         penalty = as.double(15),type = "std"),
         edge_ids = c("std_std_null", "std_std_std"))
     )
     
     node_id_to_label <- reactiveValues(
       main = list()
     )
     
     dummy_graph_refresh <- reactiveValues(i = 0)
     
     output$gfpopGraph <- renderVisNetwork({
       input$refreshGraph
       dummy_graph_refresh$i
       
       generate_visNetwork(isolate(gfpop_data$graphdata_visNetwork))
     })
     
     observe({
       if (isTruthy(gfpop_data$graphdata_visNetwork$edges)) {
         gfpop_data$graphdata_visNetwork$edges$label <- create_label(
           gfpop_data$graphdata_visNetwork$edges,
           colum = input$labels
         )
       }
       # Update graph edges and nodes
       visNetworkProxy(("gfpopGraph")) %>%
         visUpdateNodes(nodes = gfpop_data$graphdata_visNetwork$nodes) %>%
         visUpdateEdges(edges = gfpop_data$graphdata_visNetwork$edges)
       # Update the mapping between node ids and labels
       node_ids <- gfpop_data$graphdata_visNetwork$nodes$id
       node_labels <- gfpop_data$graphdata_visNetwork$nodes$label
       names(node_labels) <- node_ids
       node_id_to_label$main <- node_labels
     })
     
     
     # Adjust whether null nodes are visible, after user clicks radio box.
     observeEvent(eventExpr = input$showNull, {
       gfpop_data$graphdata_visNetwork$edges$hidden <- sapply(
         gfpop_data$graphdata_visNetwork$edges$type,
         function(x) if (input$showNull) FALSE else (x == "null"))
     })
     
     # Update Graph upon User Edit --------------------------------------------
     
     # Respond to a change in the visNetwork plot (via manipulation)
     observeEvent(input$gfpopGraph_graphChange, {
       event <- input$gfpopGraph_graphChange
       gfpop_data$graphdata_visNetwork <- modify_visNetwork(
         event,
         gfpop_data$graphdata_visNetwork
       )
       # Eure that graphdata stays in sync with visNetwork data
       gfpop_data$graphdata <- visNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
     })
     
     # Update graph when a cell is edited in the direct gfpop input datatable
     # ------------------------------------------------------------------------------------------
     # Update graph when a cell is edited in the visEdges datatable
     proxy_visEdges <- dataTableProxy("graphOutput_visEdges")
     observeEvent(input$graphOutput_visEdges_cell_edit, {
       info <- input$graphOutput_visEdges_cell_edit
       i <- info$row
       # Add one to the column to shift for id
       j <- info$col + 1
       v <- info$value
       
       # Update visNetwork data via proxy
       gfpop_data$graphdata_visNetwork$edges[i, j] <<- DT::coerceValue(
         v, gfpop_data$graphdata_visNetwork$edges[i, j, with = F]
       )
       replaceData(proxy_visEdges, gfpop_data$graphdata_visNetwork$edges, resetPaging = FALSE)
       
       # Make sure main graphdata stays up-to-date
       # gfpop_data$graphdata <- gfpop_data$graphdatavisNetwork_to_graphdf(gfpop_data$graphdata_visNetwork)
     })
     
     # Update graph when a cell is edited in the visNodes datatable
     proxy_visNodes <- dataTableProxy("graphOutput_visNodes")
     observeEvent(input$graphOutput_visNodes_cell_edit, {
       info <- input$graphOutput_visNodes_cell_edit
       i <- info$row
       # Add one to column to shift for id
       j <- info$col + 1
       v <- info$value
       
       gfpop_data$graphdata_visNetwork$nodes[i, j] <<- DT::coerceValue(
         v, gfpop_data$graphdata_visNetwork$nodes[i, j, with = F]
       )
       replaceData(proxy_visNodes, gfpop_data$graphdata_visNetwork$nodes, resetPaging = FALSE)
       
     })
     
     # Render Graph DataTables ------------------------------------------------
     output$graphOutput <- DT::renderDT(
       {
         gfpop_data$graphdata %>% select_graph_colum()
       },
       editable = TRUE,
       optio = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
     )
     
     output$graphOutput_visEdges <- DT::renderDT(
       {
         gfpop_data$graphdata_visNetwork$edges[,c(
           "label", 
           "to", "from","parameter",
           "type",  "penalty",
           "K", "a", "min", "max", "selfReference.angle",
           "selfReference.size", "hidden", "color"
         )]
       },
       editable = TRUE,
       optio = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
     )
     
     output$graphOutput_visNodes <- DT::renderDT(
       {
         gfpop_data$graphdata_visNetwork$nodes[,c(
           "label", 
           "size", "start"
           # "end",
           # "shape", "color.background", "color.border", "shadow"
         )]
       },
       editable = TRUE,
       optio = list("pageLength" = 5, dom = "tp", searching = F, scrollX = T)
     )
}

shinyApp(ui, server)


# Manipulate An Existing DataTables Instance
# https://rstudio.github.io/DT/shiny.html
