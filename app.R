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
source("R/columns.R")
source("R/components.R")
source("R/func/gMCP_xc2.R")
source("R/func/generate_graph.R")
source("R/func/generate_data.R")
source("R/func/function_matrix.R")
source("R/func/graph_create.R")

# ui output
source("R/module/tabHome.R")
source("R/module/tabDraw.R")
source("R/module/tabProcedure.R")
source("R/module/tabTest.R")


# -----------------------------------------------------
ui <- tagList(
  fluidPage(setBackgroundColor("AliceBlue"),
      theme = shinytheme("cerulean"),
      list(tags$head(tags$style(HTML("
      .navbar .navbar-nav {float: right; 
                           color: white; 
                           font-size: 10px;} 
      .navbar .navbar-header {float: left;} 

  ")))),
      tags$head(tags$style(HTML("
      .shiny-output-error-myClass {
        color: red;
      }
    "))),
      navbarPage(id = "tabs",
                 title=tags$em("GraphApp"),
                 collapsible = TRUE,
                 tabHome,
                 tabDraw,
                 tabProcedure,
                 tabTest
    )),
  br(),br(),br(),
  components$foot)

# graph_data initial setting -----------------------
init.nodes.df = data.frame(id = c("H1","H2","H3"),
                           label = c("H1","H2","H3"),  # label should be the same as 'id'
                           weight = c("1/3", "1/3","1/3"),
                           pvalue = c("0.01","0.01","0.01"),
                           stringsAsFactors = F)
init.edges.df = data.frame(id = c("e1","e2"),
                           from = c("H1","H2"), 
                           to = c("H2","H3"),
                           label = c("0.1","0.1"), # label should be the same as 'propagation'
                           stringsAsFactors = F)

server <- function(input, output,session) { 
  output$downloadData <- downloadHandler(
    filename = "file1.pdf",
    content = function(file) {
      file.copy("www/file1.pdf", file)
    }
  )
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
    # ---------------- Draw Page output ----------------  
  graph_data = reactiveValues(
    nodes = init.nodes.df,
    edges = init.edges.df
  )
  
  output$editable_network <- renderVisNetwork({
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visExport() %>%
      visEdges(arrows = 'to') %>%
      visNodes(shape = "ellipse") %>%
      visOptions(manipulation = list(enabled = T,
                                     editEdgeCols = c("label"),
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
          # label = "NULL",
          label = "NULL",
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
      temp$label[temp$id == input$editable_network_graphChange$id] = input$editable_network_graphChange$label
      
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
  
  # Update graph when a cell is edited in the visEdges datatable
  proxy_visEdges <- dataTableProxy("graphOutput_visEdges")
  observeEvent(input$graphOutput_visEdges_cell_edit, {
    info <- input$graphOutput_visEdges_cell_edit
    i <- info$row
    # Add one to the column to shift for id
    j <- info$col + 1
    v <- info$value
    
    # Update visNetwork data via proxy
    graph_data$edges[i, j] <<- DT::coerceValue(
      v, graph_data$edges[i, j]
    )
    replaceData(proxy_visEdges, graph_data$edges, resetPaging = FALSE)
    
    # Make sure main graphdata stays up-to-date
    # graph_data <- graph_data
  })
  
  proxy_visNodes <- dataTableProxy("graphOutput_visNodes")
  observeEvent(input$graphOutput_visNodes_cell_edit, {
    info <- input$graphOutput_visNodes_cell_edit
    i <- info$row
    # Add one to column to shift for id
    j <- info$col + 1
    v <- info$value
    
    # Update visNetwork data via proxy
    graph_data$nodes[i, j] <<- DT::coerceValue(
      v, graph_data$nodes[i, j]
    )
    replaceData(proxy_visNodes, graph_data$nodes, resetPaging = FALSE)
    
  })
  
  output$graphOutput_visNodes = DT::renderDT({
    nodes_result = graph_data$nodes[,c("id","weight","pvalue")]
    colnames(nodes_result) = c("hypothesis (id)","weight","p-value")
    nodes_result
  },
  editable = TRUE,
  options = list("pageLength" = 4, dom = "tp", searching = F, scrollX = F))
  
  output$graphOutput_visEdges = DT::renderDT({
    result <- graph_data$edges[,c("from","to","label")]
    colnames(result) <- c("from","to","propagation")
    result
  },
  editable = TRUE,
  options = list("pageLength" = 4, dom = "tp", searching = F, scrollX = F))
  
  output$res_Table1 <- renderTable({
    num <- nrow(graph_data$nodes)
    names <- lapply(1:num, function(i) {paste0("H", i)})
    ini.matrix <- matrix(0,nrow=num,ncol=num)
    colnames(ini.matrix) <- names
    rownames(ini.matrix) <- names
    for (i in 1:nrow(graph_data$edges)){
      ini.matrix[which(graph_data$edges$from[i]==rownames(ini.matrix)), 
                 which(graph_data$edges$to[i]==rownames(ini.matrix))] <- as.numeric(graph_data$edges[i,"label"])
    }
    result <- gMCP_xc2(matrix=ini.matrix,
                       weights=f2n(graph_data$nodes[,"weight"]),
                       pvalues=as.numeric(graph_data$nodes[,"pvalue"]),
                       alpha = input$alpha_draw,fweights = F)
    result <- data.frame(result$rejected)
    result <- ifelse(result=="TRUE", "rejected", "not rejected")
    result <- cbind(as.character(names),result)
    colnames(result) <- c("hypothesis","result")
    result
  })
  
  output$res_Table2 <- renderTable({
    num <- nrow(graph_data$nodes)
    names <- lapply(1:num, function(i) {paste0("H", i)})
    ini.matrix <- matrix(0,nrow=num,ncol=num)
    colnames(ini.matrix) <- names
    rownames(ini.matrix) <- names
    for (i in 1:nrow(graph_data$edges)){
      ini.matrix[which(graph_data$edges$from[i]==rownames(ini.matrix)), 
                 which(graph_data$edges$to[i]==rownames(ini.matrix))] <- as.numeric(graph_data$edges[i,"label"])
    }
    result <- gMCP_xc2(matrix=ini.matrix,
                       weights=f2n(graph_data$nodes[,"weight"]),
                       pvalues=as.numeric(graph_data$nodes[,"pvalue"]),
                       alpha = input$alpha_draw,fweights = F)
    result <- data.frame(result$adjpvalues)
    result <- cbind(as.character(names),result)
    colnames(result) <- c("hypothesis","adjusted p-values")
    result
  })
  
  output$extend_G <- renderTable(
    {
      result <- data.frame(res$G)
      colnames(result) <- rownames(input$TransitionMatrixG)
      rownames(result) <- rownames(input$TransitionMatrixG)
      result
    }, caption = "0 means no trasition.", caption.placement = "bottom")
  
    # ---------------- Procedure Page output ----------------
    df_create <- reactive({
      switch(input$Weighting_Strategy2,
             "Bonferroni-Holm procedure" = dfcreate(input$Number_Hypotheses,"Bonferroni-Holm procedure"),
             "Fixed sequence test" = dfcreate(input$Number_Hypotheses,"Fixed sequence test"),
             "Fallback procedure" = dfcreate(input$Number_Hypotheses,"Fallback procedure")
             # "Simple successive procedure" = dfcreate(input$Number_Hypotheses,"Simple successive procedure")
      )
    })
    
    wp_create <- reactive({
      switch(input$Weighting_Strategy2,
             "Bonferroni-Holm procedure" = wpcreat(input$Number_Hypotheses,"Bonferroni-Holm procedure"),
             "Fixed sequence test" = wpcreat(input$Number_Hypotheses,"Fixed sequence test"),
             "Fallback procedure" = wpcreat(input$Number_Hypotheses,"Fallback procedure")
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
                                                                  margin = margin(15, 0, 15, 0)),
                                        plot.margin = margin(0.5,0.1,0.1,0.1))    # t r b l
                                  
                                res <- gMCP_xc2(matrix=input$TransitionMatrixG,
                                                weights=f2n(input$WeightPvalue[,"Weights"]),
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
                                             color = "grey50",curvature = 0.15) +
                                  geom_nodes(aes(x, y,color = Rejection), alpha = 0.5,size = 14) +
                                  geom_nodetext(aes(label = vertex.names)) +
                                  scale_color_brewer(palette = "Set2") +
                                  labs(title='Final graph')+
                                  theme_blank()+
                                  theme(aspect.ratio=1,
                                        plot.title = element_text(size=15, face="bold.italic",
                                                                  margin = margin(10, 5, 10, 0)),
                                        plot.margin = margin(0.5,0.5,0.1,0.1))+
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
                            weights=f2n(input$WeightPvalue[,1]),
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
                             weights=f2n(input$WeightPvalue[,"Weights"]),
                             pvalues=as.numeric(input$WeightPvalue[,"P-values"]),
                             alpha = input$alpha,fweights = F)
             result <- data.frame(res$G)
             colnames(result) <- rownames(input$TransitionMatrixG)
             rownames(result) <- rownames(input$TransitionMatrixG)
             result
         }, caption = "0 means no trasition.", caption.placement = "bottom")

     # ---------------- Test Page output ----------------
     df_create_test <- reactive({
       switch(input$exRadio,
              "Simple successive procedure" = dfcreate(4,"Simple successive procedure"),
              "Second" = dfcreate(4,"Simple successive procedure")
       )
     })
     
     wp_create_test <- reactive({
       switch(input$exRadio,
              "Simple successive procedure" = wpcreat(4,"Simple successive procedure"),
              "Second" = wpcreat(4,"Simple successive procedure")
       )
     })
     
     output$uioutput_Tmatrix_df <- renderUI({
       num <- 4
       df <- df_create_test()
       rownames(df) <- lapply(1:num, function(i) {
         paste0("H", i)
       })
       colnames(df) <- rownames(df)
       wp <- wp_create_test()
           box(title = div(HTML("Transition matrix <em>G</em>")),
               status = "primary", solidHeader = TRUE,width = 10,
               withMathJax(helpText("The propagation of significance levels")),
               matrixInput(inputId = "TransitionMatrixG",
                           value = df,class = "numeric",
                           cols = list(names = TRUE,extend = FALSE,
                                       editableNames = FALSE,delta = 2),
                           rows = list(names = TRUE, extend = FALSE,
                                       editableNames = FALSE,delta = 1),
                           copy = TRUE,paste = TRUE),
               helpText("The values are between 0 and 1.")
           )
           })
       
       output$uioutput_Tmatrix_wp <- renderUI({
         num <- 4
         df <- df_create_test()
         rownames(df) <- lapply(1:num, function(i) {
           paste0("H", i)
         })
         colnames(df) <- rownames(df)
         wp <- wp_create_test()
           box(title = div(HTML("Weights <em>w</em> and <em>p</em>-values")),
               status = "primary",solidHeader = TRUE,width = 10,
               helpText(div(HTML("Initial weights and <em>p</em>-values"))),
               matrixInput(inputId = "WeightPvalue",
                           value = wp, class = "numeric",
                           cols = list(names = TRUE, extend = FALSE,
                                       editableNames = FALSE, delta = 2),
                           rows = list(names = TRUE, extend = FALSE,
                                       editableNames = FALSE, delta = 1),
                           copy = TRUE, paste = TRUE),
               helpText("The sum of weights are no more than 1."))
     })
     
     output$resPlots_ini <- renderPlot({
       num <- 4
       df <- df_create_test()
       wp <- wp_create_test()
       names <- lapply(1:num, function(i) {paste0("H", i)})
       net <- network(df,
                      directed = TRUE,
                      names.eval = "weights",
                      ignore.eval = FALSE)
       num <- 4
       net %v% "vertex.names"  <- names
       e <- network.edgecount(net)
       
       ggplot(net, aes(x = x, y = y, xend = xend, yend = yend)) +
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
               plot.margin = margin(0.1,0.1,0.1,0.1))    # t r b l
     })


output$resPlots_final <- renderPlot({
  num <- 4
  df <- df_create_test()
  wp <- wp_create_test()
  names <- lapply(1:num, function(i) {paste0("H", i)})
  res <- gMCP_xc2(matrix=df,
                  weights=f2n(wp[,"Weights"]),
                  pvalues=as.numeric(wp[,"P-values"]),
                  alpha = input$alpha_test,fweights = F)
  res_pvalues <- res$pvalues
  res_weights <- round(res$weights,digits = 2)
  res_G <- round(res$G,digits = 2)
  res_adj <- data.frame("Hypothesis" = paste0("H", 1:num),
                        "Adjusted p-values" = res$adjpvalues,
                        check.names = FALSE)
  res_net <- network(res_G,directed = TRUE,
                     names.eval = "weights",ignore.eval = FALSE)
  res_net %v% "vertex.names"  <- names
  e <- network.edgecount(res_net)
  rej <- ifelse(res$rejected==TRUE,"rejected","not rejected")
  res_net %v% "Rejection" <- rej
  
  ggplot(res_net, aes(x = x, y = y, xend = xend, yend = yend)) +
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
})
}

shinyApp(ui, server)
