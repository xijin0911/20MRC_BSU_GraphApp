source("R/dependencies.R")
source("R/footer.R")
source("R/function/graph_app.R")
source("R/function/generate_graph.R")
source("R/function/generate_data.R")
source("R/function/function_matrix.R")
source("R/function/graph_create.R")

# ui output
source("R/module/tabhome.R")
source("R/module/tabdraw.R")
source("R/module/tabprocedure.R")
source("R/module/tabtest.R")

# -----------------------------------------------------
ui <- tagList(
  # error style setting
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))
  ),
  fluidPage(setBackgroundColor("AliceBlue"),
      theme = shinytheme("cerulean"),
      list(tags$head(tags$style(HTML("
      .navbar .navbar-nav {float: left; color: white; font-size: 10px;} 
      .navbar .navbar-header {float: left;}")))),
      tags$head(tags$style(HTML("
      .shiny-output-error-myClass { color: red;} "))),
      navbarPage(id = "tabs",title=tags$em("GraphApp"),collapsible = TRUE,
                 tabHome,tabDraw,
                 navbarMenu("Examples",icon=icon("cog", lib = "glyphicon"),  
                            tabProcedure,tabTest)
    )),
  br(),br(),br(),
  foot)

# graph_data initial setting -----------------------
init.nodes.df = data.frame(id = c("H1","H2"),
                           label = c("H1","H2"),  # label should be the same as 'id'
                           weight = c("1/2", "1/2"),
                           pvalue = c("0.01","0.01"),
                           stringsAsFactors = F)
init.edges.df = data.frame(id = c("e1","e2"),
                           from = c("H1","H2"), 
                           to = c("H2","H1"),
                           label = c("1","1"), # label should be the same as 'propagation'
                           stringsAsFactors = F)

server <- function(input, output,session){ 
  # loading setting
  Sys.sleep(0)
  # instruction
  steps_draw <- read.csv("help_draw.csv")
  
  session$sendCustomMessage(type = 'setHelpContent', 
                            message = list(steps = toJSON(steps_draw) ))
  observeEvent(input$draw_instruction,{
    session$sendCustomMessage(type = 'startHelp', message = list(""))
  })
  
  # output$downloadSlide <- downloadHandler(
  #   filename = "CourseSlide.pdf",
  #   content = function(file) {
  #     file.copy("www/CourseSlide.pdf", file)
  #   }
  # )
  output$downloadRMD <- downloadHandler(
    filename = "Function_graph_app.Rmd",
    content = function(file) {
      file.copy("www/Function_graph_app.Rmd", file)
    }
  )
  # shinyjs::onclick("Moreinformation",
  #                  shinyjs::toggle(id = "moreinfor", anim = TRUE))
  # values <- reactiveValues()
  # values$num <- 3
  # observeEvent(input$spec, {
  #   shinyalert("Number of hypotheses", 
  #              type = "input",
  #              inputType = "number",
  #              inputValue = "3",
  #              inputId = "num_alert",
  #              inputPlaceholder = "",
  #              confirmButtonText = "Yes", 
  #              callbackR = modalCallback)
  # })
  # modalCallback <- function(value) {
  #   value$num <- input$num_alert
  # }
    # ---------------- Draw Page output ----------------  
  observeEvent(input$inst, {
    shinyalert(title = "Instructions",type = "info", 
               text= "<li>Graph & Details: Inputs</li>
               <li>Results: Outputs of the rejection results.</li>",html=TRUE)
  })
  
  graph_data = reactiveValues(
    nodes = init.nodes.df,
    edges = init.edges.df
  )
  
  output$editable_network <- renderVisNetwork({
    visNetwork(graph_data$nodes, graph_data$edges) %>%
      visExport() %>%
      visEdges(arrows = 'to') %>%
      visNodes(shape = "ellipse") %>%
      visLayout(randomSeed = 123)%>%
      visPhysics(stabilization = TRUE)%>%
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
                   stringsAsFactors = F))
      graph_data$nodes = temp
    }
    else if(input$editable_network_graphChange$cmd == "addEdge") {
      temp = bind_rows(
        graph_data$edges,
        data.frame(
          id = input$editable_network_graphChange$id,
          from = input$editable_network_graphChange$from,
          to = input$editable_network_graphChange$to,
          label = "undefined",
          stringsAsFactors = F))
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
        graph_data$edges = temp}
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
    colnames(nodes_result) = c("hypotheses (ids)","weights","p-values")
    nodes_result
  },
  editable = TRUE,
  options = list("pageLength" = 4, dom = "tp", searching = F, scrollX = F))
  
  
  output$sum_weight_draw <- renderText({
    dat <- sum(f2d(graph_data$nodes[,"weight"]))
    # error information about the sum of weights
    shiny::validate(
      need(as.numeric(dat) <= 1, 'Sum of weights should not be larger than 1.')
    )
    paste("Sum of weights:",my_signif(dat,3))
    })
  
  output$graphOutput_visEdges = DT::renderDT({
    result <- graph_data$edges[,c("from","to","label")]
    colnames(result) <- c("from","to","propagation (label)")
    result
  },
  editable = TRUE,
  options = list("pageLength" = 4, dom = "tp", searching = F, scrollX = F))
  
  output$res_Table <- renderTable({
    num <- nrow(graph_data$nodes)
    names <- lapply(1:num, function(i) {paste0("H", i)})
    ini.matrix <- matrix(0,nrow=num,ncol=num)
    colnames(ini.matrix) <- names
    rownames(ini.matrix) <- names
    # transform long to wide table of the transition matrix
    for (i in 1:nrow(graph_data$edges)){
      ini.matrix[which(graph_data$edges$from[i]==rownames(ini.matrix)), 
                 which(graph_data$edges$to[i]==rownames(ini.matrix))] <- as.numeric(graph_data$edges[i,"label"])
    }
    result_draw <- graph_app(matrix=ini.matrix,
                       weights=f2d(graph_data$nodes[,"weight"]),
                       pvalues=as.numeric(graph_data$nodes[,"pvalue"]),
                       alpha = input$alpha_draw,fweights = F)
    result_rej <- data.frame(result_draw$rejected)
    result_rej <- ifelse(result_rej=="TRUE","rejected", "not rejected")
    result_adjp <- result_draw$adjpvalues
    result_draw <- cbind(as.character(names),result_adjp,result_rej)
    colnames(result_draw) <- c("hypotheses","adjusted p-values","rejections")
    result_draw
  },caption = "<b>Rejection table</b>", 
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  output$report <- downloadHandler(
    filename = function() {
      paste('Draw-output', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },  
    content = function(file) {
      src <- normalizePath('report.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      params <- list(alpha_draw = input$alpha_draw,
                     graph_data = graph_data)
      
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ),params = params,
      envir = new.env(parent = globalenv()))
      file.rename(out, file)
    }
  )
  
  
    # ---------------- Procedure Page output ----------------
    df_create <- reactive({
      switch(input$common_procedures,
             "Bonferroni-Holm procedure" = dfcreate(input$Number_Hypotheses,"Bonferroni-Holm procedure"),
             "Fixed sequence test" = dfcreate(input$Number_Hypotheses,"Fixed sequence test"),
             "Fallback procedure" = dfcreate(input$Number_Hypotheses,"Fallback procedure"))
    })
    wp_create <- reactive({
      switch(input$common_procedures,
             "Bonferroni-Holm procedure" = wpcreate(input$Number_Hypotheses,"Bonferroni-Holm procedure"),
             "Fixed sequence test" = wpcreate(input$Number_Hypotheses,"Fixed sequence test"),
             "Fallback procedure" = wpcreate(input$Number_Hypotheses,"Fallback procedure"))
      })
    output$uioutput_Tmatrix1 <- renderUI({
      num <- as.integer(input$Number_Hypotheses)
      names <- lapply(1:num, function(i) {paste0("H", i)})
      df <- df_create()
      rownames(df) <- names
      colnames(df) <- names
      box(status = "primary", solidHeader = TRUE,width = 10, 
          matrixInput(inputId = "TransitionMatrixG",
                      value = df,class = "numeric",
                      cols = list(names = TRUE,extend = FALSE,
                                  editableNames = FALSE,delta = 2),
                      rows = list(names = TRUE, extend = FALSE,
                                  editableNames = FALSE,delta = 1),
                      copy = TRUE,paste = TRUE),
          helpText("The values must be between 0 and 1.")
      )}) 
    
    output$uioutput_Tmatrix2 <- renderUI({
      wp <- wp_create()
      colnames(wp) <- c("weights","p-values")
      box(status = "primary",solidHeader = TRUE,width = 10,
          matrixInput(inputId = "WeightPvalue",
                      value = wp, class = "character",
                      cols = list(names = TRUE, extend = FALSE,
                                  editableNames = FALSE, delta = 2),
                      rows = list(names = TRUE, extend = FALSE,
                                  editableNames = FALSE, delta = 1),
                      copy = TRUE, paste = TRUE))
    })    
    
    output$rej_table <- renderTable({
      names <- paste0("H", 1:input$Number_Hypotheses)
      result_procedure <- graph_app(matrix=input$TransitionMatrixG,
                                   weights=f2d(input$WeightPvalue[,"weights"]),
                                   pvalues=as.numeric(input$WeightPvalue[,"p-values"]),
                                   alpha = input$alpha_procedure,fweights = F)
      result_procedure_rej <- data.frame(result_procedure$rejected)
      result_procedure_rejection <- as.character(ifelse(result_procedure_rej=="TRUE","rejected", "not rejected"))
      result_procedure_adjp <- result_procedure$adjpvalues
      result_procedure_table <- cbind(as.character(names),
                                      result_procedure_adjp,
                                      result_procedure_rejection)
      colnames(result_procedure_table) <- c("hypotheses","adjusted p-values","rejections")
      result_procedure_table
      }, caption = "<b>Rejection table</b>",
    caption.placement = getOption("xtable.caption.placement", "top"),
    caption.width = getOption("xtable.caption.width", NULL))
    
    output$ResultPlot <- renderPlot({
      net <- network(input$TransitionMatrixG,
                     directed = TRUE,
                     names.eval = "weights",
                     ignore.eval = FALSE)
      num <- as.integer(input$Number_Hypotheses2)
      net %v% "vertex.names"  <- rownames(input$TransitionMatrixG)
      e <- network.edgecount(net)
      
      initial <-  ggplot(net, aes(x = x, y = y, xend = xend, yend = yend)) +
        xlim(-0.05, 1.05) + ylim(-0.05, 1.05)+
        geom_edges(arrow = arrow(length = unit(10, "pt"), type = "closed"),
                   color = "black",curvature = 0.15) +
        geom_nodes(aes(x, y),color = "grey",alpha = 0.5, size = 14) +
        geom_nodetext(aes(label = vertex.names)) +
        # geom_edgetext_repel(aes(label = weights), color = "white",
        #                     fill = "grey25",
        #                     box.padding = unit(0.25, "line")) +
        labs(title='Initial graph')+
        theme_blank()+
        theme(aspect.ratio=1,
              plot.title = element_text(size=15, 
                                        margin = margin(15, 0, 15, 0)),
              plot.margin = margin(-1,-1,-1,-1))+  # t r b l
        theme(legend.position = "none")
      
      result_procedure <- graph_app(matrix=input$TransitionMatrixG,
                                   weights=f2d(input$WeightPvalue[,"weights"]),
                                   pvalues=as.numeric(input$WeightPvalue[,"p-values"]),
                                   alpha = input$alpha_procedure,fweights = F)
      res_pvalues <- result_procedure$pvalues
      res_weights <- result_procedure$weights
      res_G <- result_procedure$G
      
      res_net <- network(res_G,directed = TRUE,
                         names.eval = "weights",ignore.eval = FALSE)
      res_net %v% "vertex.names"  <- rownames(input$TransitionMatrixG)
      e <- network.edgecount(res_net)
      result_procedure$rejected <- ifelse(result_procedure$rejected==TRUE,
                                          "rejected","not rejected")
      res_net %v% "Rejection" <- (result_procedure$rejected)
      
      final <- ggplot(res_net, aes(x = x, y = y, xend = xend, yend = yend)) +
        xlim(-0.05, 1.05) + ylim(-0.05, 1.05)+
        geom_edges(arrow = arrow(length = unit(10, "pt"), type = "closed"),
                   color = "black",curvature = 0.15) +
        geom_nodes(aes(x, y, colour = Rejection),alpha = 0.5,size = 14) +
        scale_color_manual(values=c("rejected"="red",
                                    "not rejected"="green"))+
        geom_nodetext(aes(label = vertex.names)) +
        # geom_edgetext_repel(aes(label = weights), color = "white",
        #                     fill = "grey25",
        #                     box.padding = unit(0.25, "line")) +
        # scale_color_brewer(palette = "Set2") +
        labs(title='Final graph')+
        theme_blank()+
        theme(aspect.ratio=1,
              plot.title = element_text(size=15,margin = margin(10, 5, 10, 0)),
              plot.margin = margin(-1,-1,-1,-1))+
        theme(legend.position = "none")
      
      legend_b <- get_legend(final + theme(legend.position="bottom"))
      p <- cowplot::plot_grid(initial,final,
                              label_fontface = "plain",label_fontfamily = "serif",
                              legend_b, ncol = 2, rel_heights = c(1, .2),
                              label_size = 6,
                              label_x = 0, label_y = 0,
                              hjust = -0.5, vjust = -0.5)
      title <- ggdraw() +
        draw_label("Graphical approach for multile test procedures",
          fontface = 'bold',x = 0,hjust = 0) +
        theme(plot.margin = margin(-5, -5, 0, 5))
      
      grid.arrange(initial, final, legend_b, ncol=2, nrow = 2, 
                   layout_matrix = rbind(c(1,2), c(3,3)),
                   widths = c(2.7, 2.7), heights = c(1.5, 0.5))
      })
     # ---------------- Test Page output ----------------
     df_create_test <- reactive({
       switch(input$exRadio,
              "Simple successive procedure" = dfcreate(4,"Simple successive procedure"),
              "Parallel gatekeeping procedure" = dfcreate(4,"Parallel gatekeeping procedure"))
     })
     
     wp_create_test <- reactive({
       switch(input$exRadio,
              "Simple successive procedure" = wpcreate(4,"Simple successive procedure"),
              "Parallel gatekeeping procedure" = wpcreate(4, "Parallel gatekeeping procedure"))
     })
     
     output$uioutput_Tmatrix_df <- renderUI({
       num <- 4
       names <- lapply(1:num, function(i) {paste0("H", i)})
       df <- df_create_test()
       rownames(df) <- names
       colnames(df) <- names
       box(status = "primary", solidHeader = TRUE,width = 10, 
           matrixInput(inputId = "TransitionMatrixG_test",
                       value = df,class = "numeric",
                       cols = list(names = TRUE,extend = FALSE,
                                   editableNames = FALSE,delta = 2),
                       rows = list(names = TRUE, extend = FALSE,
                                   editableNames = FALSE,delta = 1),
                       copy = TRUE,paste = TRUE),
           helpText("The values must be between 0 and 1.")
       )}) 
     
     output$uioutput_Tmatrix_wp <- renderUI({
       wp <- wp_create_test()
       colnames(wp) <- c("weights","p-values")
       box(status = "primary",solidHeader = TRUE,width = 10,
           matrixInput(inputId = "WeightPvalue_test",
                       value = wp, class = "character",
                       cols = list(names = TRUE, extend = FALSE,
                                   editableNames = FALSE, delta = 2),
                       rows = list(names = TRUE, extend = FALSE,
                                   editableNames = FALSE, delta = 1),
                       copy = TRUE, paste = TRUE))
     })    
     
       output$rejtable <- renderTable({
         num <- 4
         wp <- wp_create_test()
         df <- df_create_test()
         names <- lapply(1:num, function(i) {paste0("H", i)})
         rownames(df) <- names
         colnames(df) <- rownames(df)
         result <- graph_app(matrix=input$TransitionMatrixG_test,
                            weights=f2d(input$WeightPvalue_test[,"weights"]),
                            pvalues=as.numeric(input$WeightPvalue_test[,"p-values"]),
                            alpha = input$alpha_test,fweights = F)
         result_rej <- data.frame(result$rejected)
         result_rej <- ifelse(result_rej=="TRUE","rejected", "not rejected")
         result_adjp <- result$adjpvalues
         output <- data.frame(cbind(as.character(names),result_adjp,result_rej))
         colnames(output) <- c("hypotheses","adjusted p-values","rejections")
         output
       }, caption = "<b>Rejection table</b>",
       caption.placement = getOption("xtable.caption.placement", "top"),
       caption.width = getOption("xtable.caption.width", NULL))
       
     output$resPlots_both <- renderPlot({
       num <- 4
       df <- df_create_test()
       wp <- wp_create_test()
       names <- lapply(1:num, function(i) {paste0("H", i)})
       net <- network(input$TransitionMatrixG_test,
                      directed = TRUE,
                      names.eval = "weights",
                      ignore.eval = FALSE)
       num <- 4
       net %v% "vertex.names"  <- names
       e <- network.edgecount(net)
       
       initial <-  ggplot(net, aes(x = x, y = y, xend = xend, yend = yend)) +
         xlim(-0.05, 1.05) + ylim(-0.05, 1.05)+
         geom_edges(arrow = arrow(length = unit(10, "pt"), type = "closed"),
                    color = "black",curvature = 0.15) +
         geom_nodes(aes(x, y),color = "grey",alpha = 0.5, size = 14) +
         geom_nodetext(aes(label = vertex.names)) +
         # geom_edgetext_repel(aes(label = weights), color = "white",
         #                     fill = "grey25",
         #                     box.padding = unit(0.25, "line")) +
         labs(title='Initial graph')+
         theme_blank()+
         theme(aspect.ratio=1,
               plot.title = element_text(size=15, 
                                         margin = margin(10, 0, 10, 0)),
               plot.margin = margin(0.1,0.1,0.1,0.1))    # t r b l
       
  res <- graph_app(matrix=input$TransitionMatrixG_test,
                  weights=f2d(input$WeightPvalue_test[,"weights"]),
                  pvalues=as.numeric(input$WeightPvalue_test[,"p-values"]),
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
  
  final <- ggplot(res_net, aes(x = x, y = y, xend = xend, yend = yend)) +
    xlim(-0.05, 1.05) + ylim(-0.05, 1.05)+
    geom_edges(arrow = arrow(length = unit(10, "pt"), type = "closed"),
               color = "black",
               curvature = 0.15) +
    geom_nodes(aes(x, y,color = Rejection), alpha = 0.5,size = 14) +
    geom_nodetext(aes(label = vertex.names)) +
    scale_color_manual(values=c("rejected"="red",
                                "not rejected"="green"))+
    labs(title='Final graph')+
    theme_blank()+
    theme(legend.position = "none")+
    theme(aspect.ratio=1,
          plot.title = element_text(size=15, 
                                    margin = margin(10, 0, 10, 0)),
          plot.margin = margin(0.1,0.1,0.1,0.1))
  legend_b <- get_legend(final + 
                           theme(legend.position="bottom",
                                 legend.box.margin = margin(0, 0, 0, 15))
                         )
  p <- cowplot::plot_grid(initial,final,
                          label_fontface = "plain",label_fontfamily = "serif",
                          legend_b, ncol = 2, rel_heights = c(1, .2),label_size = 6)
  title <- ggdraw() +
    draw_label("Graphical approach for multile test procedures",
               fontface = 'bold',x = 0,hjust = 0) +
    theme(plot.margin = margin(5, 5, 5, 5))
  # plot_grid(title, p, ncol = 1, rel_heights = c(0.1, 1))+
  #   theme(panel.border = element_rect(colour = "aliceblue",fill=NA))
  grid.arrange(initial, final, legend_b, ncol=2, nrow = 2, 
               layout_matrix = rbind(c(1,2), c(3,3)),
               widths = c(2.7, 2.7), heights = c(1.5, 0.5))
  
})
}
shinyApp(ui, server)
