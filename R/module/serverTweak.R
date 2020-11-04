output$uioutput_Tmatrix <- renderUI({
  num <- as.integer(input$Number_Hypotheses)
  df <- (1-diag(num))/(num-1)
  rownames(df) <- lapply(1:num, function(i) {
    paste0("H", i)
  })
  colnames(df) <- rownames(df)
  box(width = 6,
      box(title = "Transition matrix",
          status = "primary", 
          # solidHeader = TRUE,
          width = 6, 
          # collapsible = TRUE,collapsed = TRUE,
          helpText(""),
          matrixInput(inputId = "TransitionMatrixG",
                      value = df,class = "numeric",
                      cols = list(
                        names = TRUE,
                        # extend = FALSE,
                        editableNames = FALSE,
                        delta = 2),
                      rows = list(
                        names = TRUE, 
                        # extend = FALSE,
                        editableNames = FALSE,
                        delta = 1),
                      copy = TRUE,paste = TRUE)),
      box(title = "Weights and P-values",
          status = "primary",
          # solidHeader = TRUE,
          width = 6,
          # collapsible = TRUE,collapsed = TRUE,
          matrixInput(inputId = "WeightPvalue",
                      value = matrix(cbind(
                        (lapply(1:num, function(i) {
                          paste0("H", i)
                        })),rep(1/num,num),rep(0.01,num)),
                        nrow = num, ncol = 3,
                        dimnames = list(NULL, c("Hypotheses", "Weights",'P-values'))),
                      cols = list(names = TRUE, 
                                  # extend = FALSE,
                                  editableNames = FALSE, delta = 2),
                      rows = list(
                        names = FALSE, 
                        # extend = FALSE,
                        editableNames = FALSE, delta = 1),
                      copy = TRUE, paste = TRUE)
      ),
  )
})

output$all_nodes = renderUI({
  num <- as.integer(input$Number_Hypotheses2)
  df <- (1-diag(num))/(num-1)
  rownames(df) <- lapply(1:num, function(i) {
    paste0("H", i)
  })
  colnames(df) <- rownames(df)
  box(width = 10,
      matrixInput(inputId = "WeightPvalue2",
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
  )
})

output$all_edges = renderUI({
  num <- as.integer(input$Number_Hypotheses2)
  df <- (1-diag(num))/(num-1)
  rownames(df) <- lapply(1:num, function(i) {
    paste0("H", i)
  })
  colnames(df) <- rownames(df)
  box(width = 10,
      matrixInput(inputId = "TransitionMatrixG2",
                  value = df,class = "numeric",
                  cols = list(
                    names = TRUE,extend = FALSE,
                    editableNames = TRUE,delta = 2),
                  rows = list(
                    names = TRUE, extend = FALSE,
                    editableNames = TRUE,delta = 1),
                  copy = TRUE,paste = TRUE)
  )
})


output$uioutput_Tmatrix <- renderUI({
  num <- as.integer(input$Number_Hypotheses)
  df <- (1-diag(num))/(num-1)
  rownames(df) <- lapply(1:num, function(i) {
    paste0("H", i)
  })
  colnames(df) <- rownames(df)
  box(width = 9,
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