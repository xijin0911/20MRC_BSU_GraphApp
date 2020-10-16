visOption_xc <- function(graph,
                       width = NULL,
                       height = NULL,
                       highlightNearest = FALSE,
                       nodesIdSelection = FALSE,
                       selectedBy = NULL,
                       collapse = FALSE,
                       autoResize = NULL,
                       clickToUse = NULL,
                       manipulation = NULL){
  
  if(!any(class(graph) %in% c("visNetwork", "visNetwork_Proxy"))){
    stop("graph must be a visNetwork or a visNetworkProxy object")
  }
  
  options <- list()
  
  options$autoResize <- autoResize
  options$clickToUse <- clickToUse
  
  if(is.null(manipulation)){
    options$manipulation <- list(enabled = FALSE)
  }else{   
    # ---- diff1 ----
    graph$x$opts_manipulation$datacss <- paste(readLines(system.file("htmlwidgets/lib/css/dataManipulation.css", package = "visNetwork"), warn = FALSE), collapse = "\n")
    # ---- diff1 ----
    if(is.logical(manipulation)){
      options$manipulation <- list(enabled = manipulation)
      
    } else if(is.list(manipulation)){
      options$manipulation <- manipulation
    } else {
      stop("Invalid 'manipulation' argument. logical or list")
    }
    # ---- diff2 ----
    if(!"addNodeCols" %in% names(manipulation)){
      graph$x$opts_manipulation$addNodeCols <- c("id", "label")
      addNodeCols_html_input_type <- rep("text", 2)
    } else {
      if(is.list(manipulation$addNodeCols)){
        graph$x$opts_manipulation$addNodeCols <- unname(do.call("c", manipulation$addNodeCols))
        addNodeCols_html_input_type <- rep(names(manipulation$addNodeCols), sapply(manipulation$addNodeCols, length))
      } else if(is.vector(manipulation$addNodeCols)){
        graph$x$opts_manipulation$addNodeCols <- manipulation$addNodeCols
        addNodeCols_html_input_type <- rep("text", length(manipulation$addNodeCols))
      }
      options$manipulation$addNodeCols <- NULL
    }
    # ---- diff2 ----
    if(!"editNodeCols" %in% names(manipulation)){
      graph$x$opts_manipulation$editNodeCols <- c("id", "label")
      editNodeCols_html_input_type <- rep("text", 2)
    } else {
      if(is.list(manipulation$editNodeCols)){
        graph$x$opts_manipulation$editNodeCols <- unname(do.call("c", manipulation$editNodeCols))
        editNodeCols_html_input_type <- rep(names(manipulation$editNodeCols), sapply(manipulation$editNodeCols, length))
      } else if(is.vector(manipulation$editNodeCols)){
        graph$x$opts_manipulation$editNodeCols <- manipulation$editNodeCols
        editNodeCols_html_input_type <- rep("text", length(manipulation$editNodeCols))
      }
      options$manipulation$editNodeCols <- NULL
    }
    # ---- diff2 ----
    if("editEdgeCols" %in% names(manipulation) && !is.null(manipulation$editEdgeCols) && length(manipulation$editEdgeCols) > 0){
      if(is.list(manipulation$editEdgeCols)){
        graph$x$opts_manipulation$editEdgeCols <- unname(do.call("c", manipulation$editEdgeCols))
        editEdgeCols_html_input_type <- rep(names(manipulation$editEdgeCols), sapply(manipulation$editEdgeCols, length))
      } else if(is.vector(manipulation$editEdgeCols)){
        graph$x$opts_manipulation$editEdgeCols <- manipulation$editEdgeCols
        editEdgeCols_html_input_type <- rep("text", length(manipulation$editEdgeCols))
      } 
      options$manipulation$editEdgeCols <- NULL
    } 
    # ---- diff2 ----
    if(length(graph$x$opts_manipulation$addNodeCols) == 1){
      graph$x$opts_manipulation$addNodeCols <- list(graph$x$opts_manipulation$addNodeCols)
    }
    if(length(graph$x$opts_manipulation$editNodeCols) == 1){
      graph$x$opts_manipulation$editNodeCols <- list(graph$x$opts_manipulation$editNodeCols)
    }
    if(length(graph$x$opts_manipulation$editEdgeCols) == 1){
      graph$x$opts_manipulation$editEdgeCols <- list(graph$x$opts_manipulation$editEdgeCols)
    }
    
    if(!is.null(graph$x$opts_manipulation$addNodeCols)){
      graph$x$opts_manipulation$tab_add_node <- build_manipulation_table(
        col = graph$x$opts_manipulation$addNodeCols, 
        type = addNodeCols_html_input_type,
        id = "addnode")
    }
    
    if(!is.null(graph$x$opts_manipulation$editNodeCols)){
      graph$x$opts_manipulation$tab_edit_node <- build_manipulation_table(
        col = graph$x$opts_manipulation$editNodeCols, 
        type = editNodeCols_html_input_type,
        id = "editnode") 
    }
    
    if(!is.null(graph$x$opts_manipulation$editEdgeCols)){
      graph$x$opts_manipulation$tab_edit_edge <- build_manipulation_table(
        col = graph$x$opts_manipulation$editEdgeCols,
        type = editEdgeCols_html_input_type, 
        id = "editedge") 
    }
    
  }
  # ---- diff2 ----
  options$height <- height
  options$width <- width
  
  if(!"nodes"%in%names(graph$x) && any(class(graph) %in% "visNetwork")){
    highlight <- list(enabled = FALSE)
    idselection <- list(enabled = FALSE)
    byselection <- list(enabled = FALSE)
    list_collapse <- list(enabled = FALSE, fit = FALSE, resetHighlight = TRUE, 
                          keepCoord = TRUE, labelSuffix = "(cluster)")
  }else{
    #############################
    # collapse
    #############################
    list_collapse <- list(enabled = FALSE, fit = FALSE, resetHighlight = TRUE, 
                          clusterOptions = NULL, keepCoord = TRUE, labelSuffix = "(cluster)")
    if(is.list(collapse)){
      if(any(!names(collapse)%in%c("enabled", "fit", "resetHighlight", "clusterOptions", "keepCoord", "labelSuffix"))){
        stop("Invalid 'collapse' argument")
      }
      
      if("enabled"%in%names(collapse)){
        stopifnot(is.logical(collapse$enabled))
        list_collapse$enabled <- collapse$enabled
      }
      if("fit"%in%names(collapse)){
        stopifnot(is.logical(collapse$fit))
        list_collapse$fit <- collapse$fit
      }
      if("resetHighlight"%in%names(collapse)){
        stopifnot(is.logical(collapse$resetHighlight))
        list_collapse$resetHighlight <- collapse$resetHighlight
      }
      if("keepCoord"%in%names(collapse)){
        stopifnot(is.logical(collapse$keepCoord))
        list_collapse$keepCoord <- collapse$keepCoord
      }
      if("labelSuffix"%in%names(collapse)){
        stopifnot(is.character(collapse$labelSuffix))
        list_collapse$labelSuffix <- collapse$labelSuffix
      }
      if("clusterOptions"%in%names(collapse)){
        stopifnot(is.list(collapse$clusterOptions))
        list_collapse$clusterOptions <- collapse$clusterOptions
      }
    } else {
      stopifnot(is.logical(collapse))
      list_collapse$enabled <- collapse
    }
    
    #############################
    # highlightNearest
    #############################
    highlight <- list(enabled = FALSE, hoverNearest = FALSE, degree = 1, algorithm = "all", hideColor = 'rgba(200,200,200,0.5)', labelOnly = TRUE)
    if(is.list(highlightNearest)){
      if(any(!names(highlightNearest)%in%c("enabled", "degree", "hover", "algorithm", "hideColor", "labelOnly"))){
        stop("Invalid 'highlightNearest' argument")
      }
      
      if("algorithm"%in%names(highlightNearest)){
        stopifnot(highlightNearest$algorithm %in% c("all", "hierarchical"))
        highlight$algorithm <- highlightNearest$algorithm
      }
      
      if("hideColor"%in%names(highlightNearest)){
        highlight$hideColor <- highlightNearest$hideColor
      }
      
      if("degree"%in%names(highlightNearest)){
        highlight$degree <- highlightNearest$degree
      }
      
      if(highlight$algorithm %in% "hierarchical"){
        if(is.list(highlight$degree)){
          stopifnot(all(names(highlight$degree) %in% c("from", "to")))
        }else{
          highlight$degree <- list(from = highlight$degree, to = highlight$degree)
        }
      }
      
      if("labelOnly"%in%names(highlightNearest)){
        stopifnot(is.logical(highlightNearest$labelOnly))
        highlight$labelOnly <- highlightNearest$labelOnly
      }
      
      if("hover"%in%names(highlightNearest)){
        stopifnot(is.logical(highlightNearest$hover))
        highlight$hoverNearest <- highlightNearest$hover
      }
      
      if("enabled"%in%names(highlightNearest)){
        stopifnot(is.logical(highlightNearest$enabled))
        highlight$enabled <- highlightNearest$enabled
      }
      
    } else {
      stopifnot(is.logical(highlightNearest))
      highlight$enabled <- highlightNearest
    }
    
    if(highlight$enabled && any(class(graph) %in% "visNetwork")){
      if(!"label"%in%colnames(graph$x$nodes)){
        if(is.data.frame(graph$x$nodes)){
          graph$x$nodes$label <- as.character(graph$x$nodes$id)
        } else if(is.list(graph$x$nodes)){
          ctrl <- lapply(1:length(graph$x$nodes), function(x){
            graph$x$nodes[[x]]$label <<- as.character(graph$x$nodes[[x]]$id)
          })
        }
      }
      # if(!"group"%in%colnames(graph$x$nodes)){
      #   if(is.data.frame(graph$x$nodes)){
      #     graph$x$nodes$group <- 1
      #   } else if(is.list(graph$x$nodes)){
      #     ctrl <- lapply(1:length(graph$x$nodes), function(x){
      #       graph$x$nodes[[x]]$group <<- 1
      #     })
      #   }
      # }
    }
    
    #############################
    # nodesIdSelection
    #############################
    idselection <- list(enabled = FALSE, style = 'width: 150px; height: 26px', useLabels = TRUE, main = "Select by id")
    if(is.list(nodesIdSelection)){
      if(any(!names(nodesIdSelection)%in%c("enabled", "selected", "style", "values", "useLabels", "main"))){
        stop("Invalid 'nodesIdSelection' argument. List can have 'enabled', 'selected', 'style', 'values', 'useLabels', 'main'")
      }
      if("selected"%in%names(nodesIdSelection)){
        if(any(class(graph) %in% "visNetwork")){
          if(!nodesIdSelection$selected%in%graph$x$nodes$id ){
            stop(nodesIdSelection$selected, " not in data. nodesIdSelection$selected must be valid.")
          }
        }
        idselection$selected <- nodesIdSelection$selected
      }
      if("enabled"%in%names(nodesIdSelection)){
        idselection$enabled <- nodesIdSelection$enabled
      }else{
        idselection$enabled <- TRUE
      }
      
      if("main"%in%names(nodesIdSelection)){
        idselection$main <- nodesIdSelection$main
      }
      
      if("useLabels"%in%names(nodesIdSelection)){
        idselection$useLabels <- nodesIdSelection$useLabels
      }else if(any(class(graph) %in% "visNetwork_Proxy")){
        idselection$useLabels <- NULL
      }
      
      if("style"%in%names(nodesIdSelection)){
        idselection$style <- nodesIdSelection$style
      }else if(any(class(graph) %in% "visNetwork_Proxy")){
        idselection$style <- NULL
      }
      
    }else if(is.logical(nodesIdSelection)){
      idselection$enabled <- nodesIdSelection
      if(any(class(graph) %in% "visNetwork_Proxy")){
        idselection$useLabels <- NULL
        idselection$style <- NULL
      }
    }else{
      stop("Invalid 'nodesIdSelection' argument")
    }
    
    if(idselection$enabled){
      if("values"%in%names(nodesIdSelection)){
        idselection$values <- nodesIdSelection$values
        if(length(idselection$values) == 1){
          idselection$values <- list(idselection$values)
        }
        if("selected"%in%names(nodesIdSelection)){
          if(!idselection$selected%in%idselection$values){
            stop(idselection$selected, " not in data/selection. nodesIdSelection$selected must be a valid value.")
          }
        }
      }
    }
    
    #############################
    # selectedBy
    #############################
    byselection <- list(enabled = FALSE, style = 'width: 150px; height: 26px', multiple = FALSE, 
                        hideColor = 'rgba(200,200,200,0.5)', highlight = FALSE)
    
    if(!is.null(selectedBy)){
      if(is.list(selectedBy)){
        if(any(!names(selectedBy)%in%c("variable", "selected", "style", "values", "multiple", "hideColor", "main", "sort", "highlight"))){
          stop("Invalid 'selectedBy' argument. List can have 'variable', 'selected', 'style', 'values', 'multiple', 'hideColor', 'main', 'sort', 'highlight'")
        }
        if("selected"%in%names(selectedBy)){
          byselection$selected <- as.character(selectedBy$selected)
        }
        
        if("hideColor"%in%names(selectedBy)){
          byselection$hideColor <- selectedBy$hideColor
        }
        
        if("highlight"%in%names(selectedBy)){
          byselection$highlight <- selectedBy$highlight
        }
        
        if(!"variable"%in%names(selectedBy)){
          stop("'selectedBy' need at least 'variable' information")
        }
        
        byselection$variable <- selectedBy$variable
        
        if("main" %in% names(selectedBy)){
          byselection$main <- selectedBy$main
        } else {
          byselection$main <- paste0("Select by ", selectedBy$variable)
        }
        
        if("style"%in%names(selectedBy)){
          byselection$style <- selectedBy$style
        }else if(any(class(graph) %in% "visNetwork_Proxy")){
          byselection$style <- NULL
        }
        
        if("multiple"%in%names(selectedBy)){
          byselection$multiple <- selectedBy$multiple
        }else if(any(class(graph) %in% "visNetwork_Proxy")){
          byselection$multiple <- NULL
        }
        
      }else if(is.character(selectedBy)){
        byselection$variable <- selectedBy
        
        byselection$main <- paste0("Select by ", selectedBy)
        
        if(any(class(graph) %in% "visNetwork_Proxy")){
          byselection$style <- NULL
          byselection$multiple <- NULL
        }
        
      }else{
        stop("Invalid 'selectedBy' argument. Must a 'character' or a 'list'")
      }
      
      if(any(class(graph) %in% "visNetwork_Proxy")){
        byselection$enabled <- TRUE
        
        if("values"%in%names(selectedBy)){
          if(length(selectedBy$values) > 1){
            byselection$values <- selectedBy$values
          } else {
            byselection$values <- list(selectedBy$values)
          }
        }
        
        if("selected"%in%names(byselection)){
          byselection$selected <- byselection$selected
        }
      }else{
        if(!byselection$variable%in%colnames(graph$x$nodes)){
          warning("Can't find '", byselection$variable, "' in node data.frame")
        }else{
          byselection$enabled <- TRUE
          byselection$values <- unique(graph$x$nodes[, byselection$variable])
          if(byselection$multiple){
            byselection$values <- unique(gsub("^[[:space:]]*|[[:space:]]*$", "",
                                              do.call("c",strsplit(as.character(byselection$values), split = ","))))
          }
          if(any(c("integer", "numeric") %in% class(graph$x$nodes[, byselection$variable]))){
            byselection$values <- byselection$values
          }else{
            byselection$values <- as.character(byselection$values)
          }
          
          if("sort"%in%names(selectedBy)){
            if(selectedBy$sort){
              byselection$values <- sort(byselection$values)
            }
          } else {
            byselection$values <- sort(byselection$values)
          }
          
          if("values"%in%names(selectedBy)){
            # byselection$values <- intersect(byselection$values, selectedBy$values)
            byselection$values <- selectedBy$values
          }
          
          if("values"%in%names(byselection)){
            if(length(byselection$values) == 1){
              byselection$values <- list(byselection$values)
            }
          }
          
          if("selected"%in%names(byselection)){
            if(!byselection$selected%in%byselection$values){
              stop(byselection$selected, " not in data/selection. selectedBy$selected must be a valid value.")
            }
            byselection$selected <- byselection$selected
          }
          
          if(!"label"%in%colnames(graph$x$nodes)){
            if(is.data.frame(graph$x$nodes)){
              graph$x$nodes$label <- ""
            } else if(is.list(graph$x$nodes)){
              ctrl <- lapply(1:length(graph$x$nodes), function(x){
                graph$x$nodes[[x]]$label <<- ""
              })
            }
          }
          # if(!"group"%in%colnames(graph$x$nodes)){
          #   if(is.data.frame(graph$x$nodes)){
          #     graph$x$nodes$group <- 1
          #   } else if(is.list(graph$x$nodes)){
          #     ctrl <- lapply(1:length(graph$x$nodes), function(x){
          #       graph$x$nodes[[x]]$group <<- 1
          #     })
          #   }
          # }
        }
      }
    }
  }
  
  # x <- list(highlight = highlightNearest, hoverNearest = hoverNearest, degree = degree, 
  #           idselection = idselection, byselection = byselection)
  
  x <- list(highlight = highlight, idselection = idselection, byselection = byselection, collapse = list_collapse)
  
  if(highlight$hoverNearest){
    graph <- visInteraction(graph, hover = TRUE)
  }
  
  if(any(class(graph) %in% "visNetwork_Proxy")){
    
    data <- list(id = graph$id, options = options)
    graph$session$sendCustomMessage("visShinyOptions",data)
    
    if(missing(highlightNearest)){
      x$highlight <- NULL
    }
    if(missing(nodesIdSelection)){
      x$idselection <- NULL
    }
    if(missing(selectedBy)){
      x$byselection <- NULL
    }
    if(missing(collapse)){
      x$collapse <- NULL
    }
    
    data <- list(id = graph$id, options = x)
    graph$session$sendCustomMessage("visShinyCustomOptions",data)
    
  }else{
    graph$x <- mergeLists(graph$x, x)
    graph$x$options <- mergeLists(graph$x$options, options)
  }
  graph
}


build_manipulation_table <- function(col, type, id = "node"){
  
  if(length(col) > 0){
    table <- paste0('<span id="', id, '-operation" class = "operation">node</span> <br><table style="margin:auto;">')
    
    for(i in 1:length(col)){
      
      add <- paste0('<tr><td>', col[i], '</td><td><input id="', id, "-", col[i], '"  type= "', type[i], '" value="new value"></td></tr>')
      table <- paste0(table, add)
    }
    
    table <- paste0(table, '</table><input type="button" value="save" id="', id, '-saveButton"></button><input type="button" value="cancel" id="', id, '-cancelButton"></button>')
  } else {
    table <- ""
  }
  
  table
  
}