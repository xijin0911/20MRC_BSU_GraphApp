

components <- list(toolbar = list())
# Components - footer ----

components$foot <- tags$footer(
  tagList(
    p("All code and detailed instructions for usage is available on",
      tags$a(
        href = "https://github.com/xijin0911","GitHub")
    ),
    p(
      "If you have any questions or comments, you can email us at ",
      tags$a(href = "chenxijin2017@gmail.com", "chenxijin2017@gmail.com"),
    )
  ),
  align = "center",
  style = "
      * {
    margin: 0;
  }
  html, body {
    height: 100%;
    width:100%;
  }
  .wrapper {
    min-height: 100%;
    height: auto !important; /* This line and the next line are not necessary unless you need IE6 support */
    height: 100%;
    margin: 0 auto -155px; /* the bottom margin is the negative value of the footer's height */
  }
  .footer, .push {
    height: 155px; /* .push must be the same height as .footer */
  }

  /*

  Sticky Footer by Ryan Fait
  http://ryanfait.com/

  */"
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