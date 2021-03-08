


foot <- tags$footer(
  tagList(
    p("A web app implementation of the graphical approach for multiple testing."),
    p("If you have any questions or comments, you can email us at",
      tags$a(href = "mailto:xijin.chen@mrc-bsu.cam.ac.uk",
             "xijin.chen@mrc-bsu.cam.ac.uk"),
      style="font-size:100%; font-family:Arial; line-height:.8rem;"),
    p("GUI created with R Shiny by Xijin Chen and Dr. David Robertson. Code is available on",
      tags$a(href = "https://github.com/xijin0911/20MRC_BSU_GraphApp","Github."),
      style="font-size:100%; font-family:Arial; line-height:.8rem;"
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
