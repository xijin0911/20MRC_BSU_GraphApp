


foot <- tags$footer(
  tagList(
    p("If you have any questions or comments, you can",
      tags$a(href = "https://www.mrc-bsu.cam.ac.uk/people/in-alphabetical-order/n-to-s/david-robertson/",
             "email us."),
      style="font-size:100%; font-family:Arial; line-height:.8rem;"),
    p("Built to implement the graphical approach for multile test procedures."),
    p("GUI created with R Shiny by Xijin Chen and Dr. David Robertson. Code is available on",
      tags$a(
        href = "https://github.com/xijin0911","Github."),
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
