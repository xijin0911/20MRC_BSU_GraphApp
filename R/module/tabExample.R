tabexample <- tabPanel("examples",
                       tags$script(src = "net.js"),
                       box("Examples",width = "16 col-md-8"),
                       box(width = "8 col-md-4",
                           components$examples$team
                       ))