tabTest <- tabPanel("Case studies", 
                    icon=icon("tasks", lib = "glyphicon"),
                    fluidRow(
                      column(4,style = "background-color: skyblue;",
                h2("Clinical trials", align = "center"),
                HTML("<p>Trial compares two doses <em>D1</em> or <em>D2</em> against placebo in diabetes patients 
                  for two endpoints.</p>"),
                tags$div(style = "font-size:12px;",
                  tags$ul(
                    tags$li("Primary endpoint: HbA1c"),
                    tags$li("Secondary endpoint: Body weight")
                  )
                ),
                HTML("<p>Both doses are equally important (<em>w1=w2</em>). 
                There is a natural order: a primary endpoint is more important than a secondary endpoint. 
                We test the primary null hypotheses first (<em>H1</em> and <em>H2</em>); 
                only if this is rejected do we test the secondary hypotheses (<em>H3</em> and <em>H4</em>).</p>"),
                hr(),
                prettyRadioButtons(
                  inputId = "exRadio",
                  label = "Specific procedures",
                  choices = c("Simple successive procedure",
                              "Parallel gatekeeping procedure"),
                  shape = "round",
                  fill = TRUE,
                  inline = TRUE),
                conditionalPanel(
                  condition = "input.exRadio == 'Simple successive procedure'",
                  HTML("<p>The graph initially has weights 0 on both secondary hypotheses (<em>H3</em> and <em>H4</em>) 
                  and the only edges with positive weight leading into a secondary hypothesis are 
                  those originating at its parent primary hypotheses (<em>H1->H3</em> and <em>H2->H4</em>) 
                  and there are no edges leading from a secondary hypothesis to another secondary hypothesis that 
                  has not the same parents (<em>H3</em> and <em>H4</em>), 
                  then the rejection algorithm generates a <b>successive procedure</b>.</p>")
                ),
                conditionalPanel(
                  condition = "input.exRadio == 'Parallel gatekeeping procedure'",
                  HTML("<p>The requirement to reject all primary trial hypotheses before 
                  performing the secondary analyses is inappropriate in this case. 
                  Instead, one examines the higher doses first and studies the lower doses 
                  if <b>at least one</b> of the higher doses (<b>but not necessarily both</b>) has 
                  shown a signicant difference from the control.</p>"),
                      # one may proceed to secondary family tests when at least one of the primary tests exhibits significance.")
                ),
                
                hr(),
                p(style="font-family:courier;","Reference"),
                conditionalPanel(
                  condition = "input.exRadio == 'Simple successive procedure'",
                  div(HTML("Maurer, W., Glimm, E., & Bretz, F. (2011). Multiple and repeated testing of primary, coprimary, and secondary hypotheses. <i>Statistics in Biopharmaceutical Research</i>, 3(2), 336-352."))),
                conditionalPanel(
                  condition = "input.exRadio == 'Parallel gatekeeping procedure'",
                  div(HTML("Dmitrienko, A., Offen, WW, & Westfall, PH (2003). Gatekeeping strategies for clinical trials that do not require all primary effects to be significant. <i>Statistics in medicine</i>, 22 (15), 2387-2400."))
                ),br()
         ),
         column(4,
                h2("Details",align = "center"),
                numericInput(inputId = "alpha_test", 
                             label = HTML("Total &alpha;"),
                             value = 0.05,step = 0.001,min = 0),
                actionButton("wp_infor2", HTML("Weights <em>w</em> and <em>p</em>-values (Nodes)"),
                             icon("paper-plane"),width = "250px",
                             style="background-color: AliceBlue;
                                            border-color: AliceBlue"),
                br(),
                HTML("The initial local levels <em>&alpha;3</em> and <em>&alpha;4</em> are set as 0, 
                     as we do not want to reject a secondary hypothesis until its parent primary hypothesis is rejected."),
                withSpinner(tableOutput("uioutput_Tmatrix_wp")),
                br(),br(),
                actionButton("G_infor2", HTML("Transition matrix <em>G</em> (Edges)"), 
                             icon("paper-plane"),width = "220px",
                             style="background-color: AliceBlue;
                                            border-color: AliceBlue"),
                br(), 
                conditionalPanel(
                  condition = "input.exRadio == 'Simple successive procedure'",
                  HTML("<p><em>H3</em> has only one parent hypothesis <em>H1</em>, and 
                <em>H4</em> has only one parent hypothesis <em>H2</em>.</p>")
                ),
                # HTML("The propagation of significance levels between two connected hypotheses."),
                # bsTooltip("G_infor2", "The propagation of significance levels",
                #           "right", options = list(container = "body")),
                withSpinner(tableOutput("uioutput_Tmatrix_df")),
                br(),
                shinyjs::useShinyjs()),
         column(4,style="background-color: AliceBlue;border-color: AliceBlue;padding:8px; font-size:80%;",
                h2("Results", align = "center"),
                withSpinner(tableOutput("rejtable")),
                withSpinner(plotOutput("resPlots_both"))
         ))
)
         
