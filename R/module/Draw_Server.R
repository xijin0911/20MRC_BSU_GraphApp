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