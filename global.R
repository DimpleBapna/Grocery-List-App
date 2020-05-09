# DO NOT TOUCH THIS FILE!!!xx
# IT IS NOT SUPPOSED TO BE TAMPERED 

library(shiny)
source("./server/utilities.R")
source("./server/server.R")
source("./ui/ui.R")

# Run the application 
shinyApp(ui = ui, server = server)
