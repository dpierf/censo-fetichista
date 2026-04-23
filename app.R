rm(list=ls()) ; gc() ; cat('\014')

source('global.R', encoding = 'UTF-8')
source('ui.R',     encoding = 'UTF-8')
source('server.R', encoding = 'UTF-8')

shinyApp(ui = ui, server = server)
