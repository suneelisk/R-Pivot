library(shiny)
library(rpivotTable)
data = read_xlsx('insurance data.xlsx')
js <- "
function filter(node){
  return (node.tagName !== 'i');
}
function exportPlot(filename){
  var plot = document.getElementsByClassName('pvtRendererArea');
  domtoimage.toPng(plot[0], {filter: filter, bgcolor: 'white'})
    .then(function (dataUrl) {
      var link = document.createElement('a');
      link.download = filename;
      link.href = dataUrl;
      link.click();
    });
}
Shiny.addCustomMessageHandler('export', exportPlot);
"

ui <- fluidPage(
  tags$head(
    tags$script(src = "dom-to-image.min.js"),
    tags$script(HTML(js))
  ),
  br(),
  rpivotTableOutput("pivotbl"),
  br(),
  actionButton("export", "Export")
)

server <- function(input, output, session){
  
  pivotTable <- rpivotTable(
    data
  )
  
  output[["pivotbl"]] <- renderRpivotTable({
    pivotTable
  })
  
  observeEvent(input[["export"]], {
    session$sendCustomMessage("export", "plot.png")
  })  
  
}

shinyApp(ui, server)
