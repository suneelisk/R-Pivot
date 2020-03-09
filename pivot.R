library(devtools)
# install_github("ramnathv/htmlwidgets") 
# install_github("smartinsightsfromdata/rpivotTable")
# Load rpivotTable
library(rpivotTable)
data = read_xlsx('insurance data.xlsx')
## One line to create pivot table
pivot = rpivotTable(data)

length(pivot$x[[1]])

library(rpivotTable)
library(htmlwidgets)

savedPivot <- "savedPivot.html"
saveWidget(rpivotTable(data),file.path(normalizePath(dirname(savedPivot)),basename(savedPivot)))

if(dim(data)[1]!=0){
  print('Yes')
}else{
  print("NO")
}

library(pivottabler)
pt = qpvt(mtcars, 'cyl', 'vs', 'n()')
pt
