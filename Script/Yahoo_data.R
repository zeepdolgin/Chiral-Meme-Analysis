library(tidyquant)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

getSymbols("BB", from = '2020-11-01',
           to = "2021-06-25",warnings = FALSE,
           auto.assign = TRUE)

# Displaying BB Price:

chart_Series(BB)

chart_Series(BB['2021-06-18/2021-06-25'])
