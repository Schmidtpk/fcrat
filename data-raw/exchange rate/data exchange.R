library(tidyverse)
library(ecb)

first.date <- "2000-01-01"
last.date <- "2020-07-31"

cur.currencies <- c("USD","GBP","JPY","CAD","CHF","AUD","CNY")

data <- data.frame()
for (cur.currency in cur.currencies){

  data_input <- get_data(key = paste0("EXR.D.",cur.currency,".EUR.SP00.A"),
                                         filter = list(startPeriod = first.date,
                                                       endPeriod = last.date))

  data_input <- data_input %>%
    rename(
      date = obstime,
      realization = obsvalue
    ) %>%
    mutate(date=as.Date(date))%>%
    select(date,realization) %>%
    na.omit()

  data_input$fc <- lag(data_input$realization)

  data_input$from <- "EUR"
  data_input$to <- cur.currency

  print(range(data_input$date)  )

  data <- rbind(data,data_input)
}

exchange <- data
usethis::use_data(exchange, overwrite = TRUE)
