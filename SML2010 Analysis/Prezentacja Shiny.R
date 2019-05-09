library(shinydashboard)
library(ggplot2)
library(dplyr)
library(chron)
library(readr)
library(plotly)


ui <- dashboardPage(
  dashboardHeader(title = "Analiza"),
  dashboardSidebar(sidebarMenu(
    menuItem("Wiatr", tabName = "wiatr", icon = icon("wia")),
    menuItem("Temperatura", tabName = "temp", icon = icon("temp")),
    menuItem("Wilgotność", tabName = "wilg", icon = icon("wilg")),
    menuItem("CO2", tabName = "co2", icon = icon("co2")),
    menuItem("Oswietlenie", tabName = 'lux', icon = icon("co2"))
  )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "wiatr",
              fluidRow(
                box(plotlyOutput("plot7"), width = 600),
                box(plotlyOutput("plot8"), width = 600)
              )
      ),
      tabItem(tabName = "temp",
    fluidRow(
      box(plotlyOutput("plot1"), width = 600),
      box(plotlyOutput("plot2"), width = 600)
    )
  ),
  tabItem(tabName = 'wilg',
          fluidRow(
            box(plotlyOutput("plot3"), width = 600),
            box(plotlyOutput("plot4"), width = 600)
          )
          ),
  tabItem(tabName = 'co2',
          fluidRow(
            box(plotlyOutput("plot5"), width = 600),
            box(plotlyOutput("plot6"), width = 600),
            box(plotlyOutput("plot11"), width = 600)
          )
  ),
  tabItem(tabName = 'lux',
          fluidRow(
            box(plotlyOutput("plot9"), width = 600),
            box(plotlyOutput("plot10"), width = 600),
            box(plotlyOutput("plot12"), width = 600)
          )
  )
)
)
)
server <- function(input, output) {
  set.seed(122)
  data_train <- read_table2("NEW-DATA-1.T15.txt")
  data_test <- read_table2("NEW-DATA-2.T15.txt")
  data_all <- rbind(data_train, data_test)
  data_all <- as.data.frame(data_all)
  Date_time = chron(dates=data_all[,1],times=data_all[,2],format=c('d/m/y','h:m:s'))
  data_all <- as.data.frame(data_all)
  data_all[,25] <- Date_time
  colnames(data_all)[25] <- "Data"
  
  output$plot1 <- renderPlotly({
    x <- list(
      title = "Data"
    )
    y <- list(
      title = "Stopnie Celsjusza"
    )
   plot_ly(data_all, x = ~Data, y =~`22:Temperature_Exterior_Sensor`, name = 'Na zewnątrz', type = 'scatter', mode = 'markers',
           marker=list(color="green")) %>%
           add_trace(y = ~`4:Temperature_Habitacion_Sensor`, name = 'Pokój',
           type = 'scatter', mode = 'markers', marker=list(color="red")) %>%
      add_trace(y = ~`3:Temperature_Comedor_Sensor`, name = 'Jadalnia', type = 'scatter', mode = 'markers', marker=list(color="blue")) %>%
      layout(xaxis = x, yaxis = y)
  })

  output$plot2 <- renderPlotly({
    x <- list(
      title = "Data"
    )
    y <- list(
      title = "Stopnie Celsjusza"
    )
    plot_ly(data_all, y =~`22:Temperature_Exterior_Sensor`, name = 'Na zewnątrz', type = 'box'
            , line=list(color = 'green'), marker=list(color="green")) %>%
      add_trace(y = ~`4:Temperature_Habitacion_Sensor`, name = 'Pokój', line = list(color = 'red'), marker=list(color="red")) %>%
      add_trace(y = ~`3:Temperature_Comedor_Sensor`, name = 'Jadalnia',line = list(color = 'blue'), marker=list(color="blue")) %>%
      layout(xaxis = x, yaxis = y)
  })
  
  output$plot3 <- renderPlotly({
    x <- list(
      title = "Data"
    )
    y <- list(
      title = "%"
    )
    plot_ly(data_all, x = ~Data, y =~`23:Humedad_Exterior_Sensor`, name = 'Na zewnątrz', type = 'scatter', mode = 'markers',
            marker=list(color="green")) %>%
      add_trace(y = ~`9:Humedad_Habitacion_Sensor`, name = 'Pokój',
                type = 'scatter', mode = 'markers', marker=list(color="red")) %>%
      add_trace(y = ~`8:Humedad_Comedor_Sensor`, name = 'Jadalnia', type = 'scatter', mode = 'markers', marker=list(color="blue")) %>%
      layout(xaxis = x, yaxis = y)
  })
  
  output$plot4 <- renderPlotly({
    x <- list(
      title = "Data"
    )
    y <- list(
      title = "%"
    )
    plot_ly(data_all, y =~`23:Humedad_Exterior_Sensor`, name = 'Na zewnątrz', type = 'box'
            , line=list(color = 'green'), marker=list(color="green")) %>%
      add_trace(y = ~`9:Humedad_Habitacion_Sensor`, name = 'Pokój', line = list(color = 'red'), marker=list(color="red")) %>%
      add_trace(y = ~`8:Humedad_Comedor_Sensor`, name = 'Jadalnia',line = list(color = 'blue'), marker=list(color="blue")) %>%
      layout(xaxis = x, yaxis = y)
  })
  
  output$plot5 <- renderPlotly({
    x <- list(
      title = "Data"
    )
    y <- list(
      title = "ppm"
    )
    plot_ly(data_all, x = ~Data, y =~`7:CO2_Habitacion_Sensor`, name = 'Pokój', type = 'scatter', mode = 'markers',
            marker=list(color="red")) %>%
      add_trace(y = ~`6:CO2_Comedor_Sensor`, name = 'Jadalnia',
                type = 'scatter', mode = 'markers', marker=list(color="blue")) %>%
      layout(xaxis = x, yaxis = y)
  })
  
  output$plot6 <- renderPlotly({
    x <- list(
      title = "Data"
    )
    y <- list(
      title = "ppm"
    )
    plot_ly(data_all, y =~`7:CO2_Habitacion_Sensor`, name = 'Pokój', type = 'box'
            , line=list(color = 'red'), marker=list(color="red")) %>%
      add_trace(y = ~`6:CO2_Comedor_Sensor`, name = 'Jadalnia', marker=list(color="blue"), line = list(color = 'blue')) %>%
      layout(xaxis = x, yaxis = y)
  })
  
  output$plot7 <- renderPlotly({
    x <- list(
      title = "Data"
    )
    y <- list(
      title = "m/s"
    )
    plot_ly(data_all[1:1000,], x = ~Data, y =~`14:Meteo_Exterior_Viento`, type = 'scatter', mode = 'markers',
            marker=list(color="blue")) %>%
      layout(xaxis = x, yaxis = y)
  })
  
  output$plot8 <- renderPlotly({
    x <- list(
      title = "m/s"
    )
    y <- list(
      title = "Liczebność"
    )
    plot_ly(data_all[1:1000,], x =~`14:Meteo_Exterior_Viento`, type = 'histogram') %>%
      layout(xaxis = x, yaxis = y)
  })
  
  output$plot10 <- renderPlotly({
    x <- list(
      title = "Data"
    )
    y <- list(
      title = "Lux"
    )
    plot_ly(data_all, y =~`11:Lighting_Habitacion_Sensor`, name = 'Pokój', type = 'box'
            , line=list(color = 'red'), marker=list(color="red")) %>%
      add_trace(y = ~`10:Lighting_Comedor_Sensor`, name = 'Jadalnia', marker=list(color="blue"), line = list(color = 'blue')) %>%
      layout(xaxis = x, yaxis = y)
  })
  
  output$plot9 <- renderPlotly({
    x <- list(
      title = "Data"
    )
    y <- list(
      title = "Lux"
    )
    plot_ly(data_all[1:1000,], x = ~Data, y =~`11:Lighting_Habitacion_Sensor`, name = 'Pokoj', type = 'scatter', mode = 'markers',
            marker=list(color="red")) %>%
      add_trace(y =~`10:Lighting_Comedor_Sensor`, name = 'Jadalnia', type = 'scatter', mode = 'markers',marker=list(color="blue"))%>%
      layout(xaxis = x, yaxis = y)
  })
  
  data_train <- as.data.frame(data_train)
  odst_co2_j <- (211.2 - 200.9) * 1.5 + 211.2
  data_train_1 <- data_train[data_train[,6] < odst_co2_j,]
  odst_co2_p <- (212.8 - 202.4) * 1.5 + 212.8
  data_train_1 <- data_train_1[data_train_1[,7] < odst_co2_p,]
  
  output$plot11 <- renderPlotly({
    x <- list(
      title = "Data"
    )
    y <- list(
      title = "ppm"
    )
    plot_ly(data_train_1, y =~`7:CO2_Habitacion_Sensor`, name = 'Pokój', type = 'box'
            , line=list(color = 'red'), marker=list(color="red")) %>%
      add_trace(y = ~`6:CO2_Comedor_Sensor`, name = 'Jadalnia', marker=list(color="blue"), line = list(color = 'blue')) %>%
      layout(xaxis = x, yaxis = y)
  })
  
  odst_lux_j <- (26.69 - 11.52) * 1.5 + 26.69
  data_train_1 <- data_train_1[data_train_1[,10] < odst_lux_j,]
  odst_lux_p <- (23.42 - 13.18) * 1.5 + 23.42
  data_train_1 <- data_train_1[data_train_1[,11] < odst_lux_p,]
  
  output$plot12 <- renderPlotly({
    x <- list(
      title = "Data"
    )
    y <- list(
      title = "Lux"
    )
    plot_ly(data_train_1, y =~`11:Lighting_Habitacion_Sensor`, name = 'Pokój', type = 'box'
            , line=list(color = 'red'), marker=list(color="red")) %>%
      add_trace(y = ~`10:Lighting_Comedor_Sensor`, name = 'Jadalnia', marker=list(color="blue"), line = list(color = 'blue')) %>%
      layout(xaxis = x, yaxis = y)
  })
}

shinyApp(ui, server)