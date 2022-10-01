rm(list = ls())
packages <- c("dplyr", "ggplot2", "plotly", "hrbrthemes", "png", "patchwork", "shiny", "bslib")

# Si no existen en el ambiente en donde se esta ejecutando este script, los instalo
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}


# Importo los paquetes
library(shiny)
library(bslib)

library(dplyr) 
require(ggplot2)
library(plotly)
library(hrbrthemes)

####
# Leo el archivo
productos_lacteos_solidos <- read.csv('./datos/productos-lacteos-solidos-1989-2016.csv')
datos_agrupados <- productos_lacteos_solidos %>% 
  select(c(3,4,6,9))

# Agrupo las producciones de los productos por Anyo
df<-data.frame(datos_agrupados)
data_por_anyo <- aggregate(Cantidad ~ Anyo+Producto,data=df,FUN=sum)
#Elimino ultimo anyo
data_por_anyo <- data_por_anyo %>% filter(Anyo < max(Anyo))

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel('Produccion anual de productos lacteos'),
  sidebarLayout(
    sidebarPanel(
        radioButtons(
          inputId = "chartType",
          label = "Select Chart Type",
          choices = c("Line", "Columns")
        ),
        selectInput(inputId = "prods", label = "Choose a product",
                    unique(data_por_anyo$Producto),
                    selected = NULL,
                    multiple = TRUE,
                    selectize = TRUE, width = NULL, size = NULL),
        sliderInput( inputId = "anyos",
                    label = "Choose a Range Year",
                    value=range(data_por_anyo$Anyo),
                    min = min(data_por_anyo$Anyo), max = max(data_por_anyo$Anyo)),
    ),
    mainPanel(
        plotOutput("productos")
    ),
  )
)

server <- function(input, output) {
  output$productos <- renderPlot({
    
    data_to_plot <- data_por_anyo %>%
      filter(Producto %in% input$prods) %>% 
      filter(Anyo >= input$anyos[1] & Anyo <= input$anyos[2]) %>%
      ggplot( aes(x=Anyo, y=Cantidad, group=Producto, color=Producto))
      # geom_line() 
    
      if(input$chartType == "Columns") {
        data_to_plot + geom_col()      
      } else {
        data_to_plot + geom_line()
      }
  })
}

shinyApp(ui = ui, server = server)