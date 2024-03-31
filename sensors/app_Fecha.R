library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

ui <- fluidPage(
  titlePanel("Visualización de datos del sensor: Promedios por Fecha"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Seleccionar archivo Excel", accept = c(".xlsx")),
      uiOutput("selectUI"),  # UI para selección de ejes
      uiOutput("sliderUI"),  # UI para sliderInput dinámico
      downloadButton("downloadData", "Descargar Datos")  # Botón de descarga
    ),
    mainPanel(
      plotlyOutput("linePlot")  # Cambiado a plotlyOutput para la visualización de gráficos
    )
  ),
  # Agregar nota al pie de página
  tagList(
    tags$footer(
      style = "position: fixed; bottom: 0; width: 100%; background-color: #f5f5f5; text-align: center; padding: 10px;",
      "© 2024 Sarah Richards."
    )
  )
)

server <- function(input, output, session) {
  
  datos_reactivos <- reactive({
    req(input$fileUpload)
    datos <- read_excel(input$fileUpload$datapath)
    datos$Fecha <- as.Date(datos$Fecha) # Asegúrate de convertir la columna Fecha al formato de fecha
    return(datos)
  })
  
  output$sliderUI <- renderUI({
    req(datos_reactivos())
    df <- datos_reactivos()
    fecha_min <- min(df$Fecha, na.rm = TRUE)
    fecha_max <- max(df$Fecha, na.rm = TRUE)
    
    sliderInput("fechaRango", "Rango de Fecha:", 
                min = fecha_min, max = fecha_max, 
                value = c(fecha_min, fecha_max),
                timeFormat = "%d/%m/%Y")
  })
  
  output$selectUI <- renderUI({
    req(datos_reactivos())
    df <- datos_reactivos()
    columnas_x <- c("Fecha")
    columnas_y <- c("Temp_C_(Bare_soil_temperatura)", "Temp_C_(Cafe_temperatura)", "Water_Content_m3/m3_(Bare_soil_contenido_de_agua)", "Water_Content_m3/m3_(Cafe_contenido_de_agua)", "Water_Content_m3/m3_(Covercrop_contenido_de_agua)")
    
    list(
      selectInput("ejeX", "Elegir eje X:", choices = columnas_x, selected = columnas_x[1]),
      checkboxGroupInput("ejeY", "Elegir eje(s) Y:", choices = columnas_y, selected = columnas_y[1])
    )
  })
  
  output$linePlot <- renderPlotly({
    req(datos_reactivos(), input$ejeY, input$fechaRango)
    df <- datos_reactivos()
    df_filtrado <- df %>% filter(Fecha >= input$fechaRango[1] & Fecha <= input$fechaRango[2])
    df_long <- pivot_longer(df_filtrado, cols = input$ejeY, names_to = "variable", values_to = "valor")
    df_promedio <- df_long %>%
      group_by_at(vars(input$ejeX, "variable")) %>%
      summarise(valor = mean(valor, na.rm = TRUE)) %>%
      ungroup()
    df_promedio[[input$ejeX]] <- as.Date(df_promedio[[input$ejeX]])
    
    p <- ggplot(df_promedio, aes_string(x = input$ejeX, y = 'valor', color = 'variable')) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(x = input$ejeX, y = "Promedio de las variables", title = "Promedio de valores por variable") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
      scale_x_date(date_labels = "%d/%m/%Y")
    
    ggplotly(p)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("datos-filtrados-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(datos_reactivos(), input$ejeY, input$fechaRango)
      df <- datos_reactivos()
      df_filtrado <- df %>%
        filter(Fecha >= input$fechaRango[1] & Fecha <= input$fechaRango[2]) %>%
        select(c(input$ejeX, input$ejeY))
      write.csv(df_filtrado, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)