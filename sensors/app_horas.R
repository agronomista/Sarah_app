library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(lubridate)

ui <- fluidPage(
  titlePanel("Visualización de datos del sensor por Hora"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fileUpload", "Seleccionar archivo Excel", accept = c(".xlsx")),
      uiOutput("selectUI"),
      uiOutput("sliderUI"),
      uiOutput("fechaRangoUI"),  # UI para el rango de fechas
      downloadButton("downloadData", "Descargar Datos")
    ),
    mainPanel(
      plotlyOutput("linePlot")
    )
  ),
  tagList(  # Aquí agregas el footer
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
    # Convertir "Inicio_hora" a tipo POSIXct y "Fecha" a tipo Date
    datos$Inicio_hora <- ymd_hms(datos$Inicio_hora)
    datos$Fecha <- as.Date(datos$Fecha)  # Asegúrate de que esta línea sea correcta para tu formato de fecha
    return(datos)
  })
  
  output$sliderUI <- renderUI({
    req(datos_reactivos())
    df <- datos_reactivos()
    
    Inicio_hora_min <- min(df$Inicio_hora, na.rm = TRUE)
    Inicio_hora_max <- max(df$Inicio_hora, na.rm = TRUE)
    Inicio_hora_max <- as.POSIXct(format(Inicio_hora_max, "%Y-%m-%d 23:59:59"))
    
    sliderInput("Inicio_horaRango", "Rango de Inicio_hora:", 
                min = Inicio_hora_min, max = Inicio_hora_max, 
                value = c(Inicio_hora_min, Inicio_hora_max),
                timeFormat = "%H:%M")
  })
  
  output$fechaRangoUI <- renderUI({
    req(datos_reactivos())
    df <- datos_reactivos()
    fecha_min <- min(df$Fecha, na.rm = TRUE)
    fecha_max <- max(df$Fecha, na.rm = TRUE)
    
    sliderInput("fechaRango", "Rango de Fecha:",
                min = fecha_min, max = fecha_max,
                value = c(fecha_min, fecha_max),
                timeFormat = "%Y-%m-%d")
  })
  
  output$selectUI <- renderUI({
    req(datos_reactivos())
    df <- datos_reactivos()
    columnas_x <- c("Inicio_hora")
    columnas_y <- c("Temp_C_(Bare_soil_temperatura)", "Temp_C_(Cafe_temperatura)", "Water_Content_m3/m3_(Bare_soil_contenido_de_agua)", "Water_Content_m3/m3_(Cafe_contenido_de_agua)", "Water_Content_m3/m3_(Covercrop_contenido_de_agua)")
    
    list(
      selectInput("ejeX", "Elegir eje X:", choices = columnas_x, selected = columnas_x[1]),
      checkboxGroupInput("ejeY", "Elegir eje(s) Y:", choices = columnas_y, selected = columnas_y[1])
    )
  })
  
  output$linePlot <- renderPlotly({
    req(datos_reactivos(), input$ejeY, input$Inicio_horaRango, input$fechaRango)
    df <- datos_reactivos()
    
    df_filtrado <- df %>%
      filter(Inicio_hora >= input$Inicio_horaRango[1] & Inicio_hora <= input$Inicio_horaRango[2],
             Fecha >= input$fechaRango[1] & Fecha <= input$fechaRango[2])
    
    df_long <- pivot_longer(df_filtrado, cols = input$ejeY, names_to = "variable", values_to = "valor")
    
    df_promedio <- df_long %>%
      group_by_at(vars(input$ejeX, "variable")) %>%
      summarise(valor = mean(valor, na.rm = TRUE)) %>%
      ungroup()
    
    p <- ggplot(df_promedio, aes_string(x = input$ejeX, y = 'valor', color = 'variable')) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(x = input$ejeX, y = "Promedio de las variables", title = "Promedio de valores por variable") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
      scale_x_datetime(date_labels = "%H:%M")
    
    ggplotly(p)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("datos-filtrados-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(datos_reactivos(), input$ejeY, input$Inicio_horaRango, input$fechaRango)
      df <- datos_reactivos()
      
      df_filtrado <- df %>%
        filter(Inicio_hora >= input$Inicio_horaRango[1] & Inicio_hora <= input$Inicio_horaRango[2],
               Fecha >= input$fechaRango[1] & Fecha <= input$fechaRango[2]) %>%
        select(c(input$ejeX, input$ejeY))
      
      write.csv(df_filtrado, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)
