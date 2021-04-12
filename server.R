library("shiny")
library("shinydashboard")
library("data.table")
library("dplyr")
library("lubridate")
library("googlesheets4")
library("tibble")
library("ggplot2")
library("DT")

server = function(input, output, session) {
  
  output$menu = renderMenu({
    
    sidebarMenu(
      menuItem("Avance de encuestas", tabName = "dashboard1", icon = icon("bar-chart-o")),
      menuItem("Opciones", tabName = "opciones", icon = icon("bar-char-o"),
               selectInput(inputId = "Enlace",
                           label = HTML("Seleccione un Enlace:"),
                           choices = c("Seleccione un Enlace")),
               selectInput(inputId = "Modulo", "Seleccione un punto de vacunación:",
                           choices = c("Seleccione un punto de vacunación:"), selected = "Seleccione un punto de vacunación:"),
               actionButton("go", "Actualizar", icon("refresh")))
    )
  })
  
  observe({
    z = read.csv("https://raw.githubusercontent.com/ghostdoggie1/Shiny/main/Enlaces.csv", header = TRUE, encoding = "UTF-8")
    colnames(z) = c("Enlace", "Punto de vacunacion")
    updateSelectInput(session, inputId = "Enlace", label = "Seleccione un Enlace", choices = c("Seleccione un Enlace:", unique(as.character(z$Enlace))))
  })
  
  observe({
    enlace = input$Enlace
    z = read.csv("https://raw.githubusercontent.com/ghostdoggie1/Shiny/main/Enlaces.csv", header = TRUE, encoding = "UTF-8")
    colnames(z) = c("Enlace", "Punto de vacunacion")
    sub = subset(z, Enlace == enlace)
    updateSelectInput(session = session, inputId = "Modulo", label = "Seleccione un punto de vacunación", choices = c("Seleccione un producto:", unique(sub$`Punto de vacunacion`)))
  })
  
  googlesheets4::gs4_auth(cache = ".secrets", email = TRUE)
  z = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1YZc4O22i02Zik3BLwPOwDY8iQoCovrEUgMtyh2rSVfs/edit?ts=606cc31d#gid=642670686")
  # z$`Nombre del Encuestador`[971] = "Lucia Dzib"
  # z$`Nombre del Encuestador`[995] = "Faira Gomez"
  # z$`Nombre del Encuestador`[1991] = "Jorge Barcelo Paredes"
  # z$`Nombre del Encuestador`[2058] = "Russel Acosta"
  z = z[-c(1:9),]
  z = z[, -which(colnames(z) %in% c("...22", "...23", "ffdgfdgf"))]
  z$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?` = ifelse(is.na(z$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?`), "No disponible" , z$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?`)
  z$`¿En que consistió el apoyo?`[is.na(z$`¿En que consistió el apoyo?`)] = "No disponible"
  z = z[-496,]
  
  metas = read.csv("https://raw.githubusercontent.com/ghostdoggie1/Shiny/main/Metas.csv", encoding = "UTF-8")
  colnames(metas) = c("Nombre del Encuestador", "Punto de vacunación", "Sexo del Encuestado", "Rango de Edad", "Encuestas", "Zona", "Dia", "Meta")
  
  data = z %>%
    filter((hour(`Marca temporal`) > 7 & hour(`Marca temporal`) < 21) & !(`Nombre del Encuestador` == "Cinthya Ayala" & `Marca temporal` < as.POSIXct("2021-04-10")))
  data$`Nombre del Encuestador`[data$`Nombre del Encuestador` == "Rafael Polanco"] = "Cinthya Ayala"
  
  condiciones_invalidas = unique(data$`¿Padece alguna de las siguientes condiciones?`[which(stringr::str_detect(data$`¿Padece alguna de las siguientes condiciones?`, "Ninguna"))])[which(stringr::str_length(unique(data$`¿Padece alguna de las siguientes condiciones?`)[stringr::str_detect(unique(data$`¿Padece alguna de las siguientes condiciones?`), "Ninguna")]) != stringr::str_length("Ninguna"))]
  consecuencia_invalidas = unique(data$`¿Considera que el fenómeno de la pandemia del Covid-19 y sus implicaciones le causo alguna consecuencia o impacto negativo en los siguientes aspectos?`[which(stringr::str_detect(data$`¿Considera que el fenómeno de la pandemia del Covid-19 y sus implicaciones le causo alguna consecuencia o impacto negativo en los siguientes aspectos?`, "Ninguna"))])[which(stringr::str_length(unique(data$`¿Considera que el fenómeno de la pandemia del Covid-19 y sus implicaciones le causo alguna consecuencia o impacto negativo en los siguientes aspectos?`)[stringr::str_detect(unique(data$`¿Considera que el fenómeno de la pandemia del Covid-19 y sus implicaciones le causo alguna consecuencia o impacto negativo en los siguientes aspectos?`), "Ninguna")]) != stringr::str_length("Ninguna"))]
  convivencia_invalidas = unique(data$`¿Ha cambiado la forma de convivencia con sus familiares y amistades a causa de las implicaciones de la pandemia? ¿Usa las siguientes medidas al reunirse con ellos?`[which(stringr::str_detect(data$`¿Ha cambiado la forma de convivencia con sus familiares y amistades a causa de las implicaciones de la pandemia? ¿Usa las siguientes medidas al reunirse con ellos?`, "Ninguna"))])[which(stringr::str_length(unique(data$`¿Ha cambiado la forma de convivencia con sus familiares y amistades a causa de las implicaciones de la pandemia? ¿Usa las siguientes medidas al reunirse con ellos?`)[stringr::str_detect(unique(data$`¿Ha cambiado la forma de convivencia con sus familiares y amistades a causa de las implicaciones de la pandemia? ¿Usa las siguientes medidas al reunirse con ellos?`), "Ninguna")]) != stringr::str_length("Ninguna"))]
  apoyo_invalidos = c(unique(data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?`[which(stringr::str_detect(data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?`, "No"))])[which(stringr::str_length(unique(data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?`)[stringr::str_detect(unique(data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?`), "No")]) != stringr::str_length("No"))],
                      unique(data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?`[which(stringr::str_detect(data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?`, "Prefiero no contestar"))])[which(stringr::str_length(unique(data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?`)[stringr::str_detect(unique(data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?`), "Prefiero no contestar")]) != stringr::str_length("Prefiero no contestar"))])
  apoyo_invalidos = apoyo_invalidos[-which(apoyo_invalidos == "No disponible")]
  tipo_apoyo_invalido_si = unique(data$`¿En que consistió el apoyo?`[!data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?` %in% c("No", "Prefiero no contestar")])[which(unique(data$`¿En que consistió el apoyo?`[!data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?` %in% c("No", "Prefiero no contestar")]) %in% c(".", "NA", "Ninguno", "Ningjna"))]
  tipo_apoyo_invalido_no = unique(data$`¿En que consistió el apoyo?`[data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?` %in% c("No", "Prefiero no contestar")])[which(unique(data$`¿En que consistió el apoyo?`[data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?` %in% c("No", "Prefiero no contestar")]) %in% c("Pensión del seguro"))]
  
  data = data[!c(data$`¿Padece alguna de las siguientes condiciones?` %in% condiciones_invalidas | data$`¿Considera que el fenómeno de la pandemia del Covid-19 y sus implicaciones le causo alguna consecuencia o impacto negativo en los siguientes aspectos?` %in% consecuencia_invalidas | data$`¿Ha cambiado la forma de convivencia con sus familiares y amistades a causa de las implicaciones de la pandemia? ¿Usa las siguientes medidas al reunirse con ellos?` %in% convivencia_invalidas  | data$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?` %in% apoyo_invalidos),]
  data_apoyo = data %>%
    filter(!`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?` %in% c("No", "Prefiero no contestar")) %>%
    filter(!`¿En que consistió el apoyo?` %in% tipo_apoyo_invalido_si)
  data_apoyo_no = data %>%
    filter(`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?` %in% c("No", "Prefiero no contestar")) %>%
    filter(!`¿En que consistió el apoyo?` %in% tipo_apoyo_invalido_no)
  
  data = rbind(data_apoyo, data_apoyo_no) %>%
    filter(!(`¿A raíz de los efectos de la pandemia del Covid-19 disminuyeron los ingresos en su hogar?` %in% c("No", "No se, o prefiero no contestar") & `¿Se ha recuperado esa disminución de ingresos que se dio a raíz de la pandemia?` != "No aplica"))
  
  
  df = eventReactive(input$go, {
    modulo = input$Modulo
    
    data = filter(z, `Punto de vacunación` == modulo)
    
    corte_anterior = as.POSIXct(paste0("2021", sep = "-", "04", sep = "-", day(now()), sep = " ", "07:00:00"), tz = "UTC")
    corte_actual = now(tzone = "UTC")
    
    # Encuestas de hoy por persona #
    encuesta_persona_dia = data[which(data$`Marca temporal` >= corte_anterior & data$`Marca temporal` <= corte_actual), ] %>%
      select(`Nombre del Encuestador`, `Punto de vacunación`, `Sexo del Encuestado`, `¿Cuál es su Rango de Edad?`) %>%
      mutate(`Rango de Edad` = ifelse(`¿Cuál es su Rango de Edad?` %in% c("65-69 años", "60-64 años"), "60-69 años", "70+")) %>%
      group_by(`Nombre del Encuestador`, `Punto de vacunación`, `Sexo del Encuestado` = as.factor(`Sexo del Encuestado`), `Rango de Edad` = as.factor(`Rango de Edad`), .drop = FALSE) %>%
      summarise(Encuestas = n()) %>%
      arrange(`Nombre del Encuestador`, `Punto de vacunación`,`Sexo del Encuestado`, `Rango de Edad`)

    meta_dia = metas %>%
      filter(Dia == day(corte_actual) & `Nombre del Encuestador` != "Por definir" & `Punto de vacunación` == modulo) %>%
      group_by(`Nombre del Encuestador`, `Punto de vacunación`, `Sexo del Encuestado`, `Rango de Edad`) %>%
      summarise(Meta = sum(Meta))

    encuesta_persona_dia = merge(x = encuesta_persona_dia, y = meta_dia, by = c("Nombre del Encuestador", "Punto de vacunación", "Sexo del Encuestado", "Rango de Edad")) %>%
      select(`Nombre del Encuestador`, `Punto de vacunación`, `Sexo del Encuestado`, `Rango de Edad`, Encuestas, Meta) %>%
      group_by(`Nombre del Encuestador`, `Punto de vacunación`,`Sexo del Encuestado`, `Rango de Edad`) %>%
      summarise(Encuestas = Encuestas, Meta = Meta, Avance = paste0(round((sum(Encuestas)/sum(Meta)) * 100, 0), "%"))
    encuesta_persona_dia$Avance[which(is.na(encuesta_persona_dia$Avance))] = "0%"
    encuesta_persona_dia$Avance[is.infinite(encuesta_persona_dia$Avance)] = "100%"
    
    # Encuestas hasta ahora #
    encuesta_persona_semana = data[which(data$`Marca temporal` <= corte_actual), ] %>%
      select(`Nombre del Encuestador`, `Punto de vacunación`, `Sexo del Encuestado`, `¿Cuál es su Rango de Edad?`) %>%
      mutate(`Rango de Edad` = ifelse(`¿Cuál es su Rango de Edad?` %in% c("65-69 años", "60-64 años"), "60-69 años", "70+")) %>%
      group_by(`Nombre del Encuestador`, `Punto de vacunación`, `Sexo del Encuestado` = as.factor(`Sexo del Encuestado`), `Rango de Edad` = as.factor(`Rango de Edad`), .drop = FALSE) %>%
      summarise(Encuestas = n()) %>%
      arrange(`Nombre del Encuestador`, `Punto de vacunación`, `Sexo del Encuestado`, `Rango de Edad`)

    # Metas por persona para terminar el viernes #
    meta_semana = metas %>%
      filter(Dia <= 19 & `Nombre del Encuestador` != "Por definir" & `Punto de vacunación` == modulo) %>%
      group_by(`Nombre del Encuestador`, `Punto de vacunación`, `Sexo del Encuestado`, `Rango de Edad`) %>%
      summarise(Meta = sum(Meta))

    # Encuestas faltantes por persona para trabajar hasta el viernes #
    semana = merge(x = encuesta_persona_semana, y = meta_semana, by = c("Nombre del Encuestador", "Punto de vacunación", "Sexo del Encuestado", "Rango de Edad"), all.x = TRUE) %>%
      select(`Nombre del Encuestador` = `Nombre del Encuestador`, `Sexo del Encuestado` = `Sexo del Encuestado`, `Rango de Edad` = `Rango de Edad`, Encuestas, Meta) %>%
      group_by(`Nombre del Encuestador`, `Sexo del Encuestado`, `Rango de Edad`, .drop = FALSE) %>%
      summarise(Encuestas = Encuestas, Meta = Meta, Faltantes = max(0, Meta - Encuestas)) %>%
      na.omit()
    
    # Encuestas realizadas por centro y por caracteristica demografica #
    encuesta_centro_total_socio = data %>%
      select(`Punto de vacunación`, `Sexo del Encuestado`, `¿Cuál es su Rango de Edad?`) %>%
      mutate(`Rango de Edad` = ifelse(`¿Cuál es su Rango de Edad?` %in% c("65-69 años", "60-64 años"), "60-69 años", "70+")) %>%
      group_by(`Punto de vacunación`, `Sexo del Encuestado` = as.factor(`Sexo del Encuestado`), `Rango de Edad` = as.factor(`Rango de Edad`), .drop = FALSE) %>%
      summarise(Encuestas = n()) %>%
      arrange(`Punto de vacunación`,`Sexo del Encuestado`, `Rango de Edad`)

    # Metas por centro y por caracteristica demografica #
    meta_total_socio = metas %>%
      filter(`Punto de vacunación` == modulo) %>%
      group_by(`Punto de vacunación`, `Sexo del Encuestado`, `Rango de Edad`) %>%
      summarise(Meta = sum(Meta))

    encuesta_centro_total = merge(x = encuesta_centro_total_socio, y = meta_total_socio, by = c("Punto de vacunación", "Sexo del Encuestado", "Rango de Edad")) %>%
      select(`Punto de vacunación` = `Punto de vacunación`, `Sexo del Encuestado` = `Sexo del Encuestado`, `Rango de Edad` = `Rango de Edad`, Encuestas, Meta) %>%
      group_by(`Punto de vacunación`, `Sexo del Encuestado`, `Rango de Edad`) %>%
      summarise(Encuestas = min(Encuestas, Meta)) %>%
      group_by(`Punto de vacunación`) %>%
      summarise(Encuestas = sum(Encuestas)) %>%
      na.omit()
    encuesta_centro_total$ID = seq(1:nrow(encuesta_centro_total))
    
    meta_centro_total = metas %>%
      filter(`Punto de vacunación` == modulo) %>%
      group_by(`Punto de vacunación`) %>%
      summarise(Meta = sum(Meta))
    meta_centro_total$ID = seq(1:nrow(meta_centro_total))
    
    encuesta_centro_total = merge(x = encuesta_centro_total, y = meta_centro_total, by = "Punto de vacunación") %>%
      select(`Punto de vacunación` = `Punto de vacunación`, Encuestas, Meta) %>%
      group_by(`Punto de vacunación`) %>%
      summarise(Avance = sum(Encuestas)/sum(Meta))
    
    bd = list(encuesta_centro_total, semana, encuesta_persona_dia)
    return(bd)
  })
  
  output$General = renderDataTable({
    datos = df()[[2]]
    return(datos)
  }, extensions = "Buttons", options = list(scrollX = TRUE,
                    autoWidth = TRUE,
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
  
  output$General_chart = renderPlot({
    ggplot(df()[[1]],
           aes(ymax = Avance, ymin = 0, xmax = 2, xmin = 1)) +
      geom_rect(aes(ymax = 1, ymin = 0, xmax = 2, xmin = 1), fill = "#CCCCCC", alpha = 0.4, color = "#001a23") +
      geom_rect(fill = "#001a23") + 
      coord_polar(theta = "y", start = -pi/2) + xlim(c(0, 2)) + ylim(c(0, 2)) +
      geom_text(aes(x = 0, y = 0, label = paste0(round(Avance * 100, 0), "%")), family = "Barlow Black", size = 10) +
      geom_text(aes(x = 0.5, y = 1.5, label = `Punto de vacunación`), family = "Barlow SemiBold", size = 7) +
      theme_void() +
      theme(strip.background = element_blank(),
            strip.text.x = element_blank()) +
      guides(fill = FALSE) +
      guides(colour = FALSE)
  })
  
  output$Meta_dia = renderDataTable({
    datos = df()[[3]]
    return(datos)
  }, extensions = "Buttons", options = list(scrollX = TRUE,
                                            autoWidth = TRUE,
                                            dom = 'Bfrtip',
                                            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ))
}