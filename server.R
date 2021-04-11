library("shiny")
library("shinydashboard")
library("rAmCharts")
library("data.table")
library("dplyr")
library("lubridate")
library("googlesheets4")
library("tibble")

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
  
  df = eventReactive(input$go, {
    modulo = input$Modulo
    googlesheets4::gs4_auth(cache = ".secrets", email = TRUE)
    z = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1YZc4O22i02Zik3BLwPOwDY8iQoCovrEUgMtyh2rSVfs/edit?ts=606cc31d#gid=642670686")
    #colnames(z) = make.names(colnames(z))
    metas = read.csv("https://raw.githubusercontent.com/ghostdoggie1/Shiny/main/Metas.csv")
    colnames(metas) = c("Nombre del Encuestador", "Punto de vacunación", "Sexo del Encuestado", "Rango de Edad", "Encuestas", "Zona", "Dia", "Meta")
     
    z$`Nombre del Encuestador`[971] = "Lucia Dzib"
    z$`Nombre del Encuestador`[995] = "Faira Gomez"
    z = z[-c(1:9),]
    z = z[, -which(colnames(z) %in% c("...22", "...23", "ffdgfdgf"))]
    z$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?` = ifelse(is.na(z$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?`), "No disponible" , z$`¿Ha recibido algún apoyo de algún tipo a causa del Covid-19?`)
    z$`¿En que consistió el apoyo?`[is.na(z$`¿En que consistió el apoyo?`)] = "No disponible"
    
    data = filter(z, `Punto de vacunación` == modulo)
    
    data = data[-496,]
    data = data %>%
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

    corte_actual = now(tzone = "UTC")

    encuesta_persona_semana = data[which(data$`Marca temporal` <= corte_actual), ] %>%
      select(`Nombre del Encuestador`, `Punto de vacunación`, `Sexo del Encuestado`, `¿Cuál es su Rango de Edad?`) %>%
      mutate(`Rango de Edad` = ifelse(`¿Cuál es su Rango de Edad?` %in% c("65-69 años", "60-64 años"), "60-69 años", "70+")) %>%
      group_by(`Nombre del Encuestador`, `Punto de vacunación`, `Sexo del Encuestado` = as.factor(`Sexo del Encuestado`), `Rango de Edad` = as.factor(`Rango de Edad`), .drop = FALSE) %>%
      summarise(Encuestas = n()) %>%
      arrange(`Nombre del Encuestador`, `Punto de vacunación`, `Sexo del Encuestado`, `Rango de Edad`)
    encuesta_persona_semana$ID = seq(1:nrow(encuesta_persona_semana))

    # meta_semana = metas %>%
    #   filter(Dia <= 19 & `Nombre del Encuestador` != "Por definir" & `Nombre del Encuestador` %in% unique(encuesta_persona_semana$`Nombre del Encuestador`)) %>%
    #   group_by(`Nombre del Encuestador`, `Punto de vacunación`, `Sexo del Encuestado`, `Rango de Edad`) %>%
    #   summarise(Demanda = sum(Demanda))
    # meta_semana$ID = seq(1:nrow(meta_semana))

    # semana = merge(x = encuesta_persona_semana, y = meta_semana, by = "ID") %>%
    #   select(Nombre.del.Encuestador.x, Punto.de.vacunación.x, Sexo.del.Encuestado, Rango.de.Edad, Encuestas, Demanda) %>%
    #   group_by(Nombre.del.Encuestador.x, Punto.de.vacunación.x, Sexo.del.Encuestado, Rango.de.Edad) %>%
    #   summarise(Faltantes = max(0, Demanda - Encuestas))
    semana = head(metas)
    return(semana)
  })
  
  output$Meta = renderDataTable({
    datos = df()
    return(datos)
  })
}