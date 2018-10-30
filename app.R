#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sunburstR)



makedataSun = function(database){ 
  dataSun = paste0(database$Perfil.Profesional,
                   "-",database$Grado.Academico,
                   "-",database$Titulo,
                   "-",database$Nom2)
  
  dataSun = aggregate(dataSun, list(dataSun), length)
  
  return(dataSun)
  
}


makedataSun2 = function(database){ 
  dataSun = paste0(paste(database$Nombres,"\n",
                         database$Apellidos))

  return(dataSun)
  
}

makedataSun3 = function(database){ 
  dataSun = paste0(database$Titulo,
    "<br>",database$Perfil.Profesional,
    "<br>",  database$E.mail)
  
  dataSun = aggregate(dataSun, list(dataSun), length)
  dataSun = dataSun[order(dataSun$x), T]
  return(dataSun)
  
}


pickCol = function(col, L ){
  col = sample(col, size = L, T)
  return(col)}


# Define UI for application that draws a histogram
ui <-fluidPage(
  tags$link(rel = "stylesheet", 
            type = "text/css",
            href = "custom.css"),
 tabsetPanel(type = "pills",
   tabPanel("Directorio", 
            fluidRow(
              column(3,
                     HTML("<br>","<br>"),
                     h4("Instrucciones:"),
                     h4("Desliza el mouse sobre la figura, una tarjeta dinamica aparecera aqui 
                        abajo cuando selecciones un borde"),
                     br(),
                     HTML('<hr color="#d95f0e">'),
                     h2(textOutput("selection" )),
                     htmlOutput("selection2"),
                     div(img(src = "MarcaPagWeb.png", height = 200, width = 300), style="text-align: center;"),
              HTML('<hr color="#d95f0e">')),
              column(9,
            sunburstOutput("sunburst",
                           width = "500px",
                           height = "500px"))),
            br(),
            p("Estos analisis provienen de una encuesta realizada a Becarios Retornados,
              si quieres saber más detalles sobre la metodología contactanos a:"),
            tags$a(href="asoabrec@gmail.com", "asoabrec@gmail.com"), p("o al Facebook:"), 
            tags$a(href="www.facebook.com/ABREC.BecariosRetornadosEc", "www.facebook.com/ABREC.BecariosRetornadosEc"),
            h5("Eres becario y no has llenado la encuesta?, hazlo en el siguiente link:"),
            tags$a(href="https://goo.gl/forms/UgYeaxJCXCdnmcHD2", "EncuestaBecariosRetornadosEC!")
             
            ),
 
    tabPanel("Indicadores",
            
 # Application title
   h2("Indicadores del desempeño del programa nacional de Becarios SENESCYT al exterior"),
   br(),br(),
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     
      sidebarPanel(
#Indicadores######
      h3("Indicadores disponibles:"), p(), 
      h4("Trabajo:"),
      p("Este indicador representa la insercion laboral del 
         becario una vez regresado al pais. 
         Es un simple estimador dado por los meses trabajando en relacion a los meses retornados
         1 = Total insercion laboral | 0 = Inserción laboral nula (i.e. desempleado desde el momento de arrivo al pais)"),
      h4("Desempleo:"),
      p("Este indicador representa la facilidad que tuvo el 
         becario para encontrar trabajo una vez regresado al pais. 
         Es un simple estimador dado por los meses buscando empleo en relacion a los meses retornados  
         1 = Todavia no consigue trabajo  | 0 = Consiguio trabajo inmediatamente "),
      h4("Transferencia del Conocimiento"),
      p("Este indicador representa la Transferencia efectiva de Conocimiento por parte del Becario Retornado.         
Es un simple estimador dado por los meses en los cuales realizo transferencia de conocimiento (remunerada o no) en relacion a los meses retornados 

         1 = Realizo una plena transferencia de conocimiento  | 0 = La Transferencia de Conocimiento fue nula"),
      br(),
selectInput("input",
            "Selecciona un Indicador",
            choices = c("Indicador Trabajo", 
                        "Indicador Desempleo", 
                        "Indicador Transferencia Conocimiento")),
br(),
p("Estos analisis provienen de una encuesta realizada a Becarios Retornados,
  si quieres saber más detalles sobre la metodología contactanos a:"),
tags$a(href="asoabrec@gmail.com", "asoabrec@gmail.com"), p("o al Facebook:"), 
tags$a(href="www.facebook.com/ABREC.BecariosRetornadosEc", "www.facebook.com/ABREC.BecariosRetornadosEc"),
h5("Eres becario y no has llenado la encuesta?, hazlo en el siguiente link:"),
tags$a(href="https://goo.gl/forms/UgYeaxJCXCdnmcHD2", "EncuestaBecariosRetornadosEC!"),
      HTML('<hr color="#d95f0e">'),
         div(img(src = "MarcaPagWeb.png", height = 200, width = 300), style="text-align: center;")

      ),
      
     
      
      # Show a plot of the generated distribution
      mainPanel(
     
                   fluidPage(
         plotOutput("distPlot"),
         HTML('<hr color="#d95f0e">'),
         p("Lineas marcan la tendencia de cada indicador en relación a la convocatoria que pertenecen 
            los becarios retornados. Lineas solidas representan tendencias significativas, mientras que las entrecortadas 
            representan tendencias cuasi-significativas. A priori se puede concluir que los becarios de convocatorias más recientes
            (i.e. que estan empezando a retornar) tienen relativamente más complicaciones para insertarse laboralmente y pasan más tiempo buscando empleo"),
         p("La similitud entre las tendencias de Trabajo y Transferencia del Conocimiento implica que la falla en inserción laboral repercute en la trasnferencia de conocimiento por parte del becario. 
            Sin embargo, que esta relación no sea del todo significativa tambien sugiere que la Transferencia de Conocimiento no esta del todo ligada al Empleo.
            Es decir, que un becario tenga trabajo no necesariamente significa que realize Transferencia de Conocimiento"),
         
         h3("Desliza abajo para más información"),
         HTML('<hr color="#d95f0e">'),
         plotOutput( "plot2"),
         HTML('<hr color="#d95f0e">'),
         p("Este grafico muestra la Situación Laboral de los Becarios Retornados basados en el año de convocatoria
           Aqui es observable como la tasa de desempleo aumenta para los becarios que han retornado más recientemente. 
           Sugiriendo una saturación del mercado laboral local para los mismos, es importante notar que hay un 
           porcentaje de becarios que han optado por el Subempleo")
         
#End####       
      

      )
   )
))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  EmpleoC <- reactive({ 
   read.csv("EmpleoC.csv", header = T)
    })
  
  PProf = reactive({ 
    
    database = xlsx::read.xlsx("PPROF.xlsx", sheetIndex = 1)
    database$Nom2 = sapply(1:length(database$E.mail),
                           function(x) paste0(database$Nombres[x],".", database$Apellidos[x]))
    database
  })
  
   output$distPlot <- renderPlot({
     # load data
    
      # make plot 
     if (input$input == "Indicador Trabajo") {
       par(las = 1)
       plot(EmpleoC()$TrabajoInd~EmpleoC()$Ano.de.la.Convocatoria, 
            xlab = "Año de la Convocatoria", 
            ylab = " Meses Trabajando / Meses Retornado ",
            pch  = 16, 
            col = "#d95f0e", 
            cex = 1.5)
       
       lm = lm(EmpleoC()$TrabajoInd~EmpleoC()$Ano.de.la.Convocatoria)
       abline(lm, col = "red", lwd = 2)
       legend("bottomleft", "Rsq=0.13; p>0.001; N = 116")
     }
     
     if (input$input == "Indicador Desempleo") { 
       par(las = 1)
       plot(EmpleoC()$ParoInd~EmpleoC()$Ano.de.la.Convocatoria, 
            xlab = "Año de la Convocatoria", 
            ylab = "Meses Buscando Empleo / Meses Retornado ",
            pch  = 16, 
            col = "#d95f0e", 
            cex = 1.5)
       
       lm = lm(EmpleoC()$ParoInd~EmpleoC()$Ano.de.la.Convocatoria)
       abline(lm, col = "red", lwd = 2)
       legend("topleft", "Rsq=0.04; p=0.02; N = 116")
       
       }
     if (input$input == "Indicador Transferencia Conocimiento") { 
       par(las = 1)
       plot(EmpleoC()$TransferenInd~EmpleoC()$Ano.de.la.Convocatoria, 
            xlab = "Año de la Convocatoria", 
            ylab = " Meses Transferencia de Conocimiento / Meses Retornado ",
            pch  = 16, 
            col = "#d95f0e", 
            cex = 1.5)
       
       lm = lm(EmpleoC()$TransferenInd~EmpleoC()$Ano.de.la.Convocatoria)
       abline(lm, col = "red", lwd = 2,lty = 2)
       legend("bottomleft", "Rsq=0.02; p=0.06; N = 116")
       
     }
    
   })
   
   output$plot2 <- renderPlot({
     
     cSum = colSums(table(EmpleoC()$Situacion.Laboral, EmpleoC()$Ano.de.la.Convocatoria))
     
     Std = t(matrix(rep(cSum,3),
                    ncol = 3,
                    nrow = 8))
     par(las = 1, mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)           
     barplot(table(EmpleoC()$Situacion.Laboral, EmpleoC()$Ano.de.la.Convocatoria) / Std,
             col = c('#1b9e77','#d95f02','#7570b3'),
             xlab = "Año de convocatoria",
             ylab = "Situación Laboral (%)",
             main = "Tendencias de Empleo Becarios")
     legend("topright", c("Desempleado", "Empleado", "Subempleado"), 
            fill = c('#1b9e77','#d95f02','#7570b3'),
            inset=c(-0.18,0))
   })
   
   
   output$sunburst <- renderSunburst({
     #invalidateLater(1000, session)
     col = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6')
     
     add_shiny(sunburst(makedataSun(PProf()),
                        legend = FALSE,
                        count = T,breadcrumb = c(1,1,1,1), 
                        colors = pickCol(col, 
                                         prod(dim(PProf())))
))
   })
   
   selection <- reactive({
     
     name = unlist(stringr::str_split(input$sunburst_mouseover, "\n"))
     name2 = name[length(name)]
     sel = which(!is.na(match(PProf()$Nom2,name2)))
     dSun = PProf()[sel,]
     dSun = makedataSun2(dSun)
     
   })
   
   selection2 <- reactive({
     
     name = unlist(stringr::str_split(input$sunburst_mouseover, "\n"))
     name2 = name[length(name)]
     sel = which(!is.na(match(PProf()$Nom2,name2)))
     dSun = PProf()[sel,]
     dSun = makedataSun3(dSun)$Group.1
     
   })
   
   
   output$selection <- renderText(selection())
   output$selection2 <- renderUI(HTML(selection2()))
 
}

# Run the application 
shinyApp(ui = ui, server = server)

