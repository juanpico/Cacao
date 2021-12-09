library(shiny)
library(plotly)
library(shinydashboard)
library(DT)
source('calcular_productividad.R')
# Define UI for application that draws a histogram
ui <- dashboardPage(skin = 'green',
    dashboardHeader(title = "Predicción de productividad del cacao",
                    titleWidth = 350),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Datos meteorológicos", tabName = "clima", icon = icon("cloud-sun-rain")),
            menuItem("Sistema de cultivo", tabName = "cultivo", icon = icon("seedling")),
            menuItem("Productividad", tabName = "productividad", icon = icon("chart-bar"))
        )
    ),
    
    dashboardBody(
        
        tabItems(
            
            # Tab de datos meteorológicos
            tabItem(tabName = "clima",
                    
                    fluidPage(
                        titlePanel("Datos meteorológicos de la región"),
                        wellPanel(
                            fluidRow(
                                column(6,
                                        fileInput("inputClima","Cargar archivo .csv con los datos del clima:",
                                           accept = c("text/csv","text/comma-separated-values,
                                           text/plain",".csv") )
                                ),
                                column(6,
                                selectInput("anio", label = "Elegir el año para la predicción", 
                                            choices = 2020:2010, 
                                            selected = 1)
                                )
                            ),
                            helpText("Nota: Considere que para el año de predicción elegido,
                                         se debe tener información meteorológica desde octubre del año anterior.")
                        ),

                        fluidRow(
                            column(12,
                                    DT::dataTableOutput("tabla_clima"))
                        )
                        
                    )
            )
            
            
            ,
            # Tabla de parámetros de los cultivos
            tabItem(tabName = "cultivo",
                    fluidPage(
                        titlePanel("Caracteristicas de los cultivos"),
                        wellPanel(
                            fluidRow(
                                column(6,
                                       fileInput("inputCultivos","Cargar archivo .csv con las características de los cultivos:",
                                                 accept = c("text/csv","text/comma-separated-values,
                                           text/plain",".csv") )
                                )
                            )
                        ),
                        
                        fluidRow(
                            column(12,
                                   DT::dataTableOutput("tabla_cultivos"))
                        )
                        
                    ) 
                    
            ),
            tabItem(
                tabName = "productividad",
                box(title="Productividad teórica y ajustada para cada cultivo",
                    plotlyOutput("boxplot"), width = 20
                ),
                fluidPage(
                    wellPanel(
                        fluidRow(
                            column(12,
                                   DT::dataTableOutput("tabla_prods"))
                        )
                    )
                    
                )
            )
            #cierra el objeto tab items  
        )
        #cierra el dashboard body  
    )
    
) 

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Reactive function de Tabla del clima
    tablaClima <- eventReactive(input$inputClima, {
        #Importar el archivo de datos del clima
        inFile0 <- input$inputClima
        if(is.null(inFile0)) {return(NULL)}
        dataFile0<- read.csv(inFile0$datapath)[-1]
        return(dataFile0)
    })
    
    # Reactive function de Tabla del clima
    tablaCultivos <- eventReactive(input$inputCultivos, {
        #Importar el archivo de datos de los cultivos
        inFile0 <- input$inputCultivos
        if(is.null(inFile0)) {return(NULL)}
        dataFile0<- read.csv(inFile0$datapath, sep=';')
        return(dataFile0)
    })
    
    # Renderizar tabla del clima
    output$tabla_clima <- DT::renderDataTable(DT::datatable(tablaClima(),
                                                            options = list(scrollX = TRUE,
                                                                           pageLength = 8)
                                                            ) %>% formatRound(2:5, digits=3))
    
    # Renderizar tabla de cultivos
    output$tabla_cultivos <- DT::renderDataTable(DT::datatable(tablaCultivos(),
                                                            options = list(scrollX = TRUE,
                                                                           pageLength = 8)
                                                ))
    
    # Realizar predicción con base en clima y cultivos
    resultado_prod <- eventReactive(c(tablaClima(), tablaCultivos(),input$anio),{
        if(is.null(tablaClima())==FALSE & is.null(tablaCultivos())==FALSE){
            resultado <- calcular_productividad(tablaClima(), tablaCultivos(), input$anio)
        }else{return(NULL)}
        return(resultado)
    })
    
    # Renderizar tabla de productividad
    output$tabla_prods <- DT::renderDataTable(DT::datatable({
        resultado_prod()[[1]][,c(1, 2, 3, 6)]
    }) %>% formatRound(1:4, digits=2))
    
    # Renderizar boxplots de la productividad
    output$boxplot <- renderPlotly({
        datos <- resultado_prod()[[2]]
        
        plot_ly(datos, x = ~Cultivo, y = ~semillas, color = ~tipo_prod, 
                type = 'box', colors = "Dark2") %>% 
            layout(xaxis = list(title=""),
                   yaxis = list(title = "Productividad [kg/hectarea]"))
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
