#EQUIPO 22
#Flor de María Medina García
#José Andrés Echeveste Vázquez

library(shiny)
library(shinydashboard)
library(shinyjs)

shinyUI(fluidPage(
    
    useShinyjs(),
    
    dashboardPage(
        
        skin = "yellow",
        
        dashboardHeader(title = "Vacunación COVID-19",
                        titleWidth = 275),
        
        dashboardSidebar(
            
            width = 275,
            
            sidebarMenu(
                
                id = "Sidebar",
                
                menuItem("Data Table de Vacunación", tabName = "data_table", icon = icon("table")),
                menuItem("Paises que adquieren más vacunas", tabName = "grafica", icon = icon("area-chart")),
                menuItem("Paises que han aplicado más vacunas", tabName = "grafica1", icon = icon("area-chart")),
                menuItem("Marca más distribuida por continente", tabName = "grafica2", icon = icon("area-chart")),
                menuItem("Correlación vacun/contag (USA)", tabName = NULL, icon = icon("area-chart"), 
                         menuSubItem("Plot", tabName = "grafica3"),
                         menuSubItem("Ajuste", tabName = "inf"),
                         menuSubItem("Resumen", tabName = "inf1")
                         ),
                menuItem("Series de tiempo", tabName = "grafica4", icon = icon("area-chart"))
            )
            
        ),
        
        dashboardBody(
            
            tabItems(
                
                
                #Data Table
                tabItem(tabName = "data_table",
                        fluidRow(        
                            titlePanel(h3("Vacunación")),
                            dataTableOutput ("data_table")
                        )
                ), 
                
                # Países que han adquirido màs vacunas
                tabItem(tabName = "grafica",
                        
                        fluidRow(
                            sidebarPanel(
                                selectInput("grafcont", "Selecciona el Continente", 
                                            c("Africa", "América", "Asia", "Europa"))
                            ),
                            
                            mainPanel(
                                plotOutput("plot")
                            )
                        
                        )
                ),
                
                # Países que aplican más vacunas al día
                tabItem(tabName = "grafica1", 
                        fluidRow(
                            titlePanel("Países que han aplicado más vacunas en un día"), 
                            box(plotOutput("plot1"))
                        )
                ),
                
                
                # Marca de vacunas más distribuida por continente
                tabItem(tabName = "grafica2",
                        
                        fluidRow(
                            sidebarPanel(
                                selectInput("marcont", "Selecciona el Continente", 
                                            c("Africa", "América", "Asia", "Europa"))
                            ),
                            
                            mainPanel(
                                plotOutput("plot2")
                            )
                            
                        )
                ),
                
                tabItem(tabName = "grafica3",
                        fluidRow(
                            titlePanel("Correlación vacunados y contagiados en USA"), 
                            box(plotOutput("plot3"))
                        )
                ),
                
                tabItem(tabName = "inf",
                        fluidRow(        
                            titlePanel(h3("Ajuste")),
                            verbatimTextOutput("ajuste")
                        )
                ),
               
                tabItem(tabName = "inf1",
                        fluidRow(        
                            titlePanel(h3("Resumen")),
                            verbatimTextOutput("summary")
                        )
                ),
                
                tabItem(tabName = "grafica4",
                        fluidRow(
                            titlePanel("TS para dos paises de cada contiente"),
                            sidebarPanel(
                                selectInput("tscont", "Selecciona el Continente", 
                                            c("America", "Asia", "Europa")),
                                uiOutput("var")
                            ),
                            
                            mainPanel(
                                plotOutput("plot4")
                            )
                            
                        )
                )
            )
        )
    )
))
