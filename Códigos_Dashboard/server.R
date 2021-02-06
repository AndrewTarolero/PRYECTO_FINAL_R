#EQUIPO 22
#Flor de María Medina García
#José Andrés Echeveste Vázquez

library(dplyr)
library(ggplot2)
library(readr)
library(TSA)
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinyjs)


vac <- read.csv("https://raw.githubusercontent.com/AndrewTarolero/PRYECTO_FINAL_R/master/country_vaccinations_1.csv")
names(vac)

vac1 <- select(vac, country, date, total_vaccinations, people_vaccinated, people_fully_vaccinated, daily_vaccinations, vaccines)

v <- select(vac, country, continent, total_vaccinations)

afr <- which(v$continent == 'Africa')
vafr <- v[afr, ]
va.af <- as.data.frame(group_by(vafr, country)%>%
                           summarise(Vacunas = max(total_vaccinations, na.rm = TRUE)))
ame <- which(v$continent == 'America')  
vame <- v[ame, ]                       
va.am <- as.data.frame(group_by(vame, country)%>%
                           summarise(Vacunas = max(total_vaccinations, na.rm = TRUE)))

asi <- which(v$continent == 'Asia') 
vasi <- v[asi, ]                    
va.as <- as.data.frame(group_by(vasi, country)%>%
                           summarise(Vacunas = max(total_vaccinations, na.rm = TRUE)))
eur <- which(v$continent == 'Europe')
veur <- v[eur, ]                      
va.eu <- as.data.frame(group_by(veur, country)%>%
                           summarise(Vacunas = max(total_vaccinations, na.rm = TRUE)))

vd <- select(vac, country, continent, daily_vaccinations)
va.di <- as.data.frame(group_by(vd, country)%>%
                           summarise(Vac.di = max(daily_vaccinations, na.rm = TRUE)))

mar <- select(vac, country, continent, vaccines) 

africa <- which(mar$continent == 'Africa')
mafr <- unique(mar[africa, ])              
marafr <- as.data.frame(mafr%>%
    group_by(vaccines)%>%
    tally())

america <- which(mar$continent == 'America')
mame <- unique(mar[america, ])               
marame <- as.data.frame(mame%>%
    group_by(vaccines)%>%
    tally())

asia <- which(mar$continent == 'Asia')
masi <- unique(mar[asia, ])            
marasi <- as.data.frame(masi%>%
    group_by(vaccines)%>%
    tally())

europa <- which(mar$continent == 'Europe') 
meur <- unique(mar[europa, ])              
mareur <- as.data.frame(meur%>%
    group_by(vaccines)%>%
    tally())

cvv<- read.csv("https://raw.githubusercontent.com/AndrewTarolero/PRYECTO_FINAL_R/master/cases_vs_vaccined.csv")

attach(cvv)
cvv<-na.omit(cvv)

ts.ame<-read.csv("https://raw.githubusercontent.com/AndrewTarolero/PRYECTO_FINAL_R/master/TS_AME.csv", header = TRUE)
ts.ame<-mutate(ts.ame, date = as.Date(date, "%d/%m/%Y")) 

ts.asia<-read.csv("https://raw.githubusercontent.com/AndrewTarolero/PRYECTO_FINAL_R/master/TS_ASIA.csv", header = TRUE)
ts.asia<-mutate(ts.asia, date = as.Date(date, "%d/%m/%Y"))

ts.eur<-read.csv("https://raw.githubusercontent.com/AndrewTarolero/PRYECTO_FINAL_R/master/TS_EUR.csv", header = TRUE)
ts.eur<-mutate(ts.eur, date = as.Date(date, "%d/%m/%Y"))

ts.ame <- rename(ts.ame, USA = daily_vaccinations_EUA, Mexico = daily_vaccinations_MEX)
ts.asia <- rename(ts.asia, China = daily_vaccinations_CHI, Israel = daily_vaccinations_ISR)
ts.eur <- rename(ts.eur, Inglaterra = daily_vaccinations_ENG, Alemania = daily_vaccinations_GER)

shinyServer(function(input, output) {
    
    #Data Table
    output$data_table <- renderDataTable( {vac1}, 
                                          options = list(aLengthMenu = c(5,25,50),
                                                         iDisplayLength = 10)
    )

    # Países que adquieren más vacunas
    conti <- reactive(
        switch(input$grafcont, 
               "Africa" = va.af, 
               "América" = va.am, 
               "Asia" = va.as,
               "Europa" = va.eu)
    )
    
    output$plot <- renderPlot({
        
        nom <- input$grafcont
        
        ggplot(conti(), aes(x = country, y = Vacunas, fill = country))+
            geom_col()+
            labs(
                x = "País",              
                y = "Vacunas",  
                title = paste("Países que más han vacunado en ", nom),   
                fill = "País")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
    })
    
    # Países que han aplicado más vacunas en un día
    output$plot1 <- renderPlot({
        
        ggplot(va.di, aes(y = country, x = Vac.di, fill = Vac.di))+
            geom_col()+
            labs(
                x = "Vacunas",              
                y = "País",  
                fill = "# de vacunas")
    })
    
    
    # Marca más distribuida en cada continente
    conti1 <- reactive(
        switch(input$marcont, 
               "Africa" = marafr, 
               "América" = marame, 
               "Asia" = marasi,
               "Europa" = mareur)
    )
    
    output$plot2 <- renderPlot({
        
        nom <- input$marcont
        
        ggplot(conti1(), aes(x = vaccines, y = n, fill = n))+
            geom_col()+
            labs(
                x = "Marca",              
                y = "Países que aplican (la marca)",  
                title = paste("Marca de vacuna más distribuida en ", nom),   
                fill = "Número de paises"   
            )+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            scale_fill_identity()
    })
    
    # Correlación vacunados y contagiados en USA
    output$plot3 <- renderPlot({
        
        ggplot(cvv, aes(x=people_vaccinated, y=confirmed_cases)) + 
            geom_smooth(method = "lm", se = F) + 
            geom_point() + 
            labs(x = "Personas vacunadas", 
                 y = "Casos confirmados",
                 title = paste("Relación entre el número de contagios y personas vacunadas en EUA:")) + #labs coloca los nombres a los ejes y título de la gráfica
            theme(plot.title = element_text(hjust = 0.5))  + 
            theme(axis.text.x = element_text(face = "bold", color="#993333" , 
                                             size = 10, angle = 45, 
                                             hjust = 1),
                  axis.text.y = element_text(face = "bold", color="#993333" , 
                                             size = 10, angle = 45, 
                                             hjust = 1))
    })
    
    #Ajuste
    
    corvc <- lm(people_vaccinated~confirmed_cases)
    output$ajuste <- renderPrint({
        corvc
    })
    
    #Resumen
    output$summary <- renderPrint({
        summary(corvc)
    })
    
    # Series de tiempo
   

    
    conti2 <- reactive(
        switch(input$tscont, 
               "America" = ts.ame[,2:3], 
               "Asia" = ts.asia[,2:3], 
               "Europa" = ts.eur[,2:3] )
    )
    
    output$var <- renderUI({
        
        radioButtons("varname", 
                     "Elige un país", 
                     names(conti2()))
    })
    
    output$plot4 <- renderPlot({
        
        g1 <- ggplot(ts.ame, aes(x = date, y = USA)) +
            geom_line() +
            labs(x = "Fecha",
                 y = "Vacunados", 
                 title = "Administración de vacunas por día",
                 subtitle = "Estados Unidos, 20/12/2020 - 29/01/2021")+
            theme(axis.text.x = element_text(face = "bold", color="red" , 
                                             size = 10, angle = 45, 
                                             hjust = 1),
                  axis.text.y = element_text(face = "bold", color="red" , 
                                             size = 10, angle = 45, 
                                             hjust = 1))
        g2 <- ggplot(ts.ame, aes(x = date, y = Mexico)) +
            geom_line() +
            labs(x = "Fecha",
                 y = "Vacunados", 
                 title = "Administración de vacunas por día",
                 subtitle = "México, 25/12/2020 - 28/01/2021")+
            theme(axis.text.x = element_text(face = "bold", color="blue" , 
                                             size = 10, angle = 45, 
                                             hjust = 1),
                  axis.text.y = element_text(face = "bold", color="blue" , 
                                             size = 10, angle = 45, 
                                             hjust = 1))
        g3 <- ggplot(ts.asia, aes(x = date, y = China)) +
            geom_line() +
            labs(x = "Fecha",
                 y = "Vacunados", 
                 title = "Administración de vacunas por día",
                 subtitle = "China, 15/12/2020 - 27/01/2021")+
            theme(axis.text.x = element_text(face = "bold", color="dark red" , 
                                             size = 10, angle = 45, 
                                             hjust = 1),
                  axis.text.y = element_text(face = "bold", color="dark red" , 
                                             size = 10, angle = 45, 
                                             hjust = 1))
        g4 <- ggplot(ts.asia, aes(x = date, y = Israel)) +
            geom_line() +
            labs(x = "Fecha",
                 y = "Vacunados", 
                 title = "Administración de vacunas por día",
                 subtitle = "Israel, 20/12/2020 - 29/01/2021")+
            theme(axis.text.x = element_text(face = "bold", color="dark orange" , 
                                             size = 10, angle = 45, 
                                             hjust = 1),
                  axis.text.y = element_text(face = "bold", color="dark orange" , 
                                             size = 10, angle = 45, 
                                             hjust = 1))
        g5 <- ggplot(ts.eur, aes(x = date, y = Inglaterra)) +
            geom_line() +
            labs(x = "Fecha",
                 y = "Vacunados", 
                 title = "Administración de vacunas por día",
                 subtitle = "Inglaterra, 20/12/2020 - 28/01/2021")+
            theme(axis.text.x = element_text(face = "bold", color="#E35C0A" , 
                                             size = 10, angle = 45, 
                                             hjust = 1),
                  axis.text.y = element_text(face = "bold", color="#E35C0A" , 
                                             size = 10, angle = 45, 
                                             hjust = 1))
        g6 <- ggplot(ts.eur, aes(x = date, y = Alemania)) +
            geom_line() +
            labs(x = "Fecha",
                 y = "Vacunados", 
                 title = "Administración de vacunas por día",
                 subtitle = "Alemania, 28/12/2020 - 28/01/2021")+
            theme(axis.text.x = element_text(face = "bold", color="#840FE0" , 
                                             size = 10, angle = 45, 
                                             hjust = 1),
                  axis.text.y = element_text(face = "bold", color="#840FE0" , 
                                             size = 10, angle = 45, 
                                             hjust = 1))
        
        switch (input$varname,
            "USA" = g1,
            "Mexico" = g2,
            "China" = g3,
            "Israel" = g4,
            "Inglaterra" = g5,
            "Alemania" = g6
        )
    })
   
})
