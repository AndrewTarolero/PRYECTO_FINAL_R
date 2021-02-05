#librer�as a utilizar
library(dplyr)
library(ggplot2)
library(TSA)
library(lubridate)
##Paises que m�s han vacunado

# Se utiliz� el dataset country_vaccinations_1.csv que fue guardado en la variable vac
vac <- read.csv("https://raw.githubusercontent.com/AndrewTarolero/PRYECTO_FINAL_R/master/country_vaccinations_1.csv")

# Se extrajeron las columnas country, continent y total_vaccination  en un nuevo df llamado v que se usa 
# para cada contiente
names(vac)
v <- select(vac, country, continent, total_vaccinations)

#Africa
# En el dataset solo tenemos a Seychelles

afr <- which(v$continent == 'Africa')
vafr <- v[afr, ]
va.af <- as.data.frame(group_by(vafr, country)%>%
                         summarise(Vacunas = max(total_vaccinations, na.rm = TRUE)))
ggplot(va.af, aes(x = country, y = Vacunas, fill = country))+
geom_col()+
  labs(
    x = "Pa�s",              
   y = "Vacunas",  
   title = "Pa�ses que han adquirido m�s vacunas en Africa",   
    fill = "Pa�s")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Am�rica
# Estados Unidos seguido por Brasil, Canad� y M�xico

ame <- which(v$continent == 'America')  #Se filtran las filas de v donde el continente sea igual a Am�rica
vame <- v[ame, ]                        #Se guardan en un nuevo df
# Se agrupan los paises y se identifica el max en el numero total de dosis aplicadas (con la variable 
# total_vaccinations) en cada pa�s de Am�rica en el dataset
va.am <- as.data.frame(group_by(vame, country)%>%
                         summarise(Vacunas = max(total_vaccinations, na.rm = TRUE)))
#Gr�fica
america<-ggplot(va.am, aes(x = country, y = Vacunas, fill = country))+
  geom_col()+
  labs(
    x = "Pa�s",              
    y = "Vacunas",
    title = "Pa�ses que han adquirido m�s vacunas en Am�rica",   
    fill = "Pa�s")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
america+scale_y_continuous(breaks = seq(0,25000000,1000000)) 

#Asia
#China, despues Israel, India y Emiratos Arabes Unidos

asi <- which(v$continent == 'Asia') #Se filtran las filas de v donde el continente sea igual a Asia
vasi <- v[asi, ]                    #Se guardan en un nuevo df
# Se agrupan los pa�ses y se identifica el max en el numero total de dosis aplicadas (con la variable 
# total_vaccinations) en cada pa�s de Asia en el dataset
va.as <- as.data.frame(group_by(vasi, country)%>%
                         summarise(Vacunas = max(total_vaccinations, na.rm = TRUE)))
#Gr�fica
Asia<-ggplot(va.as, aes(x = country, y = Vacunas, fill = country))+
  geom_col()+
  labs(
    x = "Pa�s",              
    y = "Vacunas",  
    title = "Pa�ses que m�s han vacunado en Asia",   
    fill = "Pa�s")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
Asia+scale_y_continuous(breaks = seq(0,23000000,1000000))

#Europa
#Reino Unido, Inglaterra, Alemania, Italia, Espa�a y Francia

eur <- which(v$continent == 'Europe') #Se filtran las filas de v donde el continente sea igual a Europa
veur <- v[eur, ]                      #Se guardan en un nuevo df
# Se agrupan los paises y se identifica el max en el numero total de dosis aplicadas (con la variable 
# total_vaccinations) en cada pa�s de Europa en el dataset
va.eu <- as.data.frame(group_by(veur, country)%>%
                         summarise(Vacunas = max(total_vaccinations, na.rm = TRUE)))
#Gr�fica
EUR<-ggplot(va.eu, aes(x = country, y = Vacunas, fill = country))+
  geom_col()+
  labs(
    x = "Pa�s",              
    y = "Vacunas",  
    title = "Pa�ses que m�s han vacunado en Europa",   
    fill = "Pa�s")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
EUR+scale_y_continuous(breaks = seq(0,8000000,250000))

##�Qu� pa�s est� vacunando m�s personas diarias?


# Se extrajeron las columnas country, continent y daily_vaccination  en un nuevo df llamado vd 
# Se utiliza la variable daily_vaccination porque representa las dosis administradas por dia
vd <- select(vac, country, continent, daily_vaccinations) 
p <- max(vd$daily_vaccinations, na.rm = TRUE) # Se busca el maximo en los registros de vacunas diarias 
pd <- which(vd$daily_vaccinations == p)       # Se identifica el registro en el df usando la variable p
pais <- vd[pd, ]      #Se guarda el registro                      
pais  ## Estados Unidos con 1253815 vacunas, es el pa�s que vacuna a m�s personas en un d�a
#Se identifican los maximos de vacunas diarias agrupando por pais 
va.di <- as.data.frame(group_by(vd, country)%>%
                         summarise(Vac.di = max(daily_vaccinations, na.rm = TRUE)))
#Gr�fica de todos los pa�ses que representa el d�a con mayor dosis aplicadas 
ggplot(va.di, aes(y = country, x = Vac.di, fill = country))+
  geom_col()+
  labs(
    x = "Vacunas diarias",              
    y = "Pa�s",  
    title = "Paises que vacunan a m�s personas diarias",   
    fill = "Pa�s")
# Gr�fica de los diez pa�ses con el mayor n�mero de dosis aplicadas en un dia  
datos <- arrange(va.di, -Vac.di)
vac_diar <- head(datos, 10)
global<-ggplot(vac_diar, aes(x = country, y = Vac.di, fill = country))+
  geom_col()+
  labs(
    x = "Pa�s",              
    y = "Vacunas",  
    title = " 10 Pa�ses que vacunan a m�s personas diarias",   
    fill = "Pa�s")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
global+scale_y_continuous(breaks = seq(0,1200000,100000))

#Marca de vacuna m�s distribuida en cada continente


## Se extrajeron las columnas country, continent y vaccines en un nuevo df llamado mar que se usa 
# para cada contiente
mar <- select(vac, country, continent, vaccines) 

#Africa
#Oxford/AstraZeneca
africa <- which(mar$continent == 'Africa') #Se filtran las filas de mar donde el continente sea igual a Africa
mafr <- unique(mar[africa, ])              #Se guardan los registros sin repetir las marcas de las vacunas
# Se agrupan para poder ser contadas 
marafr <- mafr%>%
  group_by(vaccines)%>%
  tally()
#Gr�fica
ggplot(marafr, aes(x = vaccines, y = n, fill = n))+
  geom_col()+
  labs(
    x = "Marca",              
    y = "Pa�ses que aplican (la marca)",  
    title = "Marca de vacuna m�s distribuida en Africa",   
    fill = "N�mero de paises"   
  )+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Am�rica
#Pfizer/BioNTech
america <- which(mar$continent == 'America') #Se filtran las filas de mar donde el continente sea igual a Am�rica
mame <- unique(mar[america, ])               #Se guardan los registros sin repetir las marcas de las vacunas
# Se agrupan para poder ser contadas
marame <- mame%>%
  group_by(vaccines)%>%
  tally()
#Gr�fica
ggplot(marame, aes(x = vaccines, y = n, fill = n))+
  geom_col()+
  labs(
    x = "Marca",              
    y = "Pa�ses que aplican (la marca)",  
    title = "Marca de vacuna m�s distribuida en Am�rica",   
    fill = "N�mero de paises"   
  )+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Asia
#Pfizer/BioNTech
asia <- which(mar$continent == 'Asia') #Se filtran las filas de mar donde el continente sea igual a Asia
masi <- unique(mar[asia, ])            #Se guardan los registros sin repetir las marcas de las vacunas
# Se agrupan para poder ser contadas
marasi <- masi%>%
  group_by(vaccines)%>%
  tally()
#Gr�fica
ggplot(marasi, aes(x = vaccines, y = n, fill = n))+
  geom_col()+
  labs(
    x = "Marca",              
    y = "Pa�ses que aplican (la marca)",  
    title = "Marca de vacuna m�s distribuida en Asia",   
    fill = "N�mero de paises"   
  )+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Europa
#Pfizer/BioNTech
europa <- which(mar$continent == 'Europe') #Se filtran las filas de mar donde el continente sea igual a Europa
meur <- unique(mar[europa, ])              #Se guardan los registros sin repetir las marcas de las vacunas
# Se agrupan para poder ser contadas
mareur <- meur%>%
  group_by(vaccines)%>%
  tally()
#Gr�fica
ggplot(mareur, aes(x = vaccines, y = n, fill = n))+
  geom_col()+
  labs(
    x = "Marca",              
    y = "Pa�ses que aplican (la marca)",  
    title = "Marca de vacuna m�s distribuida en Europa",   
    fill = "N�mero de paises"   
  )+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## Correlaci�n entre vacunados y contagiados de USA (ya que es el pa�s con m�s datos)
cvv<- read.csv("https://raw.githubusercontent.com/AndrewTarolero/PRYECTO_FINAL_R/master/cases_vs_vaccined.csv")

#verificando el tipo de datos
str(cvv)
attach(cvv)
#cambiando confirmed_cases de chr a num y people_vaccinated de int a num
cvv<-na.omit(cvv)

str(cvv)

ggplot(cvv, aes(x=people_vaccinated, y=confirmed_cases)) + 
  geom_smooth(method = "lm", se = F) + #geom_smoth nos coloca la linea de ajuste
  geom_point() + #geom_point nos gr�fica los datos con estilo tipo puntos
  labs(x = "Personas vacunadas", 
       y = "Casos confirmados",
       title = paste("Relaci�n entre el n�mero de contagios y personas vacunadas en EUA:")) + #labs coloca los nombres a los ejes y t�tulo de la gr�fica
  theme(plot.title = element_text(hjust = 0.5))  + #se ajusta el t�tulo del gr�fico al centro
  theme(axis.text.x = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1))  #color, �ngulo y estilo de las abcisas y ordenadas 

corvc<-lm(people_vaccinated~confirmed_cases) #correlaci�n

summary(corvc) #resumen estad�stico de la gr�fica de correlaci�n, extraemos R ajusted y p-value


## series de tiempo de un par de pa�ses por cada continente 
## Excepto �frica

#Am�rica 
#Consideramos EUA y nuestro panorama nacional (M�xico)
ts.ame<-read.csv("https://raw.githubusercontent.com/AndrewTarolero/PRYECTO_FINAL_R/master/TS_AME.csv", header = TRUE)
str(ts.ame) #revisar los tipos de datos

ts.ame<-mutate(ts.ame, date = as.Date(date, "%d/%m/%Y")) #cambiando de char a date
str(ts.ame) #verificando el cambio

#SERIE DE TIEMPO DE VACUNADOS DIARIOS PARA EUA
ggplot(ts.ame, aes(x = date, y = daily_vaccinations_EUA)) +
  geom_line() +
  labs(x = "Fecha",
       y = "Vacunados", 
       title = "Administraci�n de vacunas por d�a",
       subtitle = "Estados Unidos, 20/12/2020 - 29/01/2021")+
  theme(axis.text.x = element_text(face = "bold", color="red" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="red" , 
                                   size = 10, angle = 45, 
                                   hjust = 1))  #color, �ngulo y estilo de las abcisas y ordenadas 

#SERIE DE TIEMPO DE VACUNADOS DIARIOS PARA MEX
ggplot(ts.ame, aes(x = date, y = daily_vaccinations_MEX)) +
  geom_line() +
  labs(x = "Fecha",
       y = "Vacunados", 
       title = "Administraci�n de vacunas por d�a",
       subtitle = "M�xico, 25/12/2020 - 28/01/2021")+
  theme(axis.text.x = element_text(face = "bold", color="blue" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , 
                                   size = 10, angle = 45, 
                                   hjust = 1))  #color, �ngulo y estilo de las abcisas y ordenadas 

#Asia
#Consideramos CHINA e ISRAEL
ts.asia<-read.csv("https://raw.githubusercontent.com/AndrewTarolero/PRYECTO_FINAL_R/master/TS_ASIA.csv", header = TRUE)
str(ts.asia) #revisar los tipos de datos

ts.asia<-mutate(ts.asia, date = as.Date(date, "%d/%m/%Y")) #cambiando de char a date
str(ts.asia) #verificando el cambio

#SERIE DE TIEMPO DE VACUNADOS DIARIOS PARA ENG
ggplot(ts.asia, aes(x = date, y = daily_vaccinations_CHI)) +
  geom_line() +
  labs(x = "Fecha",
       y = "Vacunados", 
       title = "Administraci�n de vacunas por d�a",
       subtitle = "China, 15/12/2020 - 27/01/2021")+
  theme(axis.text.x = element_text(face = "bold", color="dark red" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="dark red" , 
                                   size = 10, angle = 45, 
                                   hjust = 1))  #color, �ngulo y estilo de las abcisas y ordenadas 

#SERIE DE TIEMPO DE VACUNADOS DIARIOS PARA ISR
ggplot(ts.asia, aes(x = date, y = daily_vaccinations_ISR)) +
  geom_line() +
  labs(x = "Fecha",
       y = "Vacunados", 
       title = "Administraci�n de vacunas por d�a",
       subtitle = "Israel, 20/12/2020 - 29/01/2021")+
  theme(axis.text.x = element_text(face = "bold", color="dark orange" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="dark orange" , 
                                   size = 10, angle = 45, 
                                   hjust = 1))  #color, �ngulo y estilo de las abcisas y ordenadas 

#Europa
#Consideramos Inglaterra y Alemania
ts.eur<-read.csv("https://raw.githubusercontent.com/AndrewTarolero/PRYECTO_FINAL_R/master/TS_EUR.csv", header = TRUE)
str(ts.eur) #revisar los tipos de datos

ts.eur<-mutate(ts.eur, date = as.Date(date, "%d/%m/%Y")) #cambiando de char a date
str(ts.eur) #verificando el cambio

#SERIE DE TIEMPO DE VACUNADOS DIARIOS PARA ENG
ggplot(ts.eur, aes(x = date, y = daily_vaccinations_ENG)) +
  geom_line() +
  labs(x = "Fecha",
       y = "Vacunados", 
       title = "Administraci�n de vacunas por d�a",
       subtitle = "Inglaterra, 20/12/2020 - 28/01/2021")+
  theme(axis.text.x = element_text(face = "bold", color="#E35C0A" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="#E35C0A" , 
                                   size = 10, angle = 45, 
                                   hjust = 1))  #color, �ngulo y estilo de las abcisas y ordenadas 

#SERIE DE TIEMPO DE VACUNADOS DIARIOS PARA GER
ggplot(ts.eur, aes(x = date, y = daily_vaccinations_GER)) +
  geom_line() +
  labs(x = "Fecha",
       y = "Vacunados", 
       title = "Administraci�n de vacunas por d�a",
       subtitle = "Alemania, 28/12/2020 - 28/01/2021")+
  theme(axis.text.x = element_text(face = "bold", color="#840FE0" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="#840FE0" , 
                                   size = 10, angle = 45, 
                                   hjust = 1))  #color, �ngulo y estilo de las abcisas y ordenadas 



