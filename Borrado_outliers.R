#Leyendo datos
library(readxl)
BD <- read_excel("G:/.shortcut-targets-by-id/1MzjPuxOjHNdqXYR0YyVgZIOTjRco3plO/MEA_Estadistica/Datos/curated/20201008_BD.xlsx", 
                 sheet = "BD", col_types = c("text", "text", 
                                             "text", "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric", 
                                             "numeric", "numeric", "numeric"))
View(BD)


names(BD)
#Librerias
library(dslabs)
library(dplyr)

boxplot(BD$CT_CTE)
boxplot(CT_CTE~A?o, data=BD)

#se ha sumado el total de cada a?o para cada nit
TotalN<-BD%>%group_by(Nit)%>%summarise(T=sum(CT_CTE))

#Vector l?gico para filtrar los datos atipicos. False=Atipico
Q3<-quantile(TotalN$T, prob=c(.75))
Q1<-quantile(TotalN$T, prob=c(.25))
TotalN<-mutate(TotalN, indexOUT=(TotalN$T<=Q3 & TotalN$T>=Q1))

#Eliminando todo lo que sea false, para eliminar los outliers
TotalN<-TotalN[TotalN$indexOUT!="FALSE",]
boxplot(TotalN$T)
hist(TotalN$T)

#Indicando en la base de datos original los que aparecen en la  de TotalN que contiene los nit que no son at?picos
BDC=merge(TotalN[,c("Nit","indexOUT")],BD[,c("A?o","Nit","CIIU","TCyG","TI","PIB","Ccorr","TRM","Tasa_I","Tasa_D","Inflaci?n","BF","ICCV_TV","ICCV_Mat","ICCV_MO","ICCV_ME","ICCV_Unif","ICCV_Unim","IPP","IPP_Mat","IPVNC","IPVN_NC","IPVN_TN","IPVU","Ce_pro","Ce_des","Co_pro","CT_CTE","ING_CTE","PIB_CTE","TI_REZ","TD_REZ","VAR PIB")])

#