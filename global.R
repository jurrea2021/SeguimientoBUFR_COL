library(readxl)
library(leaflet)
library(plotly)
#library(inlmisc)

RUTA = "data/"
# RUTA = "I:/.shortcut-targets-by-id/1_iDGC_Qb_ZqoaK1zmBwRPDF2lwFTGQtq/CTO. 334-2023 JULIAN URREA/ProductosRelacionados/09_IntercambioDatosBUFR/ShinyBUFR/data/"

#conteoDiaVariableEst = as.data.frame(readxl::read_excel(paste0(RUTA,"conteoDiaVariableEst_Octubre2023.xlsx")))
load(paste0(RUTA,"conteoDiaVariableEst.rdata"))
conteoDiaVariableEst = conteoDiaVariableEstFIN
# conteoDiaVariableEst2 = as.data.frame(readxl::read_excel(paste0(RUTA,"conteoDiaVariableEst2_Octubre2023.xlsx")))
load(paste0(RUTA,"conteoDiaVariableEst2.rdata"))
conteoDiaVariableEst2 = conteoDiaVariableEst2FIN
CNE_IDEAM = as.data.frame(read_excel(paste0(RUTA,"CNE_IDEAM.xls")))
CNE_IDEAM$CODIGO = as.numeric(CNE_IDEAM$CODIGO)


estPolaris = read.csv(paste0(RUTA,"/StationSearchResults_31082023.csv"), encoding="UTF-8")
estPolaris$Date.established = strptime(substr(estPolaris$Date.established,2,nchar(estPolaris$Date.established)),"%Y-%m-%d")
estPolaris$Date.closed = strptime(substr(estPolaris$Date.closed,2,nchar(estPolaris$Date.closed)),"%Y-%m-%d")
estPolaris$Last.updated.date = strptime(substr(estPolaris$Last.updated.date,2,nchar(estPolaris$Last.updated.date)),"%Y-%m-%d")

estPolaris$codigo = NA

for (i in 1:dim(estPolaris)[1]) {
  estPolaris$codigo[i] = as.numeric(substr(estPolaris$WIGOS.Station.Identifier.s.[i],
                                           which(strsplit(estPolaris$WIGOS.Station.Identifier.s.[i],"")[[1]] == "-")[3] + 1,
                                           which(strsplit(estPolaris$WIGOS.Station.Identifier.s.[i],"")[[1]] == "|") - 1))
}


CNE_IDEAM = merge(CNE_IDEAM,estPolaris[,c("codigo","Station")],by.x = "CODIGO",by.y = "codigo",all.x = T)

CNE_IDEAM$OSCAR = NA
CNE_IDEAM$OSCAR[which(is.na(CNE_IDEAM$Station))] = "NO"
CNE_IDEAM$OSCAR[which(!is.na(CNE_IDEAM$Station))] = "SI"
CNE_IDEAM$Station = NULL


#conteoDia = as.data.frame(readxl::read_excel(paste0(RUTA,"/conteoDia_Octubre2023.xlsx")))
load(paste0(RUTA,"conteoDiaFIN.rdata"))
conteoDia = conteoDiaFIN
#conteoDiaVar = as.data.frame(readxl::read_excel(paste0(RUTA,"/conteoDiaVariable_Octubre2023.xlsx")))
load(paste0(RUTA,"conteoDiaVariable.rdata"))
conteoDiaVar = conteoDiaVariableFIN
