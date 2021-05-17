remove(list = ls())

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

fecha<-"2015-1-1"

gruma<-getSymbols.yahoo("GRUMAB.MX", from=fecha , auto.assign=F)

View(gruma)
#Grafico básico
chartSeries(gruma, type = "auto",subset = NULL, show.grid = TRUE, name = NULL,time.scale = NULL,
            log.scale = FALSE, TA = 'addVo()', TAsep=';', line.type = "l",bar.type = "ohlc", 
            theme = chartTheme("white"), layout = NA,major.ticks='auto', minor.ticks=TRUE, yrange=NULL,
            plot=TRUE, color.vol = TRUE, multi.col = F,)

gruma_ret <- na.omit(dailyReturn(gruma[,6], type = "log")) # Solo el cierre ajustado


# Grafico rendimientos acumulados
chart.CumReturns(gruma_ret)

####################################### MAS ACCIONES ######################################################

# Selección de activos
nombres <- c("TSLA", "BTC-USD")
  #c("AC.MX" , "ACCELSAB.MX" , "ACTINVRB.MX" , "AEROMEX.MX" , "AGUA.MX" , "ALEATIC.MX" , "ALFAA.MX" , "ALPEKA.MX" , "ALSEA.MX" , "AMXL.MX" , "ARA.MX" , "ARISTOSA.MX" , "ASURB.MX" , "AUTLANB.MX" , "AXTELCPO.MX" , "AZTECACPO.MX" , "BACHOCOB.MX" , "BAFARB.MX" , "BBAJIOO.MX" , "BIMBOA.MX" , "BOLSAA.MX" , "BSMXB.MX" , "CADUA.MX" , "CEMEXCPO.MX" , "CERAMICB.MX" , "CHDRAUIB.MX" , "CIDMEGA.MX" , "CIEB.MX" , "CMOCTEZ.MX" , "CMRB.MX" , "COLLADO.MX" , "CREAL.MX" , "CUERVO.MX" , "CULTIBAB.MX" , "CYDSASAA.MX" , "DINEB.MX" , "ELEKTRA.MX" , "ELEMENT.MX" , "FEMSAUBD.MX" , "FINAMEXO.MX" , "FRAGUAB.MX" , "GAPB.MX" ,  "GBMO.MX" , "GCARSOA1.MX" , "GCC.MX" , "GENTERA.MX" , "GFAMSAA.MX" , "GFINBURO.MX" , "GFMULTIO.MX" , "GFNORTEO.MX" , "GICSAB.MX" , "GIGANTE.MX" , "GISSAA.MX" , "GMD.MX" , "GMEXICOB.MX" ,  "GNP.MX" , "GPH1.MX" , "GPROFUT.MX" , "GRUMAB.MX" , "GSANBORB-1.MX" , "HCITY.MX" , "HERDEZ.MX" , "HOMEX.MX" , "HOTEL.MX" , "ICHB.MX" , "IDEALB-1.MX" , "IENOVA.MX" , "INVEXA.MX" , "KIMBERA.MX" , "KOFUBL.MX" , "KUOB.MX" , "LABB.MX" , "LACOMERUBC.MX" , "LALAB.MX" , "LAMOSA.MX" , "LIVEPOLC-1.MX" , "MAXCOMA.MX" , "MEDICAB.MX" , "MEGACPO.MX" , "MFRISCOA-1.MX" , "MINSAB.MX" , "MONEXB.MX" , "NEMAKA.MX" , "OMAB.MX" , "ORBIA.MX" , "PAPPEL.MX" , "PASAB.MX" , "PE&OLES.MX" , "PINFRA.MX" , "POCHTECB.MX" , "POSADASA.MX" , "Q.MX" , "RCENTROA.MX" , "RLHA.MX" , "SIMECB.MX" , "SITESB-1.MX" , "SORIANAB.MX" , "SPORTS.MX" , "TEAKCPO.MX" , "TLEVISACPO.MX" , "TMMA.MX" , "TRAXIONA.MX" , "UNIFINA.MX" , "URBI.MX" , "VALUEGFO.MX" , "VESTA.MX" , "VINTE.MX" , "VISTAA.MX" , "VITROA.MX" , "VOLARA.MX" , "WALMEX.MX")

# Obtencion de datos
precios <- NULL
for(i in nombres){
  precios <- cbind(precios, getSymbols.yahoo(i, from=fecha, periodicity = "daily", auto.assign=F)[,4])
}

# Gradfico de los precios
chart.TimeSeries(scale(precios))

# Chequeo de la base
colSums(is.na(precios))

# Calculo de rendimientos individuales de portafolio estático
rendimientos_individuales <- na.omit(ROC(precios))


library(xlsx)
write.xlsx(rendimientos_individuales,                    # Data frame to be exported
           "NUEVO.xls")         

# Precios de referencia
ipc <- getSymbols.yahoo("^MXX", from=fecha, periodicity = "daily", auto.assign=F)[,4]

# Calculo de rendimientos IPC
rendimientos_ipc <- na.omit(ROC(ipc)) # tasa de cambio continua
colnames(rendimientos_ipc) <- "rendimientos_ipc"
head(rendimientos_ipc)
colSums(is.na(ipc))

# Rendimientos ponderados del portafolio
rendimientos_portafolio_estatico <- Return.portfolio(rendimientos_individuales)
chart.CumReturns(rendimientos_portafolio_estatico)

### Metricas basicas del portafolio ###

# Potencial de diversificación
CAPM.beta(rendimientos_portafolio_estatico, rendimientos_ipc, Rf=0.06/252) # > 1 mas riesgoso que el IPC

# Rendimiento de exceso ajustado a la tasa de riesgo libre de 6%
CAPM.jensenAlpha(rendimientos_portafolio_estatico, rendimientos_ipc, Rf=0.06/252) # >0 le ganas al IPC

# Rendimiento por unidad de riesgo ajustado
SharpeRatio(rendimientos_portafolio_estatico, Rf=0.06/252)

# Rendimientos, Riesgo y SharpeRatio anualizados
table.AnnualizedReturns(rendimientos_portafolio_estatico)
table.CalendarReturns(rendimientos_portafolio_estatico)

### Especificación modelo dinámico ###

# Especificamos el portafolio
portf <- portfolio.spec(colnames(rendimientos_individuales))

# Añadimos restricciones
portf <- add.constraint(portf, type = "weight_sum", min_sum=0.99, max_sum=1.01) 
portf <- add.constraint(portf, type = "box", min=0.01, max=.99) # para cada activo individual
portf <- add.constraint(portf, type = "transaction_cost", ptc = 0.01) # Costo de transacción proporcional del 1%

# Añadimos objetivos {anualizados}
portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="long_only")
portf <- add.objective(portf, type = "return", name ="mean") #maximizar el rendimiento
portf <- add.objective(portf, type = "risk", name= "StdDev", target = 0.1) #Minimizar el riesgo {volatilidad diria}

#
## Backtesting Otimización
#

# Portafolios aleatorios
pa <- random_portfolios(portf, 1000, "sample") #genera 10000 portafolios

# Rebalanceo
op_reb <- optimize.portfolio.rebalancing(rendimientos_individuales,
                                         portf, 
                                         optimize_method = "random",
                                         rp=pa,
                                         rebalance_on = "months",
                                         training_period = 30, 
                                         rolling_window = 14) 


# Visualización los portafolios rebalanceados {pesos dinámicos}
chart.Weights(op_reb, main="Pesos Rebalanceados en el tiempo")
x <- extractWeights(op_reb)

### Comparación ###

# Pesos equilibrados
pesos_equilibrados <- rep(1/ncol(rendimientos_individuales),ncol(rendimientos_individuales)) # pesos equilibrados
portafolio_equilibrado <- Return.portfolio(rendimientos_individuales, weights =pesos_equilibrados)
colnames(portafolio_equilibrado) <- "Portafolio Estático"

# Pesos rebalanceados
pesos_rebalanceados <- extractWeights(op_reb)
porfolio_rebalanceado <- Return.portfolio(rendimientos_individuales, weights = pesos_rebalanceados)
colnames(porfolio_rebalanceado) <- "Portafolio Dinámico Rebalanceado"

# Grafico de desempleño en el tiempo
desempeño <- cbind(porfolio_rebalanceado, portafolio_equilibrado, rendimientos_ipc)
x11()
charts.PerformanceSummary(desempeño, main = "Desempeño en el tiempo", legend.loc = "center" )

### FIN ###

