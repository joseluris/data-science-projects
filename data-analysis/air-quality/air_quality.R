library(readr)
calidad_del_aire_datos_historicos_diarios <- read_delim("calidad-del-aire-datos-historicos-diarios.csv", 
                                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

datos_joseluis_risco <- calidad_del_aire_datos_historicos_diarios

datosZA2014 <- subset(datos_joseluis_risco, Provincia == "Zamora" & Estación == "Zamora 2" & Fecha >= "2014-01-01" & Fecha <= "2014-12-31")
datosZA2015 <- subset(datos_joseluis_risco, Provincia == "Zamora" & Estación == "Zamora 2" & Fecha >= "2015-01-01" & Fecha <= "2015-12-31")


datosZA2014$Fecha[is.na(datosZA2014$`PM10 (ug/m3)`)]

png(file = "plot6.png", width = 880, height = 480, bg = "transparent")
plot(`PM10 (ug/m3)` ~ Fecha, data=subset(datosZA2014, Fecha >= "2014-02-01" & Fecha <= "2014-02-14"), main = "Calidad del aire en Zamora JOSE LUIS RISCO")
lines(`PM10 (ug/m3)` ~ Fecha, data = subset(datosZA2014, Fecha >= "2014-02-01" & Fecha <= "2014-02-14"), col = "red")
fit1 <- lm(`PM10 (ug/m3)` ~ Fecha, data = subset(datosZA2014, Fecha >= "2014-02-01" & Fecha <= "2014-02-14"))
abline(fit1, col = "blue")
dev.off()

realPM10 <- subset(datosZA2014, Fecha >= "2014-02-01" & Fecha <= "2014-02-14")
realPM10 <- realPM10$`PM10 (ug/m3)` 
predPM10 <- predict(fit1, subset(datosZA2014, Fecha >= "2014-02-01" &Fecha <= "2014-02-14"))
errorPM10 <- realPM10 - predPM10
errorPM10 <- errorPM10[!is.na(errorPM10)]
sum(errorPM10^2) / length(errorPM10)

subset(datosZA2014, Fecha == "2014-02-12")$`PM10 (ug/m3)` 
subset(datosZA2014, Fecha == "2014-02-13")$`PM10 (ug/m3)` 
datosZA2014$`PM10 (ug/m3)` [datosZA2014$Fecha == "2014-02-12"] <- predict(fit1, data.frame(Fecha = as.Date("2014-02-12")))
datosZA2014$`PM10 (ug/m3)` [datosZA2014$Fecha == "2014-02-13"] <- predict(fit1, data.frame(Fecha = as.Date("2014-02-13")))
subset(datosZA2014, Fecha == "2014-02-12")$`PM10 (ug/m3)` 
subset(datosZA2014, Fecha == "2014-02-13")$`PM10 (ug/m3)` 
