######## Inicializacion Libreria Y Excel ###
library(readxl)
library(latex2exp)


DatosEncuesta <- read_excel('E:\\Estudios\\Estadistica Y Probabilidad\\DatosEncuestaEstadistica.xlsx')

N <- length(DatosEncuesta$`Nombre completo`)

####### Punto 1 #########

x_data <- DatosEncuesta$`¿A cuántas clases usted no asistió debido a un mal horario académico en el semestre pasado?`
y_data <- as.numeric(DatosEncuesta$`Cuantos minutas demora filtrado`)

model <- lm(y_data~x_data)
#print(summary(model))
par(mfrow=c(1,1))
plot(x_data,y_data,col="blue",
     main="Tiempo De Llegada A La Universidad Vs Horas De Clase Perdidas",
     xlab = "Cantidad De Horas De Clase Perdidas",
     ylab = "Minutos Que Tarda En Llegar A La Universidad")
abline(model,col="red")


######## Punto 2 ##########

#Funcion para la creacion de las tablas
funcion_tablas <- function(title, rows,columns){
  tabla_contingencia <- table(rows,columns)
  tabla_con_totales <- addmargins(tabla_contingencia)
  print(title)
  print(tabla_con_totales)
  print("Tabla De Probabilidad")
  tabla_probabilidad <- round(tabla_con_totales/N,3)
  print(tabla_probabilidad)
}


#Medio De Transporte Vs Tipo De Jornada 
print("\\\\\\ Medio De Transporte Vs Tipo De Jornada \\\\ ")

rows <- DatosEncuesta$ `¿Qué tipo de jornada generalmente prefiere para estudiar?`
columns <- DatosEncuesta$`¿Qué medio de transporte usa habitualmente para llegar a la universidad?`

tabla1<- funcion_tablas("Medio De Transporte Vs Tipo De Jornada",rows,columns)

#Criterio Principal Horario Vs Tipo De Jornada
print("\\\\\\ Criterio Principal Horario \\\\ ")

rows <- DatosEncuesta$`¿Qué tipo de jornada generalmente prefiere para estudiar?`
columns <- DatosEncuesta$`¿Cuál es tu principal criterio al elegir una franja horaria para inscribir clases?`

tabla2 <- funcion_tablas("Criterio Principal Horario Vs Tipos De Jornada",rows,columns)

##### Punto Adicional #####

x <- 1:8
print(x)

p <- c(0,0.296,0,0.111,0.074,0.037,0.444,0.037)
# Valor esperado
esperado <- sum(x * p)

# Varianza
varianza <- sum((x - esperado)^2 * p)

# Desviación estándar
sd <- sqrt(varianza)

# Coeficiente de variación
cv <- sd / esperado

# Asimetría
(x-esperado)^3 * p
asimetria <- sum((x - esperado)^3 * p)

# Coeficiente de asimetría
coef_asimetria <- asimetria / sd^3

cat("Valor esperado:", esperado, "\n")
cat("Varianza:", varianza, "\n")
cat("Desviación estándar:", sd, "\n")
cat("Coeficiente de variación:", cv, "\n")
cat("Asimetría:", asimetria, "\n")
cat("Coeficiente de asimetría:", coef_asimetria, "\n")

categorias <- c("Automóvil", "Bicicleta", "Bus", "Cami.", "Moto",
                "SITP", "TransMilenio", "Intermunicipal + TM")
par(mfrow=c(1,1))
barplot(p,
        names.arg = categorias,
        col = "skyblue",
        las = 2,  # gira etiquetas para que se lean mejor
        ylim = c(0, max(p) + 0.1),
        main = "Distribución de variable aleatoria X (Medio de transporte)",
        xlab = "Medios de transporte (codificados)",
        ylab = "Probabilidad")

##### Punto 3 ########

#Distribucion Bernoulli

prob_p <- function(xlsxData, var_id,n=N){
  a<- table(xlsxData)
  p<-sum(a[names(a)==var_id])/n
  return(p)
}

v_bern <- function(x,p){
  return( (p**x)*((1-p)**(1-x)) )
}

best_graphs_configuration <- function(name_data){
  total <- length(name_data)
  rows <- ceiling(sqrt(total))
  columns <- ceiling(total / rows)
  par(mfrow = c(rows, columns))
}

tabla_de_frecuencia <- function(xlsxData, name_data, title = "Tabla de Frecuencia") {
  # Contar cuántas veces aparece cada categoría en name_data dentro de xlsxData
  frecuencias <- sapply(name_data, function(nombre) sum(xlsxData == nombre))
  
  # Crear la tabla como data frame
  tabla <- data.frame(Categoría = name_data,
                      Frecuencia = frecuencias)
  
  # Mostrar el título y la tabla
  cat("\n", title, "\n")
  print(tabla, row.names = FALSE)
  print("  ")
  #return(tabla)
}

datos <- c("Docente","Horario","Trabajo","Ninguno")
datosXlx <- DatosEncuesta$`¿Cuál es tu principal criterio al elegir una franja horaria para inscribir clases?`

datosXlx1<-DatosEncuesta$`¿Qué medio de transporte usa habitualmente para llegar a la universidad?`
datos1 <- c("Automóvil","Bicicleta","Caminando","Moto","SITP","TransMilenio","Utilizo bus intermunicipal y TM")

datosXlx2<-DatosEncuesta$ `¿Qué tipo de jornada generalmente prefiere para estudiar?`
datos2 <- c("Mañana","Tarde","Indiferente")

tabla_de_frecuencia(datosXlx,datos,"Criterio Horario")
tabla_de_frecuencia(datosXlx1,datos1,"Medio De Transporte")
tabla_de_frecuencia(datosXlx2,datos2,"Jornada Preferida")


plot_bern <- function(xlsxData,name_data){
  best_graphs_configuration(name_data)
  
  for (i in 1:length(name_data)){
    p <- prob_p(xlsxData,name_data[i])
    v <-c(v_bern(0,p),v_bern(1,p))
    barplot(v,
            names.arg = c("0 (Fracaso)", "1 (Éxito)"),
            col = c("red4","green3"),
            ylim = c(0, 1),
            main = paste("Bernoulli (p~",round(p,2), ") para", name_data[i]),
            ylab = "Probabilidad")
    
  }
}


plot_bern(datosXlx,datos)
plot_bern(datosXlx1,datos1)
plot_bern(datosXlx2,datos2)

#Binomial para n=10

k<-0:10 #n=10

set.seed(1010)
m <- sample(datosXlx,10)
m1 <- sample(datosXlx1,10)
m2 <- sample(datosXlx2,10)

binom_graph <- function(samp,name_data){
  best_graphs_configuration(name_data)
  for(i in 1:length(name_data)){
    p <- prob_p(samp,name_data[i],n=10)
    probs <- dbinom(k,size=10,prob=p)
    barplot(probs,
            names.arg = k,
            col = c("tomato", "gold", "skyblue")[i %% 3 + 1],
            ylim = c(0, max(probs) + 0.05),
            xlim = c(0, 11),
            main = TeX(sprintf("Binomial($p$ = %.2f) para %s", p, name_data[i])),
            xlab = "Número de éxitos (k)",
            ylab = "P(X = k)")
  }
}

binom_graph(m,datos)
binom_graph(m1,datos1)
binom_graph(m2,datos2)

#Distribucion de poisson
k <- 0:30 #n =30

poisson_graph <- function(samp1,samp2,name_data,n=30){
  best_graphs_configuration(name_data)
  for(i in 1:length(name_data)){
    p <- prob_p(samp1, name_data[i],n=10)
    lambda <- n * p  
    probs <- dpois(k, lambda = lambda)
    
    barplot(probs,
            names.arg = k,
            col = c("tomato", "gold", "skyblue")[i %% 3 + 1],
            ylim = c(0, max(probs) + 0.05),
            main = TeX(sprintf("Poisson($\\lambda$ = %.2f) para %s", lambda, name_data[i])),
            xlab = "Número de ocurrencias (k)",
            ylab = "P(X = k)")
  }
}

mp <- sample(datosXlx,30)
mp1 <- sample(datosXlx1,30)
mp2 <- sample(datosXlx2,30)


poisson_graph(m,mp,datos)
poisson_graph(m1,mp1,datos1)
poisson_graph(m2,mp2,datos2)

#Distribucion Normal

k<-0:33

mn <- sample(datosXlx,33)
mn1 <- sample(datosXlx1,33)
mn2 <- sample(datosXlx2,33)

tabla_normal_prob <- function(samp, name_data, n = 33) {
  for(i in 1:length(name_data)) {
    cat("\n--- Tabla para:", name_data[i], "---\n")
    
    # Calcular p y parámetros de la normal
    p <- prob_p(samp, name_data[i])
    avg <- n * p
    var <- n * p * (1 - p)
    sd <- sqrt(var)
    
    # Valores de k (0 a n)
    k <- 0:n
    
    # Probabilidad y acumulada
    probs <- dnorm(k, mean = avg, sd = sd)
    acumulada <- pnorm(k, mean = avg, sd = sd)
    acumulada_inv <- 1-acumulada
    
    # Crear tabla
    tabla <- data.frame(
      k = k,
      'P(X = k)' = round(probs, 5),
      'P(X <= k)' = round(acumulada, 5),
      'P(X > k)' = round(acumulada_inv,5),
      check.names = FALSE
    )
    
    # Mostrar tabla
    print(tabla, row.names = FALSE)
  }
}


normal_distr <- function(samp,name_data,n=33){
  best_graphs_configuration(name_data)
  
  for(i in 1:length(name_data)){
    #Aproximacion por medio de las relaciones de promedio y desviacion de la Binomial
    #mu=n*p y sigma^2=n*p*q
    p <- prob_p(samp,name_data[i])
    avg <- n*p
    var <- n*p*(1-p)
    sd <- sqrt(var)
    probs <- dnorm(k,mean=avg,sd=sd)
    
    bars <- barplot(probs,
            names.arg = k,
            col = c("tomato", "gold", "skyblue")[i %% 3 + 1],
            ylim = c(0, max(probs) + 0.1),
            main = TeX(sprintf("Normal($\\bar{x}$ = %.2f) para %s", avg, name_data[i])),
            # main = paste("Normal($\bar{x}$" ,"=", round(avg, 2), ") para", name_data[i]),
            xlab = "Número de ocurrencias (k)",
            ylab = "P(X = k)")
    text(x = bars[which.min(abs(k - avg))], 
         y = max(probs), 
         labels = sprintf("μ = %.2f\nσ² = %.2f", avg, var),
         pos = 3, cex = 0.8, col = "black")
  }
}
normal_distr(mn,datos)
#tabla_normal_prob(mn,datos)
normal_distr(mn1,datos1)
#tabla_normal_prob(mn1,datos1)
normal_distr(mn2,datos2)
#tabla_normal_prob(mn2,datos2)
prob_p(datosXlx2,"Mañana")*N

# Prueba De Hipotesis Distribucion Normal

hipotesis_test_norm <- function(samp,poblat,sglvl=0.05){
  x_bar <- mean(samp)
  sd_samp <- sd(samp)
  n <- length(samp)
  x_pob <- mean(poblat)
  cat("Media Poblacional: ",x_pob,"\n")
  cat("Media Muestral: ",x_bar,"\n")
  cat("desviacion Muestral: ",sd_samp,"\n")
  z <- (x_bar-x_pob)/((sd_samp)/(sqrt(n)))
  z_crit <- qnorm(sglvl/2)
  p_valor <- 2 * (1 - pnorm(abs(z)))
  
  cat("Z value: ",z,"\n")
  cat("Z crit: ",z_crit,"\n")
  cat("p valor: ",p_valor,"\n")
  return(c(z,z_crit))
}

verificar_hipotesis_z <- function(z, z_crit, alpha = 0.05) {
  par(mfrow =c(1,1))
  decision <- if (abs(z) > abs(z_crit)) "rechazada" else "no rechazada"
  
  cat("Estadístico z:", round(z, 3), "\n")
  cat("Valor crítico z:", round(z_crit, 3), "\n")
  cat("Decisión: Hipótesis nula", decision, "\n\n")
  
  # Graficar sólo si se rechaza
  xlim <- qnorm(0.001)
  x_vals <-seq(-xlim, xlim, length.out = 200)
  y_vals <- dnorm(x_vals)
    
  plot(x_vals, y_vals, type = "l", lwd = 2, col = "black",
       main = "Distribución Normal Estándar",
       ylab = "Densidad", xlab = "z")
  abline(v = c(-z_crit, z_crit), col = "blue", lty = 2)
  abline(v = z, col = "red", lwd = 2)
    
  # Sombrear áreas críticas
  x_crit <- seq(-abs(z_crit),abs(z_crit),length=50)
  y_crit <-dnorm(x_crit)
  polygon(c(-abs(z_crit),x_crit,abs(z_crit)),
          c(0,y_crit,0),
          col="green",
          border=NA,
          density=50)
  
    
  legend("topright", legend = c("Z observado", "Z crítico", "Área De Aceptacion"),
          col = c("red", "blue", "green"), lty = c(1, 2, NA), pch = c(NA, NA, 15))
  
}

datosHipotesis <- DatosEncuesta$`Cuantos minutas demora filtrado`
mu_pob <- sum(datosHipotesis)/length(datosHipotesis)
sd_pob <- sd(datosHipotesis)
0.5-pnorm(0.489)
muestra <-sample(datosHipotesis,33)

v_z <- hipotesis_test_norm(muestra,datosHipotesis,0.05)
verificar_hipotesis_z(v_z[1],v_z[2])

# Prueba De Hipotesis Distribucion t-student
hipotesis_test_t <- function(samp, poblat, sglvl = 0.05) {
  x_bar <- mean(samp)
  cat("Media Muestral: ",x_bar,"\n")
  sd_samp <- sd(samp)
  cat("Desviacion Muestra: ",sd_samp,"\n")
  n <- length(samp)
  x_pob <- mean(poblat)
  t <- (x_bar - x_pob) / (sd_samp / sqrt(n))
  gl <- n - 1
  t_crit <- qt(1 - sglvl / 2, df = gl)
  p_valor <- 2 * (1 - pt(abs(t), df = gl))
  
  cat("t value: ", round(t, 4), "\n")
  cat("t crítico (bilateral, gl =", gl, "): ", round(t_crit, 4), "\n")
  cat("p-valor:", round(p_valor, 4), "\n")
  return(c(t, t_crit, gl))
}

verificar_hipotesis_t <- function(t, t_crit, gl, alpha = 0.05) {
  par(mfrow = c(1, 1))
  
  decision <- if (abs(t) > abs(t_crit)) "rechazada" else "no rechazada"
  
  cat("Estadístico t:", round(t, 3), "\n")
  cat("Valor crítico t:", round(t_crit, 3), "\n")
  cat("Decisión: Hipótesis nula", decision, "\n\n")
  
  x_vals <- seq(-qt(0.0001,gl), qt(0.0001,gl), length.out = 500)
  y_vals <- dt(x_vals, df = gl)
  
  plot(x_vals, y_vals, type = "l", lwd = 2, col = "black",
       main = paste0("Distribución t-Student (gl = ", gl, ")"),
       ylab = "Densidad", xlab = "t")
  
  abline(v = c(-t_crit, t_crit), col = "blue", lty = 2)
  abline(v = t, col = "red", lwd = 2)
  
  # Sombrear áreas críticas
  # Región crítica izquierda
  crit_izq <- x_vals <= -t_crit
  polygon(c(x_vals[crit_izq], rev(x_vals[crit_izq])),
          c(y_vals[crit_izq], rep(0, sum(crit_izq))),
          col = "red", border = NA, density = 50)
  
  # Región crítica derecha
  crit_der <- x_vals >= t_crit
  polygon(c(x_vals[crit_der], rev(x_vals[crit_der])),
          c(y_vals[crit_der], rep(0, sum(crit_der))),
          col = "red", border = NA, density = 50)
  
  legend("topright",
         legend = c("t observado", "t crítico", "Área crítica"),
         col = c("red", "blue", "red"), lty = c(1, 2, NA), pch = c(NA, NA, 15))
}

muestrat <-sample(datosHipotesis,20)
v_t <- hipotesis_test_t(muestrat,datosHipotesis)
verificar_hipotesis_t(v_t[1],v_t[2],v_t[3])

2*(1-pt(0.712,df=19))
