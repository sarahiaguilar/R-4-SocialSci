library(data.table)

# Carga csv con principales resultados por localidad como data.table en objeto dt
dt <- fread("./datos/iter_00_cpv2020/conjunto_de_datos/conjunto_de_datos_iter_00CSV20.csv",
            encoding = "UTF-8")

# Carga diccionario de csv con principales resultados por localidad como data.table en objeto dt
diccionario <- fread("./datos/iter_00_cpv2020/diccionario_datos/diccionario_datos_iter_00CSV20.csv",
                     encoding = "UTF-8")

# Imprimir dimensiones de dt
print(dim(dt))

# Ver las primeras 200 observaciones de st
View(head(dt, n = 200))



# Porcentaje de población total con respecto al país completo -------------

# Instancia nuevo data.table en objeto pobtot_ent
# filtrando observaciones cuyo valor en la variable NOM_LOC sea "Total de la Entidad"
# y conservando únicamente las variables NOM_ENT y POBTOT
pobtot_ent <- dt[NOM_LOC == "Total de la Entidad", .(NOM_ENT, POBTOT)]

# Instancia nuevo objeto la suma de toda la variable POBTOT
pobtot_mx <- sum(pobtot_ent$POBTOT)

# Crea nueva variable en pobtot_ent llamada pct_pobtot
# con el porcentaje de población total por entidad con respecto al país completo
pobtot_ent[, pct_pobtot := (POBTOT/pobtot_mx)*100]

# Redondea pct_pobtot en pobtot_ent a un dígito decimal
pobtot_ent[, pct_pobtot := round(pct_pobtot, digits = 1)]

# Orderna pobtot_ent con respecto a variable POBTOT de forma descendente 
pobtot_ent <- pobtot_ent[order(-POBTOT), ]

# Crea nueva variable que categoriza pct_pobtot en rangos (1ra forma; no óptima)
pobtot_ent[, pct_pobtot_cat := NA]
pobtot_ent[, pct_pobtot_cat := ifelse(pct_pobtot <=  1.9, yes = "menos de 1.9", no = pct_pobtot_cat)]
pobtot_ent[, pct_pobtot_cat := ifelse(pct_pobtot >  1.9 & pct_pobtot <= 3.3, yes = "más de 1.9 a 3.3", no = pct_pobtot_cat)]
pobtot_ent[, pct_pobtot_cat := ifelse(pct_pobtot >  3.3 & pct_pobtot <= 5.2, yes = "más de 3.3 a 5.2", no = pct_pobtot_cat)]
pobtot_ent[, pct_pobtot_cat := ifelse(pct_pobtot >  5.2 & pct_pobtot <= 6.6, yes = "más de 5.2 a 6.6", no = pct_pobtot_cat)]
pobtot_ent[, pct_pobtot_cat := ifelse(pct_pobtot >  6.6, yes = "más de 6.6", no = pct_pobtot_cat)]

# Crea nueva variable que categoriza pct_pobtot en rangos (2da forma; más optima)
pct_pobtot_categorias <- cut(pobtot_ent$pct_pobtot, 
                             breaks = c(0, 1.9, 3.3, 5.2, 6.6, Inf),
                             labels = c("menos de 1.9", "más de 1.9 a 3.3", "más de 3.3 a 5.2", "más de 5.2 a 6.6", "más de 6.6"))
pobtot_ent[, pct_pobtot_cat := pct_pobtot_categorias]



# Porcentaje de población de mujeres con respecto al país completo --------

# Instancia nuevo data.table en objeto pobtot_ent_m
# filtrando observaciones cuyo valor en la variable NOM_LOC sea "Total de la Entidad"
# y conservando únicamente las variables POBFEM y POBTOT
pobtot_ent_m <- dt[NOM_LOC == "Total de la Entidad", .(NOM_ENT, POBFEM)]

# R marca error al ejecutar la siguiente línea porque R interpretó la variable POBFEM como caracter
# dado que algunas observaciones contenían asteriscos en dicha variable 
pobtot_mx_m <- sum(pobtot_ent_m$POBFEM)

# Imprime estructura de pobtot_ent_m
str(pobtot_ent_m)

# Convierte variable POBFEM en numerica
pobtot_ent_m[, POBFEM := as.integer(POBFEM)]

# Instancia nuevo objeto la suma de toda la variable POBFEM
pobtot_mx_m <- sum(pobtot_ent_m$POBFEM)

# Crea nueva variable en pobtot_ent_m llamada pobtot_ent_m
# con el porcentaje de población de mujeres por entidad con respecto al país completo
pobtot_ent_m[, pct_pobtot_m := (POBFEM/pobtot_mx_m)*100]

# Redondea pct_pobtot_m en pobtot_ent_m a un dígito decimal
pobtot_ent_m[, pct_pobtot_m := round(pct_pobtot_m, digits = 1)]

# Orderna pobtot_ent_m con respecto a variable POBFEM de forma descendente 
pobtot_ent_m <- pobtot_ent_m[order(-POBFEM), ]



# Porcentaje de población de mujeres con respecto a la población total ----

# Une pobtot_ent y pobtot_ent_m
pobtot_ent <- merge(x = pobtot_ent,
                    y = pobtot_ent_m,
                    by = "NOM_ENT")

# Crea nueva variable en pobtot_ent llamada pct_m
# con el porcentaje de población de mujeres por entidad con respecto a la entidad
pobtot_ent[, pct_m := POBFEM/POBTOT]



# Porcentaje de población de hombres con respecto al país completo --------
# Ejercicio #1
# Instancie nuevo data.table en objeto pobtot_ent_h
# que contenga una variable con el porcentaje de población de mujeres por entidad con respecto al país completo



# Porcentaje de población de hombres con respecto a la población total ----
# Ejercicio #2
# Una obtot_ent y pobtot_ent_h
# y cree nueva variable en pobtot_ent llamada pct_h
# con el porcentaje de población de hombres por entidad con respecto a la entidad



# Relación hombres mujeres ------------------------------------------------
# Ejercicio #3
# Cree una nueva variable en pobtot_ent_h con el número de hombres por cada 100 mujeres por entidad




# Tamaño de localidad promedio --------------------------------------------
# Instancia nuevo data.table en objeto pobtot_ent_m
# filtrando observaciones cuyo valor en la variable NOM_LOC sea "Total de la Entidad"
# y conservando únicamente las variables POBFEM y POBTOT
tamloc_loc <- dt[!LOC %in% c(0, 1, 998, 9999) & NOM_ENT != "Total nacional", .(NOM_ENT, NOM_LOC, POBTOT)]

# Instancia nuevo data.table con variable prom_tamloc con el promedio de POBTOT por entidad
prom_tamloc_ent <- tamloc_loc[, .(prom_tamloc = mean(POBTOT)), by = NOM_ENT]




# Procentaje promedio de viviendas habitadas ------------------------------
# Ejercicio #4 
# Cree un nuevo data.table que contenga a nivel entidad el porcentaje promedio de viendas habitadas por localidad

