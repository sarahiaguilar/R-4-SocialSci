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

# Instancia nuevo data.table en objeto pobtot_ent
# filtrando observaciones cuyo valor en la variable NOM_LOC sea "Total de la Entidad"
# y conservando únicamente las variables NOM_ENT y POBTOT
pobtot_ent <- dt[NOM_LOC == "Total de la Entidad", .(ENTIDAD, NOM_ENT, POBTOT, POBFEM, POBMAS)]

# Instancia nuevo objeto la suma de toda la variable POBTOT
pobtot_mx <- sum(pobtot_ent$POBTOT)

# Crea nueva variable en pobtot_ent llamada pct_pobtot
# con el porcentaje de población total por entidad con respecto al país completo
pobtot_ent[, pct_pobtot := (POBTOT/pobtot_mx)*100]

# Redondea pct_pobtot en pobtot_ent a un dígito decimal
pobtot_ent[, pct_pobtot := round(pct_pobtot, digits = 1)]

# Crea nueva variable que categoriza pct_pobtot en rangos
pct_pobtot_categorias <- cut(pobtot_ent$pct_pobtot, 
                             breaks = c(0, 1.9, 3.3, 5.2, 6.6, Inf),
                             labels = c("menos de 1.9", "más de 1.9 a 3.3", "más de 3.3 a 5.2", "más de 5.2 a 6.6", "más de 6.6"))
pobtot_ent[, pct_pobtot_cat := pct_pobtot_categorias]

# Convierte en entero POBFEM y POBMAS
pobtot_ent[, POBFEM := as.integer(POBFEM)]
pobtot_ent[, POBMAS := as.integer(POBMAS)]

# Crea nueva variable en pobtot_ent llamada pct_pobtot
# con el porcentaje de población femenina por entidad 
pobtot_ent[, pct_popfem := POBFEM/POBTOT*100]

# Crea nueva variable en pobtot_ent llamada pct_pobtot
# con el porcentaje de población masculina por entidad 
pobtot_ent[, pct_popmas := POBMAS/POBTOT*100]

# Crea nueva variable en pobtot_ent llamada pct_pobtot
# con el número de hombres por cada 100 mujeres 
pobtot_ent[, pct_massobrefem := POBMAS/POBFEM*100]

# Orderna pobtot_ent con respecto a variable POBTOT de forma descendente 
pobtot_ent <- pobtot_ent[order(-ENTIDAD), ]

pobtot_ent[, ENTIDAD_NUM := ifelse(ENTIDAD < 10, yes = paste0(0, ENTIDAD), no = ENTIDAD)]

write.csv(pobtot_ent, "./datos/tabulados_censo2020.csv", row.names = FALSE)
