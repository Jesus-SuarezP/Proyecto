# Proyecto red de comercio crudo----

# Importacion de la base de datos----

library(readxl)

ruta <- file.path("C:", "Users", "jesus", "Escritorio", "Universidad", 
                  "Topicos Avanzados", 
                  "Analisis Estadistico de redes sociales (Social Networks)", 
                  "Proyecto", "Datos proyecto.xlsx")

aristas <- as.data.frame(read_excel(ruta, sheet = "Aristas"))
aristas
class(aristas)

atributos_nodos <- as.data.frame(read_excel(ruta, sheet = "Atributos_nodos"))
atributos_nodos

library(igraph)
# Convertir el data frame en una matriz de aristas
matriz_aristas <- as.matrix(aristas[, c("Origen", "Destino")])

red <- graph_from_edgelist(matriz_aristas, directed = TRUE)

# Asignar pesos a las aristas
E(red)$weight <- aristas$Valor
E(red)$weight
is_weighted(red)

red
V(red)$name
vcount(red)
ecount(red)

# Agregar atributos a los nodos----

#PIB
# Asignar el PIB a los nodos usando los nombres de los países como índice
V(red)$PIB <- atributos_nodos$PIB[match(V(red)$name, atributos_nodos$Paises)]

#verificacion
nodo <- V(red)[7]
cat("Nombre:", V(red)$name[nodo], "- PIB:", V(red)$PIB[nodo], "\n")

#Energia
# Asignar %energia renobable
V(red)$Energia <- atributos_nodos$Energia[match(V(red)$name, 
                                        atributos_nodos$Paises)]
#verificacion
nodo <- V(red)[7]
cat("Nombre:", V(red)$name[nodo], "- Energia:", V(red)$Energia[nodo], "\n")

#Riesgo
# Asignar riesgo
V(red)$Riesgo <- atributos_nodos$Riesgo[match(V(red)$name, 
                                                atributos_nodos$Paises)]
#verificacion
nodo <- V(red)[7]
cat("Nombre:", V(red)$name[nodo], "- Riesgo:", V(red)$Riesgo[nodo], "\n")

# Region
# Asignar region
V(red)$Region <- atributos_nodos$Region[match(V(red)$name, 
                                                atributos_nodos$Paises)]
#verificacion
nodo <- V(red)[7]
cat("Nombre:", V(red)$name[nodo], "- Region:", V(red)$Region[nodo], "\n")

# Idioma
# Asignar idioma
V(red)$Idioma <- atributos_nodos$Idioma[match(V(red)$name, 
                                              atributos_nodos$Paises)]
# verificacion
nodo <- V(red)[7]
cat("Nombre:", V(red)$name[nodo], "- Idioma:", V(red)$Idioma[nodo], "\n")


# Modelos ERGM ----

library(ergm)
library(network)
library(intergraph)

# Convertir el objeto igraph a un objeto network
net <- asNetwork(red)
net
class(net)

# Modelo 1. Básico ----

# formulación del modelo
ergm_model1<- formula(net ~ edges)
summary(ergm_model1)
# ajuste del modelo
set.seed(42)
ergm1_fit <- ergm(formula = ergm_model1)
summary(ergm1_fit)
# probabilidad
expit <- function(x) 1/(1+exp(-x))
expit(-3.08329)
# anova
anova(ergm1_fit)


# Modelo 2. Variables cuantitativas (efecto principal) ----

# formulación del modelo
ergm_model2<- formula(net ~ edges + nodemain('PIB') 
                      + nodemain('Energia') + nodemain('Riesgo'))
summary(ergm_model2)
# ajuste del modelo
set.seed(42)
ergm2_fit <- ergm(formula = ergm_model2)
summary(ergm2_fit)
# anova
anova(ergm2_fit)

# Modelo 3. Variables cuantitativas (efecto principal) y PIB absdiff ----

# formulación del modelo
ergm_model3<- formula(net ~ edges + nodemain('PIB') + absdiff('PIB')
                      + nodemain('Energia') + nodemain('Riesgo'))
summary(ergm_model3)
# ajuste del modelo
set.seed(42)
ergm3_fit <- ergm(formula = ergm_model3)
summary(ergm3_fit)
# anova
anova(ergm3_fit)

# Modelo 4. ----
# Efecto principal (PIB, Riesgo)
# Efecto homofilia (PIB, Energia)

# formulación del modelo
ergm_model4<- formula(net ~ edges + nodemain('PIB') + absdiff('PIB')
                      + absdiff('Energia') + nodemain('Riesgo'))
summary(ergm_model4)
# ajuste del modelo
set.seed(42)
ergm4_fit <- ergm(formula = ergm_model4)
summary(ergm4_fit)
# anova
anova(ergm4_fit)

# Modelo 5. ----
# Efecto principal (PIB, Energia)
# Efecto homofilia (PIB, Riesgo)

# formulación del modelo
ergm_model5<- formula(net ~ edges + nodemain('PIB') + absdiff('PIB')
                      + nodemain('Energia') + absdiff('Riesgo'))
summary(ergm_model5)
# ajuste del modelo
set.seed(42)
ergm5_fit <- ergm(formula = ergm_model5)
summary(ergm5_fit)
# anova
anova(ergm5_fit)

# Modelo 6. ----
# Efecto principal (PIB, Energia)
# Efecto homofilia (PIB, Region)

# formulación del modelo
ergm_model6<- formula(net ~ edges + nodemain('PIB') + absdiff('PIB')
                      + nodemain('Energia') + nodematch('Region'))
summary(ergm_model6)
# ajuste del modelo
set.seed(42)
ergm6_fit <- ergm(formula = ergm_model6)
summary(ergm6_fit)
# anova
anova(ergm6_fit)

# Modelo 7. ----
# Efecto principal (PIB, Energia)
# Efecto homofilia (PIB, Region, Idioma)

# formulación del modelo
ergm_model7<- formula(net ~ edges + nodemain('PIB') + absdiff('PIB')
                      + nodemain('Energia') + nodematch('Region')
                      + nodematch('Idioma'))
summary(ergm_model7)
# ajuste del modelo
set.seed(42)
ergm7_fit <- ergm(formula = ergm_model7)
summary(ergm7_fit)
# anova
anova(ergm7_fit)

# Modelo 8. ----
# Efecto principal (PIB, Energia, Riesgo)
# Efecto homofilia (PIB, Region, Idioma)

# formulación del modelo
ergm_model8<- formula(net ~ edges + nodemain('PIB') + absdiff('PIB')
                      + nodemain('Energia') + nodemain('Riesgo')
                      + nodematch('Region') + nodematch('Idioma'))
summary(ergm_model8)
# ajuste del modelo
set.seed(42)
ergm8_fit <- ergm(formula = ergm_model8)
summary(ergm8_fit)
# anova
anova(ergm8_fit)

# Bondad de Ajuste ----
# mejor modelo: modelo 7
?gof

set.seed(39)
gof.ergm_model7 <- gof(ergm7_fit, control = control.gof.ergm(nsim = 1000))
print(gof.ergm_model7)
plot(gof.ergm_model7)


