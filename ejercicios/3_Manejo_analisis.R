#Este script manipula datos y hace analysis preliminares.

# volvemos a james bond
load("ejercicios/data/bond.R")

#Merging datasets--------

#tenemos una segunda tabla evaluando cuan guapo es cada actor.
howpretty <- data.frame(Actor_principal = unique(bond$Actor_principal), 
                        pretty = c("feo", "resulton", "guaperas",
                                   "feo", "resulton", "guaperas"))
#alternativamente escala 1:10
howpretty <- data.frame(Actor_principal = unique(bond$Actor_principal), 
                        pretty = round(runif(6)*10, digits = 0))

howpretty

#merge
bond <- merge(bond, howpretty)
bond <- merge(bond, howpretty, by = "Actor_principal")
head(bond)

#Reshape!-----

# Hay veces que queremos transformar datos y table() se queda corto.
install.packages("reshape2")
library(reshape2)

#Basically, you "melt" data so that each row is a unique id-variable combination. 
#Then you "dcast" the melted data into any shape you would like. 
#Here is a very simple example.

#melt() crea datos "tidy". Todos vuestros datos habrian de ser tidy.
#ejercicio: hacer bond "tidy" para los rankings
#uno: subset las columnas que nos interesan
colnames(bond)
bond_tidy <- subset(bond, select = c("Actor_principal", "Title", "Year", "imdbRating", 
                                     "tomatoMeter", "tomatoRating", "tomatoRotten",
                                     "tomatoUserMeter", "tomatoUserRating"))
head(bond_tidy)
#dos: melt
bond_melted <- melt(bond_tidy, id.vars =  c("Actor_principal", "Title", "Year"), 
                    variable.name = "Rating")
head(bond_melted)

#and cast
head(bond_melted)
dcast(bond_melted, Actor_principal ~ Rating, value.var = "value", max)
dcast(bond_melted, Actor_principal ~ Rating, value.var = "value", mean)

#vamos reducir el numero de actores con una categoria "otros" para 
#los que tengan pocas pelis (table, levels)
table(bond$Actor_principal)
levels(bond$Actor_principal)
bond$Actor_p2 <- as.factor(bond$Actor_p)
levels(bond$Actor_p2)[2] <- "other"
levels(bond$Actor_p2)[6] <- "other"

#bucles y condicionales----

#condicionales (if else)
if(sum(1:4) == 10) {
  print("right!")
} else {
  print("wrong!")
}
#bucles (for)
for(i in 1:4) { print(i) }


#Plot todas las lineas de tendencia:

boxplot(bond$imdbRating, bond$tomatoUserRating*2, las = 1, 
        names = c("IMDB", "Tomatoes"),
        main = "Rating")
#una linea
lines(x = c(1,2), y = c(bond$imdbRating[1], bond$tomatoUserRating[1]*2))
for(i in 1:nrow(bond)){
  lines(x = c(1,2), y = c(bond$imdbRating[i], bond$tomatoUserRating[i]*2),
        col = i)
}

#PCA----

head(bond_tidy)
pairs(bond_tidy[,4:9])
cor(bond_tidy[,4:9])

pca <- prcomp(x = bond_tidy[,4:9])
pca
summary(pca)
biplot(pca)

pca <- prcomp(x = bond_tidy[,4:9], center = TRUE, scale. = TRUE)
pca
summary(pca)
biplot(pca)

#nicer plot
library(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1,
              groups = bond_tidy$Actor_principal, ellipse = TRUE,
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)




