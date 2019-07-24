setwd(dir = "W:/Odélia/my_project/Code")
getwd()

library(tidyverse)
library(ggplot2)
library(compareGroups)
library(mlogit)
library(FactoMineR)
library(questionr)
library(effects)
library(glmulti)


# Chargement des donnÃ©es
data = read.csv(file = "W:/Odélia/my_project/Code/my_database.csv", header = TRUE, row.names = NULL, sep = ';')
mydata = data[,c("Id","nepp3","datnaiss","datinclu","age0","sexe","anexam","periode","seul","csp","Adm11","actpro","Adm14","educ","Qm73")]


# Vérif des types et noms des var
str(mydata)

mydata$datnaiss = as.Date(mydata$datnaiss, format="%d/%m/%Y")
mydata$datinclu = as.Date(mydata$datinclu, format="%d/%m/%Y")
mydata$sexe = as.factor(mydata$sexe)
mydata$anexam = as.factor(mydata$anexam)
mydata$periode = as.factor(mydata$periode)
mydata$seul = as.factor(mydata$seul)
mydata$Adm11 = as.factor(mydata$Adm11)
mydata$actpro = as.factor(mydata$actpro)
mydata$Adm14 = as.factor(mydata$Adm14)
mydata$educ = as.factor(mydata$educ)
mydata$Qm73 = as.factor(mydata$Qm73)
mydata$csp = as.factor(mydata$csp)

str(mydata) 

colnames(mydata)
names(mydata)[names(mydata) == "Adm11"] <- "end_work_time"
names(mydata)[names(mydata) == "Adm14"] <- "educ_level"
names(mydata)[names(mydata) == "Qm73"] <- "srh"

colnames(mydata)

# Nouvelle variable : score de santé : vaut 1 si bonne santÃ© percue (au dessus de 5) 0 sinon 
srh_bin = 0
for (i in (1:10157)){
  if(is.na(mydata$srh[i]))
    srh_bin[i] = NA
  else if(((mydata$srh[i] == 0)||(mydata$srh[i] == 1)||(mydata$srh[i] == 2)||(mydata$srh[i] == 3)||(mydata$srh[i] == 4)||(mydata$srh[i] == 5)||(mydata$srh[i] == 6)||(mydata$srh[i] == 7)))
    srh_bin[i] = 0
  else
    srh_bin[i] = 1
}

mydata$srh_bin = srh_bin  
mydata$srh_bin = as.factor(mydata$srh_bin)


#https://www.fun-mooc.fr/asset-v1:UPSUD+42001+session10+type@asset+block/labs.html

#Gestion des donnÃ©es manquantes

## On transforme les cases vides par des NA

vide_par_na = function(data){
  for(i in dim(data)[1]){
    for (j in dim(data)[2]){
      if(data[i,j] == ""){
        data[i,j] = NA}
    }
  }
}

vide_par_na(mydata)

## Nombre total de données manquantes

nbr_tot_na = function(data){
  k = sum(is.na(data))
  return (k)
}

na_tot = nbr_tot_na(mydata)

## Nombre de donnÃ©es manquantes par variables

### Données manquantes de "seul"
na_seul = sum(is.na(mydata$seul))
id_na_seul = cbind(which(is.na(mydata$seul)))


### NA pour age
na_age = sum(is.na(mydata$age0))


### NA pour sexe
na_sexe = sum(is.na(mydata$sexe))


### NA pour csp
na_csp = sum(is.na(mydata$csp))
id_na_csp = cbind(which(is.na(mydata$csp)))

### NA pour end_work_time/act_pro
na_end_work_time = sum(is.na(mydata$end_work_time))
id_na_end_work_time = cbind(which(is.na(mydata$end_work_time)))
na_actpro = sum(is.na(mydata$actpro))
id_na_actpro = cbind(which(is.na(mydata$actpro)))

### NA pour educ level / educ
na_educ_level = sum(is.na(mydata$educ_level))
id_na_educ_level = cbind(which(is.na(mydata$educ_level)))
na_educ = sum(is.na(mydata$educ))
id_na_educ = cbind(which(is.na(mydata$educ)))

### NA pour srh/srh-score
na_srh = sum(is.na(mydata$srh))
id_na_srh = cbind(which(is.na(mydata$srh)))
na_rsh_bin = sum(is.na(mydata$rsh_bin))
id_na_rsh_bin = cbind(which(is.na(mydata$rsh_bin)))

## Exclusion des individus NA

### nbr individus Ã  exclure
id_a_exclure_NA = unique(rbind(id_na_actpro,id_na_csp,id_na_educ,id_na_educ_level,id_na_end_work_time,id_na_seul,id_na_srh,id_na_rsh_bin))
dim(id_a_exclure_NA)

### Données avec ind à exclure
exclus_NA = mydata[id_a_exclure_NA,]

table(exclus_NA$sexe)
ggplot(data = exclus_NA, aes(exclus_NA$age0)) + geom_histogram(col="red", fill="blue", alpha = .2)+ggtitle('Distribution de la variable `age` dans les donnÃ©es `exclus_NA`')
table(exclus_NA$seul)
table(exclus_NA$csp)
table(exclus_NA$actpro)
table(exclus_NA$educ)
table(exclus_NA$srh)

#Afficher un graph avec les valeurs manquantes vs les valeurs observÃ©es
#library(Amelia)
#missmap(mydata, col = c("red","black"),main = "Missing values vs observed")

#Données sans NA
## Exploration des donnÃ©es

data_no_NA = mydata[-id_a_exclure_NA,]

table(data_no_NA$sexe)
ggplot(data = data_no_NA, aes(data_no_NA$age0)) + geom_histogram(col="red", fill="blue", alpha = .2)+ggtitle('Distribution de la variable `age` dans les donnÃ©es `data_no_NA`')
ggplot(data=data_no_NA, aes(log(data_no_NA$age0))) + geom_histogram(col="red", fill="blue", alpha = .2)+ggtitle('Distribution du log de la variable `age` dans les donnÃ©es `data_no_NA`')

table(data_no_NA$seul)
table(data_no_NA$csp)
table(data_no_NA$actpro)
table(data_no_NA$educ)
table(data_no_NA$srh)

# Gesion des outliers
out_age = boxplot.stats(data_no_NA$age0)$out
out_age_idx = which(data_no_NA$age0 %in% c(out_age))


# Données propres
dataClean = data_no_NA[-out_age_idx,]


#On teste si les données exclus et dataClean ont la même structure pour chaque variable

exclus = mydata[c(out_age_idx,id_a_exclure_NA),]

## Nouvelle variable groupe qui vaut 0 si exclus et 1 sinon
exclus$groupe = 0
dataClean$groupe = 1

data_test = rbind(exclus,dataClean)

khi_test = compareGroups(groupe ~ age0 + sexe + seul + csp + end_work_time +actpro + educ_level + educ + srh_bin + srh , data = data_test, method = c(age0 = NA, rsh_bin = 3, srh = 3),max.xlev = 15, alpha = 0.01)

res_khi_test = createTable(khi_test)
summary(res_khi_test)
res_khi_test
plot(res_khi_test[1])
export2word(res_khi_test, file='khi_test.docx')

# Analyse

##Exploration des données
### On travaille dans dataClean avec  variables 10(dont 2 à virer) : age, sexe, seul, csp, end_work_time, actpro, educ_level, educ, srh, rsh_bin

ggplot(dataClean, aes(x = srh, y = age0)) + geom_boxplot(aes(fill = srh))+ ggtitle("Lien srh et age 0")
ggplot(dataClean, aes(x = srh_bin, y = age0)) + geom_boxplot(aes(fill = srh_bin))+ ggtitle("Lien srh_bin et age0")

 
table(dataClean$srh, dataClean$actpro)
chisq.test(dataClean$srh, dataClean$actpro,simulate.p.value = TRUE)
table(dataClean$rsh_bin, dataClean$actpro)
chisq.test(dataClean$actpro, dataClean$rsh_bin,simulate.p.value = TRUE)

table(dataClean$srh, dataClean$sexe)
chisq.test(dataClean$srh, dataClean$sexe,simulate.p.value = TRUE)
table(dataClean$rsh_bin, dataClean$sexe)
chisq.test(dataClean$rsh_bin,dataClean$sexe,simulate.p.value = TRUE)

table(dataClean$srh, dataClean$educ)
chisq.test(dataClean$srh, dataClean$educ,simulate.p.value = TRUE)
table(dataClean$rsh_bin, dataClean$educ)
chisq.test(dataClean$rsh_bin, dataClean$educ,simulate.p.value = TRUE)

table(dataClean$srh, dataClean$csp)
chisq.test(dataClean$srh, dataClean$csp,simulate.p.value = TRUE)
table(dataClean$rsh_bin, dataClean$csp)
chisq.test(dataClean$rsh_bin, dataClean$csp,simulate.p.value = TRUE)

table(dataClean$srh, dataClean$seul)
chisq.test(dataClean$srh, dataClean$seul,simulate.p.value = TRUE)
table(dataClean$rsh_bin, dataClean$seul)
chisq.test(dataClean$rsh_bin, dataClean$seul,simulate.p.value = TRUE)

ggplot(dataClean, aes(x = actpro, y = age0)) + geom_boxplot(aes(fill = actpro))+ ggtitle("Lien actpro et age 0")
ggplot(dataClean, aes(x = educ, y = age0)) + geom_boxplot(aes(fill = educ))+ ggtitle("Lien educ et age 0")
ggplot(dataClean, aes(x = seul, y = age0)) + geom_boxplot(aes(fill = seul))+ ggtitle("Lien seul et age 0")
