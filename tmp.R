---
  title: "R Notebook"
output: html_notebook
---
  
  ```{r}
library(FactoMineR)
```
```{r}
baseline$Adm10 = as.factor(baseline$Adm10)
baseline$Adm11 = as.factor(baseline$Adm11)
baseline$Adm12 = as.factor(baseline$Adm12)
baseline$Adm12a = as.factor(baseline$Adm12a)
```
```{r}
na_Adm10_id = baseline$nepp3[which(is.na(baseline$Adm10))]
na_Adm11_id = baseline$nepp3[which(is.na(baseline$Adm11))]
na_Adm12_id = baseline$nepp3[which(is.na(baseline$Adm12))]
na_Adm12a_id = baseline$nepp3[which(is.na(baseline$Adm12a))]
```

```{r}
na_actproBl_id = unique(c(na_Adm10_id,na_Adm11_id,na_Adm12_id,na_Adm12a_id))
```

```{r}
actproBl = baseline[-which(baseline$nepp3 %in% na_actproBl_id),c(25,27,29,30) ]
```

```{r}

ACM_activpro = MCA(actproBl)
```

```{r}
colnames(actproBl)
```
```{r}
library(FactoMineR)
cah_activpro = HCPC(ACM_activpro,graph = FALSE)
```

```{r}
plot(cah_activpro, choice = "tree")
```
```{r}
plot(cah_activpro, choice = "3D.map")
```
```{r}
plot(cah_activpro, choice = "bar")
```
```{r}
plot(cah_activpro, choice = "map")
```
```{r}
actproBl$cah_activpro = cah_activpro$data.clust$clust
```

```{r}
actproBl$activpro = baseline$activpro[-which(baseline$nepp3 %in% na_actproBl_id)]
```

```{r}
actproBl$activpro[1:10]
actproBl$cah_activpro[1:10]
```

```{r}
actproBl$activpro[84:117]
actproBl$cah_activpro[84:117]
```
4 = retraite
1 = travail
2 = chomeur
3 = inactif

```{r}
for (z in 1:(dim(actproBl)[1])){
  if(actproBl$cah_activpro[z] == 1)
    actproBl$cah_activpro2[z] = "T"
  else if(actproBl$cah_activpro[z] == 2)
    actproBl$cah_activpro2[z] = "C"
  else if(actproBl$cah_activpro[z] == 3)
    actproBl$cah_activpro2[z] = "I"
  else if(actproBl$cah_activpro[z] == 4)
    actproBl$cah_activpro2[z] = "R"
  
}


```
```{r}
length(which(actproBl$activpro != actproBl$cah_activpro2))
```

```{r}
(length(which(actproBl$activpro != actproBl$cah_activpro2))/(dim(actproBl)[1]))*100
```
```{r}
actproBl$cah_activpro2 = as.character(actproBl$cah_activpro2)
```

```{r}
summary(actproBl$activpro[which(actproBl$activpro != actproBl$cah_activpro2)])
length(actproBl$cah_activpro2[which(actproBl$activpro != actproBl$cah_activpro2 & actproBl$activpro == "C")])
```

```{r}
actproBl$activpro[which(actproBl$activpro == "NSP")] = NA
str(actproBl$activpro)
str(actproBl$cah_activpro2)
```

