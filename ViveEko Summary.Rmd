---
title: "ViveEko Summary"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
---

```{r setup, include=FALSE}
library(flexdashboard)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyr)

setwd("ViveEko.xlsx")

viveeko <- read_excel("ViveEko.xlsx",sheet=1)

for(i in 1:nrow(viveeko)) {
  if(is.na(viveeko$Lugar[i])) {
    viveeko$Lugar[i] <- viveeko$Lugar[i-1]
    viveeko$Fecha[i] <- viveeko$Fecha[i-1]
  }
}

viveeko <- viveeko %>% group_by(Lugar,Fecha) %>% summarise(env_plastic=max(Envases),
                                                env_metal=min(Envases),
                                                ekopesos=max(Ekopesos,na.rm=T)) %>%
  mutate(env_plastic=ifelse(env_plastic==env_metal & grepl("METAL",env_plastic),0,env_plastic),
         env_metal=ifelse(env_plastic==env_metal & grepl("PLASTIC",env_metal),0,env_metal)) %>%
  mutate(env_plastic=gsub("PLASTIC|[:]|[[:blank:]]","",env_plastic),
         env_metal=gsub("METAL|[:]|[[:blank:]]","",env_metal)) %>%
  mutate(env_plastic=as.numeric(env_plastic),
         env_metal=as.numeric(env_metal))

vv_summ <- viveeko %>% ungroup() %>% arrange(Fecha)

i=2
while(i<nrow(viveeko)) {
  if(difftime(vv_summ$Fecha[i],vv_summ$Fecha[i-1],units="mins")<10) {
    vv_summ$env_plastic[i-1] <- vv_summ$env_plastic[i-1]+vv_summ$env_plastic[i]
    vv_summ$env_metal[i-1] <- vv_summ$env_metal[i-1]+vv_summ$env_metal[i]
    vv_summ$ekopesos[i-1] <- vv_summ$ekopesos[i-1]+vv_summ$ekopesos[i]
    if((i)<nrow(vv_summ)) {
      vv_summ <- vv_summ[c(1:(i-1),(i+1):nrow(vv_summ)),]
      }
    else {
      vv_summ <- vv_summ[c(1:(i-1)),]
      break
    }
  }
  else {i <- i+1}
}

```

Row {data-height=300}
-----------------------------------------------------------------------

### Metal cans

```{r}
max_val <- vv_summ %>% tidyr::pivot_longer(cols=3:4,names_to="material",values_to="cnt") %>%
  group_by(date=as.Date(paste0(substr(Fecha,1,7),"-01")),material) %>% 
  summarise(cnt=sum(cnt)) %>% {max(.$cnt)}

g <- vv_summ %>% tidyr::pivot_longer(cols=3:4,names_to="material",values_to="cnt") %>%
  filter(material=="env_metal") %>%
  group_by(date=as.Date(paste0(substr(Fecha,1,7),"-01")),material) %>% 
  summarise(cnt=sum(cnt)) %>%
  ggplot(aes(x=date,y=cnt))+geom_bar(stat="identity")+
  ylim(c(0,max_val))
ggplotly(g)
```


### Plastic bottles

```{r}

g <- vv_summ %>% tidyr::pivot_longer(cols=3:4,names_to="material",values_to="cnt") %>%
  filter(material=="env_plastic") %>%
  group_by(date=as.Date(paste0(substr(Fecha,1,7),"-01")),material) %>% 
  summarise(cnt=sum(cnt)) %>%
  ggplot(aes(x=date,y=cnt))+geom_bar(stat="identity")+
  ylim(c(0,max_val))
ggplotly(g)
```

Row {data-height=350}
-----------------------------------------------------------------------

### Ekopesos

```{r}
g <- vv_summ %>% ungroup() %>% arrange(Fecha) %>% mutate(sum_cum=cumsum(ekopesos)) %>%
  ggplot(aes(x=Fecha,y=sum_cum,group=1,
             text=paste0(substr(Fecha,1,7),": ",sum_cum," EkoPesos")))+
  geom_line(size=1.5,col="green")+
  ylab("Ekopesos")
ggplotly(g,tooltip="text")
```

### By place

```{r}
g <- vv_summ %>% group_by(Lugar) %>% summarise(ekopesos=sum(ekopesos)) %>%
  ggplot(aes(x=Lugar,y=ekopesos)) + geom_bar(stat="identity") +coord_flip()
ggplotly(g)
```

