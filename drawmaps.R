drawMAPDeath <-function()
{
  
  setwd("c:/users/praba.siva/Documents/2017/ML/usprescriptions")
  
  library(dplyr)
  library(magrittr)
  library(ggplot2)
  library(maps)
  library(data.table)
  
  
all_states <- map_data("state")
od <- read.csv("overdoses.csv",stringsAsFactors = FALSE)
od$State <- as.factor(od$State)
od$Population <- as.numeric(gsub(",","",od$Population))
od$Deaths<- as.numeric(gsub(",","",od$Deaths))




od %>%
  mutate(state.lower=tolower(State), Population=as.numeric(Population)) %>%
  merge(all_states,by.x="state.lower",by.y="region") %>%
  select(-subregion,-order) %>% 
 # ggplot() + geom_map(map=all_states, aes(x=long, y=lat, map_id=state.lower,fill=Deaths) )  + ggtitle("U.S. Opiate Overdose Death") +
   ggplot() + geom_map(map=all_states, aes(x=long, y=lat, map_id=state.lower,fill=Population) )  + ggtitle("Populations") +
    geom_text(data=data.frame(state.center,od$Abbrev),aes(x=x, y=y,label=od.Abbrev),size=3) +
  scale_fill_continuous(low='lightblue', high='red',
                        guide=guide_colorbar(ticks=FALSE,barheight=1,barwidth=10,title.vjust=.8,values=c(0.2,0.3)),
                        name="Total Deaths") +
  theme(axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank(),
        legend.position="bottom",plot.title=element_text(size=20))

}

drawMAPDRate <-function()
{
  
  setwd("c:/users/praba.siva/Documents/2017/ML/usprescriptions")
  
  library(dplyr)
  library(magrittr)
  library(ggplot2)
  library(maps)
  library(data.table)
  
  
  all_states <- map_data("state")
  od <- read.csv("overdoses.csv",stringsAsFactors = FALSE)
  od$State <- as.factor(od$State)
  od$Population <- as.numeric(gsub(",","",od$Population))
  od$Deaths<- as.numeric(gsub(",","",od$Deaths))
  
  
od %>%
  mutate(state.lower=tolower(State), Population=as.numeric(Population)) %>%
  merge(all_states,by.x="state.lower",by.y="region") %>%
  select(-subregion,-order) %>% 
  ggplot() + geom_map(map=all_states, aes(x=long, y=lat, map_id=state.lower,fill=Deaths/Population*1e6) )  + ggtitle("U.S. Opiate Overdose Death Rate") +
  geom_text(data=data.frame(state.center,od$Abbrev),aes(x=x, y=y,label=od.Abbrev),size=3) +
  scale_fill_continuous(low='lightblue', high='red',
                        guide=guide_colorbar(ticks=FALSE,barheight=1,barwidth=10,title.vjust=.8,values=c(0.2,0.3)),
                        name="Deaths per Million") +
  theme(axis.text=element_blank(),axis.title=element_blank(),axis.ticks=element_blank(),
        legend.position="bottom",plot.title=element_text(size=20))



}

drawMaps <- function()
{
#drawing two maps from a R function is not working
#run these function separately to get the map
  
    drawMAPDRate();
  drawMAPDeath();
}
