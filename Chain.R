library(tidyverse)
library(ggplot2)
library("readxl")
library(dplyr)
library(ggpubr)
library(ggrepel)
library(ggsoccer)
library(gghighlight)
library(standardize)
library(RColorBrewer)
library(viridis)
library(ggrepel)
library(ggforce)
library(factoextra)

#importing the data
maindata <- read_excel("UCL.xlsx", sheet = 1)
maindata <- maindata %>% filter(!is.na(playerId))
PassData <- maindata %>% filter(`type/displayName` == 'Pass')
playerlegend <- read_excel("UCL.xlsx", sheet = 2)

#testing for any trickery!
test <- maindata %>% filter(endX>100)

#putting in the names of the players in the main data-set
for(i in 1:nrow(playerlegend)){
  for(j in 1:nrow(maindata)){
    if(maindata$playerId[j] == playerlegend$playerId[i]){
      maindata$playerId[j] = playerlegend$playerKey[i]
    }
  }
}

#putting in the names of the players in the passing data-set
for(i in 1:nrow(playerlegend)){
  for(j in 1:nrow(PassData)){
    if(PassData$playerId[j] == playerlegend$playerId[i]){
      PassData$playerId[j] = playerlegend$playerKey[i]
    }
  }
}

#next stop cutting it off by team - in this case our team of Borussia Dortmund
team1 <- "Chelsea"

#only having successful pasess, selecting the relevant columns, and making the receiverID column complete
Team1 <- PassData %>% filter(teamId == team1) %>% 
  filter(`outcomeType/displayName` == 'Successful') %>% 
  mutate(receiverId = playerId) %>%
  select("id","eventId","minute","second","teamId","x","y",
         "period/value","period/displayName","type/value","type/displayName",
         "outcomeType/value","outcomeType/displayName","isTouch","playerId","receiverId",
         "endX","endY","isGoal","isShot")

#inputting the receivers set of the data so that each passer has a recipient
for(i in 1:nrow(Team1)){
  if(i != 1){
    Team1$receiverId[(i-1)] = Team1$playerId[i]
  }
}


#Implementing Actual Pass-Chains

#adding in the receiver data to the main data frame
MainData <- maindata %>% mutate(receiverId = "")  %>%
  select("id","eventId","minute","second","teamId","x","y",
         "period/value","period/displayName","type/value","type/displayName",
         "outcomeType/value","outcomeType/displayName","isTouch","playerId","receiverId",
         "endX","endY","isGoal","isShot")

MainData <- merge(x=MainData, y=Team1[,c("receiverId","id")], by="id", all.x=TRUE)

MainData <- MainData %>% select(-receiverId.x)

MainData <- MainData %>% arrange(Matchweek)

TableauData <- data.frame("x"=0,"y"=0,"endX"=0,"endY"=0, "Cluster"=0, "order"="", "count" = 0, "direct" = 0)

PassChain <- data.frame("playerId"="","receiverId"="","x"=0,
                          "endX"=0,"y"=0,"endY"=0,"group" = 0,
                          "order"="","type" = "", "minute" = 0, "sec" = 0,
                        "direct" = 0,"team" = "", "Matchweek" = "", "Opponent" = "")

#adding in directness metrics to the pass-chain dataframe
MainData <- MainData %>% mutate(OPD = endX-x)
MainData <- MainData %>% mutate(length = sqrt( ((endX-x)*(endX-x))+((endY-y)*(endY-y)) ) )
MainData <- MainData %>% mutate(directness = OPD/length)
  
#Implementing Pass-Chains - grouping passes through respective filters
grouping = 0
  for(j in 1:nrow(MainData)){
    
    if(#(MainData$playerId[j] %in% c("Mason Mount")) &&
       !is.na(MainData$endX[j]) &&
       (MainData$endX[j]>=(83)) &&
       !is.na(MainData$endY[j]) &&
       (MainData$endY[j]>=63.2 && MainData$endY[j]<=78.9) && 
       MainData$`outcomeType/displayName`[j]=="Successful" &&
       MainData$teamId[j]=="Chelsea"){
      
      if(j+3<nrow(MainData)){
        
        boolean_chain = 0
        period_prop = as.numeric(c())
        for(b in 3:0){
          if(MainData$teamId[j+b] =="Chelsea"){
            
            if(MainData$`outcomeType/displayName`[j+b] == "Successful" &&
               MainData$`type/displayName`[j+b] != "Foul" &&
               MainData$isTouch[j+b] == TRUE){
              
                period_prop = as.numeric(c(period_prop,MainData$`period/value`[j+b]))
                num = as.numeric(sum(period_prop==1))
                num2 = as.numeric(sum(period_prop==2))
                if((num/length(period_prop))==1 || (num2/length(period_prop))==1){
                  boolean_chain = boolean_chain+1
                }
            }
          }
        }
        
        
    
        if(boolean_chain == 4){
          for(i in 0:3){
            if(i==0){
              groupdivider = "A"
            }
            else if(i==1){
              groupdivider = "B"
            }
            else if(i==2){
              groupdivider = "C"
            }
            else if(i==3){
              groupdivider = "D"
            }
            #else if(i==0){
             # groupdivider = "E"
            #}
            x <- c(MainData$playerId[j+i],MainData$receiverId[j+i],MainData$x[j+i],
                   MainData$endX[j+i],MainData$y[j+i],MainData$endY[j+i],grouping,
                   groupdivider, MainData$`type/displayName`[j+i], MainData$minute[j+i],
                   MainData$second[j+i],MainData$directness[j+i],
                   MainData$teamId[j+i],MainData$Matchweek[j+i],MainData$Opponent[j+i])
            PassChain <- rbind(PassChain, x)
            
          }
          grouping = grouping + 1
        }
      }
    }
  }
  
  #formatting the pass-chain dataset to get rid of blank rows and converting
  #the necessary columns to numeric
  PassChain <- PassChain %>% filter(playerId != "")
  PassChain$x <- as.numeric(PassChain$x)
  PassChain$endX <- as.numeric(PassChain$endX)
  PassChain$y <- as.numeric(PassChain$y)
  PassChain$endY <- as.numeric(PassChain$endY)
  PassChain$direct <- as.numeric(PassChain$direct)
  PassChain$group <- as.factor(PassChain$group)
  
  #Preparing the dataset so that we can apply KMC to the passes
  kdata <- data.frame("x1"=0,"y1"=0,"xe1"=0,"ye1"=0,
                      "x2"=0,"y2"=0,"xe2"=0,"ye2"=0,
                      "x3"=0,"y3"=0,"xe3"=0,"ye3"=0,
                      "x4"=0,"y4"=0,"xe4"=0,"ye4"=0)
  
  #Concatenating the passes into a single observation
  j=1
  cluster <- c()
  for(j in 1:nrow(PassChain)){
    if(is.na(PassChain$endX[j])){
      PassChain$endX[j] = PassChain$x[j]
      PassChain$endY[j] = PassChain$y[j]
    }
    addingOne <- c(PassChain$x[j],PassChain$y[j],
                   PassChain$endX[j],PassChain$endY[j])
    cluster = c(cluster,addingOne)
    if(j %% 4 == 0){
      kdata <- rbind(kdata, cluster)
      cluster <- c()
      
    }
  }
  
  #preparing the two datasets to apply KMC
  kdata <- kdata %>% filter(x1 != 0)
  kdata <- na.omit(kdata)
  kpop <- kdata
  kpop <- scale(kpop)
  kpop <- na.omit(kpop)
  
  #required graph so that we can manually determine what to use as centers
  fviz_nbclust(kpop, kmeans, method = "wss")
  
  #applying KMC
  kpasses <- kmeans(kpop, centers = 4, nstart = 25)
  
  #Now adding in the Cluster factor to the passes
  kdata <- kdata %>% mutate(Cluster=kpasses$cluster)
  kdata$Cluster <- as.factor(kdata$Cluster)
  
  PassChain <- PassChain %>% mutate(Cluster = 0)
  
  for(j in 1:nrow(kdata)){
    for(i in 1:4){
      r = ((j-1)*4) + i
      PassChain$Cluster[r] = kdata$Cluster[j]
    }
  }
  
  PassChain <- PassChain %>% filter(Cluster!=0)
  PassChain$Cluster <- as.factor(PassChain$Cluster)
  
  #finding the single pass-chain representative for the cluster
  chainrows = nlevels(PassChain$Cluster)
  for(c in 1:chainrows){
    TempCP <- PassChain %>% filter(Cluster==c)
    for(i in 1:4){
      if(i==1){
        groupdivider = "A"
      }
      else if(i==2){
        groupdivider = "B"
      }
      else if(i==3){
        groupdivider = "C"
      }
      else if(i==4){
        groupdivider = "D"
      }
      #else if(i==5){
       # groupdivider = "E"
      #}
      TempCP2.0 <- TempCP %>% filter(order==groupdivider)
      x <- c(median(TempCP2.0$x),median(TempCP2.0$y),
             median(TempCP2.0$endX),median(TempCP2.0$endY),c,groupdivider,
             nrow(TempCP),mean(TempCP2.0$direct))
      TableauData <- rbind(TableauData,x)
    }
  }
  
  #formatting our final dataset so that it can be graphed
  TableauData <- TableauData %>% filter(x!=0)
  TableauData$x <- as.numeric(TableauData$x)
  TableauData$y <- as.numeric(TableauData$y)
  TableauData$endX <- as.numeric(TableauData$endX)
  TableauData$endY <- as.numeric(TableauData$endY)
  TableauData$count <- as.numeric(TableauData$count)
  TableauData$Cluster <- as.factor(TableauData$Cluster)
  TableauData$direct <- as.numeric(TableauData$direct)
  TableauData <- TableauData %>% filter(!is.nan(x))


#write.csv(TableauData, "D:\\R\\Chelsea\\Sheffield United\\TableauData.csv", row.names = FALSE)
#Histograms for the duration
PassChain$sec <- as.numeric(PassChain$sec)
PassChain$minute <- as.numeric(PassChain$minute)
Duration <- data.frame("duration" = 0, "distance" = 0)
for(i in seq(1,nrow(PassChain), by=4)){
  initial = (PassChain$minute[i]*60)+PassChain$sec[i]
  second = (PassChain$minute[i+3]*60)+PassChain$sec[i+3]
  difference = second-initial
  x <- c(difference)
  distance = 0
  for(j in 3:0){
    length <- sqrt(((PassChain$endX[i+j]-PassChain$x[i+j])*(PassChain$endX[i+j]-PassChain$x[i+j])) + 
                     ((PassChain$endY[i+j]-PassChain$y[i+j])*(PassChain$endY[i+j]-PassChain$y[i+j])))
    if((PassChain$endX[i+j]-PassChain$x[i+j])<0){
      length = -1*length
    }
    distance <- distance + length
  }
  final <- c(x,distance)
  Duration <- rbind(Duration,final)
}

Duration <- Duration %>% filter(duration < 50) %>%
  filter(duration > 0)
Duration$duration <- as.numeric(Duration$duration)
Duration$distance <- as.numeric(Duration$distance)
Duration <- Duration %>% mutate(speed=(abs(distance))/duration)

#graphing the histogram
Duration %>%
  ggplot(aes(x=speed))+
  geom_density(fill="#CF2990", alpha=0.4, adjust=1/2)+
  labs(x="Speed of sequences in m/s", y="Density",
       title="Chelsea's average speed for their sequences (m/s)")+
  theme_bw()+
  theme(
    plot.title = element_text(size=30),
    axis.title = element_text(size=15),
    axis.text = element_text(size=15),
  )


#ClusteredChains Graphing
TableauData %>% 
  ggplot()+
  annotate_pitch(dimensions = pitch_opta, colour = "white",
                 fill = "#141622")+
  theme_pitch()+
  theme(panel.background = element_rect(fill = "#141622"))+
  geom_segment(aes(x=(x),y=(y),xend=(endX), yend=(endY), color=Cluster,alpha=count),
               lineend = "round", size = 2, arrow = arrow(length = unit(0.08, "inches")))+
  scale_alpha_continuous(range=c(0.2,0.8))+
  geom_point(aes(x=endX,y=endY, color=Cluster,alpha=count),size=8, shape=1)

#Graphing all mini-clusters and one main chain
  ggplot()+
  annotate_pitch(dimensions = pitch_opta, colour = "white",
                 fill = "#141622")+
  theme_pitch()+
  theme(panel.background = element_rect(fill = "#141622"))+
  geom_segment(data = PassChain, aes(x=(x),y=(y),xend=(endX), yend=(endY)),
               color="#F0FFFF", alpha=0.1, lineend = "round", size = 1, arrow = arrow(length = unit(0.15, "inches")))+
  geom_segment(data = TableauData, aes(x=(x),y=(y),xend=(endX), yend=(endY), color=Cluster),
               lineend = "round", size = 1.5, arrow = arrow(length = unit(0.15, "inches")))+
  geom_point(data = TableauData, aes(x=x,y=y, color=Cluster),size=6, shape=1)+
    facet_wrap(~Cluster)
  

#graph non-pass events in the chain
  PassChain %>% filter(order=="D") %>% 
    filter(x>=83) %>% filter(type != "Pass") %>%
    ggplot()+
    annotate_pitch(dimensions = pitch_opta, colour = "white",
                   fill = "#141622")+
    theme_pitch()+
    theme(panel.background = element_rect(fill = "#141622"))+
    geom_point(aes(x=endX,y=endY, color=type,shape=type),size=3.5)
    
  
#Width and Height of the Chains
  PassData %>%
    filter(x>=66) %>% 
    filter(x<=83) %>%
    filter(y>=21.1) %>%
    filter(y<=78.9) %>%
    filter(`outcomeType/value`==1) %>%
    ggplot()+
    annotate_pitch(dimensions = pitch_opta, colour = "white",
                   fill = "#141622")+
    theme_pitch()+
    geom_point(aes(x=x,y=y), size=3, shape=1, color="white")+
    geom_segment(aes(x=(x),y=(y),xend=(endX), yend=(endY)), color="#2E8B57",
                 lineend = "round", alpha=0.7, size = 1.5, arrow = arrow(length = unit(0.15, "inches")))
  "#3CB371"
  "#2E8B57"
  "#FFD700"
  
  
  
  PassChain %>% filter(order%in%c("B","C")) %>%
    ggplot(aes(x=direct))+
    ylim(0,1.1)+
    geom_density(fill="#4d34d9", alpha=0.4)+
    labs(x="Speed of sequences in m/s", y="Density (Cumulative)",
         title="Dortmund's directnes in their sequence ")+
    theme_bw()+
    theme(
      plot.title = element_text(size=30),
      axis.title = element_text(size=15),
      axis.text = element_text(size=15),
    )
 


#ModifiedPassing
FinalPassing <- data.frame("playerId"="","receiverId"="","x"=0,
                                        "endX"=0,"y"=0,"endY"=0,"group" = 0,
                                        "order"="","type" = "", "minute" = 0, "team" = "",
                                        "Matchweek" = "", "Opponent" = "", "Cluster" = 0)
PassChain$Cluster <- as.numeric(PassChain$Cluster)
for(i in seq(1,nrow(PassChain), by=4)){
  if(PassChain$order[i] == 'A' && PassChain$x[i] >= 50){
    for(j in 3:0){
      x <- c(PassChain$playerId[i+j],PassChain$receiverId[i+j],PassChain$x[i+j],
             PassChain$endX[i+j],PassChain$y[i+j],PassChain$endY[i+j],PassChain$group[i+j],
             PassChain$order[i+j], PassChain$type[i+j], PassChain$minute[i+j],
             PassChain$team[i+j],PassChain$Matchweek[i+j],PassChain$Opponent[i+j],PassChain$Cluster[i+j])
      
      FinalPassing <- rbind(FinalPassing, x)
    }
  }
}

FinalPassing <- FinalPassing %>% filter(playerId!="")
FinalPassing$x <- as.numeric(FinalPassing$x)
FinalPassing$y <- as.numeric(FinalPassing$y)
FinalPassing$endX <- as.numeric(FinalPassing$endX)
FinalPassing$endY <- as.numeric(FinalPassing$endY)
FinalPassing$Cluster <- as.factor(FinalPassing$Cluster)
PassData <- FinalPassing %>% filter(type=="Pass")
NonPD <- FinalPassing %>% filter(type!="Pass")
FinalPassing %>% 
  ggplot()+
  annotate_pitch()+
  theme_pitch()+
  geom_point(data=NonPD, aes(x=endX,y=endY, color=type,shape=type),size=3.5)+
  geom_segment(data=PassData, aes(x=(x),y=(y),xend=(endX), yend=(endY), color=type),
               alpha = 0.7, lineend = "round", size = 2, arrow = arrow(length = unit(0.08, "inches")))+
  facet_wrap(~order)


#Heatmaps of the Ordering of the Chains
PassChain %>% 
  ggplot() + 
  annotate_pitch()+
  theme_pitch()+
  geom_density2d_filled(aes(x=x, y=y), alpha=0.8)+
  #geom_bin2d(aes(x=endX, y=endY), bins = 10, alpha=0.8)+
  #scale_fill_gradient2(low = "#8b4895", high = "#02b9fd", na.value = NA) +
  facet_wrap(~Opponent)


#Heatmaps of Players
MainData %>% filter(`type/displayName`=="Pass") %>%
  filter(teamId == "Manchester City") %>%
  ggplot() + 
  annotate_pitch()+
  theme_pitch()+
  #geom_density2d_filled(aes(x=x, y=y), alpha=0.8)+
  geom_bin2d(aes(x=endX, y=endY), binwidth = c(20, 20), alpha=0.8)+
  scale_fill_gradient2(low = "#8b4895", high = "#CF2990", na.value = NA)+
  facet_wrap(~playerId)


#PassFlow
bin = 20
x_bin = 100/bin
y_bin = 100/bin
passfx <- seq(0,100,by=bin)
passfy <- seq(0,100,by=bin)
PassFlow <- data.frame("x"=0.0,"y"=0.0,"endX"=0.0,"endY"=0.0, countP=0.0)


PlayerPF <- Team1 %>% filter(playerId %in% c("Kai Havertz"))

for(i in 1:x_bin){
  filterx <- PlayerPF %>% filter(x>=passfx[i]) %>%
    filter(x<passfx[i+1])
  for(j in 1:y_bin){
    minY = passfy[j]
    maxY = passfy[j+1]
    filtery <- filterx %>% filter(y>=minY) %>%
      filter(y<maxY)
    if(nrow(filtery)>=1){
      me_x = mean(filtery$x)
      me_y = mean(filtery$y)
      me_ex = mean(filtery$endX)
      me_ey = mean(filtery$endY)
      count = sum(filtery$x)
      x <- c(me_x,me_y,me_ex,me_ey,count)
      PassFlow <- rbind(PassFlow, x)
    }
  }
}

PassFlow <- PassFlow[2:nrow(PassFlow), ]

PassFlow %>%
  ggplot()+
  annotate_pitch(dimensions = pitch_opta, colour = "white",
                 fill = "#141622") +
  theme_pitch()+
  geom_bin2d(data=PlayerPF,aes(x=x,y=y),alpha=0.6, 
             binwidth = c(bin, bin), position = "identity")+
  scale_fill_viridis(option = "D")+
  geom_segment(aes(x=x,y=y,xend=endX,yend=endY,alpha=countP),
               color="white",lineend = "round", size=3.5, arrow = arrow(length = unit(0.1, "inches")))

#HULLS
directattack <- c('SavedShot', 'MissedShots', 'Goal', 'TakeOn')
defense <- c('Interception','Clearance','Tackle','Challenge','Ball Recoveries')
hull <- MainData %>% filter(`outcomeType/displayName` == "Successful" ) %>%
  filter(isTouch == TRUE) %>%
  filter(playerId == "Reece James") %>%
  filter(x < quantile(MainData$x, 0.90)) %>%
  filter(x>quantile(MainData$x, 0.10)) %>%
  filter(y < quantile(MainData$y, 0.90)) %>%
  filter(y>quantile(MainData$x, 0.10))%>% 
  slice(chull(x, y))

hull2 <-MainData %>% filter(`outcomeType/displayName` == "Successful" ) %>%
  filter(isTouch == TRUE) %>%
  filter(playerId == "Ben Chilwell") %>%
  filter(x < quantile(MainData$x, 0.90)) %>%
  filter(x>quantile(MainData$x, 0.10)) %>%
  filter(y < quantile(MainData$y, 0.90)) %>%
  filter(y>quantile(MainData$x, 0.10))%>% 
  slice(chull(x, y))

ggplot() +
  annotate_pitch(dimensions = pitch_opta) +
  theme_pitch() +
  geom_point(data=hull, aes(x,y), color="#CF2990")+
  geom_polygon(data=hull, aes(x,y), alpha = 0.2, colour="#CF2990", fill="#CF2990")+
  geom_point(data=hull2, aes(x,y), color="#00cc96")+
  geom_polygon(data=hull2, aes(x,y), alpha = 0.2, colour="#00cc96", fill="#00cc96")


#DirectnessRadars
DirectJorgi <- MainData %>% filter(playerId == "Kai Havertz") %>%
  filter(!is.na(directness))

DirectJorgi %>%
  ggplot()+
  geom_density(aes(x=directness), fill="#CF2990", alpha=0.4)+
  scale_x_continuous(breaks=c(-1.0, -0.5, 0, 0.5, 1.0),
                   labels=c("Directly Backwards",
                            "Angled Backwards",
                            "Sideways",
                            "Angled Forwards",
                            "Directly Forwards"))+
  theme_minimal()+
  theme(
    axis.text.x = element_text(color="black",size=13),
    axis.text.y = element_text(color="black",size=13),
    axis.title.y = element_text(color="black",size=20),
    axis.title.x = element_text(color="black",size=20)
  )
