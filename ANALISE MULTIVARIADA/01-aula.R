library(dplyr)
library(ggplot2)
library(GGally)
library(DataExplorer)
require(grDevices)


'Dataset: yankees1927.dat

Source: Baseball-Reference.com 

Description: Game stats for all offensive players for 1927 New York Yankees
(aka Murderers Row)

Variables/Columns
Batter ======================================== V1
Game ========================================== V2
fielder indicator /* 1=fielder, 0=pitcher  */ = V3
At Bats ======================================= V4
Runs ========================================== V5
Hits ========================================== V6
Runs Batted In (RBI) ========================== V7
Walks (BB) ==================================== V8
Strikeouts ==================================== V9
Cumulative Batting Average (After Game) ======= V10
Cumulative On-Base+Slugging =================== V11
Put Outs (Defense) ============================ V12     
Assists ======================================= V13
Doubles ======================================= V14
Triples ======================================= V15
Home Runs ===================================== V16
Hit By Pitcher (HBP) ========================== V17
Sacrifice Hits ================================ V18
Stolen Bases ================================== V19
Line /* Only used in original sort */ ========= V20'

data <- "http://users.stat.ufl.edu/~winner/data/yankees1927.dat"

dados <- read.fwf(file = data ,
                  widths =  c(15,-14,11,7,9,8,8,8,8,2,8,13,8,9,8,8,8,8,8,4,5))


### visualiza??o dos dados  ###

str(dados)

visdat::vis_dat(dados)

dados %>% visdat::vis_miss(cluster = TRUE)

dados$V3 <- as.factor(dados$V3)

dados <- mutate_if(dados, is.numeric, ~replace(., is.na(.), 0))

dados %>% visdat::vis_dat()


### Medidas de Centralidade e Variabildiade  " Vetor de m?dia ###

dados2 <- dados[,c(1,3:11)]

boxplot(dados[,4:13])

apply(dados[,4:19],2,summary)

ds::gds(dados[,4:19]) %>% View()


DataExplorer::plot_bar(dados2)
DataExplorer::plot_density(dados2)
DataExplorer::plot_histogram(dados2)
DataExplorer::plot_qq(dados2)


##### CORRELA??O #######

dados3 <- dados[,c(3,10:13)]

corrplot::corrplot(cor(dados2), tl.col = "black" ,method = 'number',
                   cl.pos = "n", tl.pos = "d", addrect = 1)

corrplot::corrplot(cor(dados2[,-1:-2]), method = 'circle', type = 'lower',
                   insig='blank',addCoef.col ='black',
                   number.cex = 0.7, order = 'AOE', diag=FALSE)

dados %>% ggpairs(., columns = c(4,10,11,12,13))

### Gropos ###

DataExplorer::plot_boxplot(dados2,by = "V1")
DataExplorer::plot_boxplot(dados2,by = "V3")

DataExplorer::plot_scatterplot(dados2,by = "V1")
DataExplorer::plot_scatterplot(dados2,by = "V3")

dados %>% ggplot(aes(x = V10, y = V11, col = V3)) + geom_point()

dados4 <- filter(dados, V3 == 1)
dados4 %>% ggplot(aes(x = V10, y = V11, col = V1)) + geom_point()

library(ggridges)
ggplot(dados, aes(x = V4, y = V1, fill = V1)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  theme_ridges() + 
  theme(legend.position = "none")


dados %>% ggpairs(., columns = c(4:13), 
                  aes(color = factor(V3)),legend = 1) + 
  theme(legend.position = "bottom" ) 


dados4 %>% ggpairs(., columns = c(11:12), 
                   aes(color = factor(V1)),legend = 1) + 
  theme(legend.position = "bottom" ) 

#### TABELAS DE FREQU?NCIAS CONJUNTAS #####

table(dados$V3,dados$V4)

dados %>% group_by(Batter = V1)%>%
  summarise(Media = mean(V10),Mediana = median(V10),
            Sd = sd(V10)) %>% View()


x <-dados %>% filter(!V16 == "0") %>% group_by(Batter = V1) %>%
  summarise(Home_Runs=sum(V16)) 
View(x)

ggplot(x, aes(x = Batter,y = Home_Runs)) + geom_bar(stat = "identity")

require(vcd) 

structable(V3~V1+V4,data=dados)
structable(V4 ~ V3+V1,data=dados)

dados$V20 <- if_else(dados$V4 < 2 ,"Ruim", "Bom")
dados$V21 <- if_else(dados$V16 < 1 , "Fraco" , "forte")

visdat::vis_dat(dados)

mosaic(~V20+V21+V3,data=dados)
mosaic(~V20+V21+V3,data=dados,highlighting=c(3))

mosaic(~V20+V21+V3,data=dados,highlighting=c(1))
mosaic(~V20+V21+V3,data=dados,highlighting_fill = gray.colors,highlighting=c(1))

round(prop.table(structable(~V20+V21+V3,data=dados)) *100, 2)


##### GRAFICOS DE PARALLELPLOT

library(lattice)
parallelplot(dados[,5:8])
parallelplot(dados[,5:8], col="black")

parallelplot(dados[,4:14], groups = dados[,3],
             horizontal.axis = TRUE )

parallelplot(dados[,4:14], groups = dados[,20],
             horizontal.axis = TRUE )

parallelplot(dados[,4:14], groups = dados[,21],
             horizontal.axis = TRUE )

parallelplot(dados[,4:14], groups = dados[,1],
             horizontal.axis = TRUE )

parallelplot(dados[,9:14], groups = dados[,1],
             horizontal.axis = TRUE )

parallelplot(dados[,9:12], groups = dados[,1],
             horizontal.axis = TRUE )

parallelplot(dados[,9:12], groups = dados[,3],
             horizontal.axis = TRUE )

parallelplot(dados[,9:12], groups = dados[,20],
             horizontal.axis = TRUE )

parallelplot(dados[,9:12], groups = dados[,21],
             horizontal.axis = TRUE )


###### Gr?fico Radar #######

dados5 <- dados %>% group_by(Batter = V1)%>%
  summarise(At_Bats = mean(V4),Runs = mean(V5),Hits = mean(V6),Runs_Batted = mean(V7),
            Walks = mean(V8),Strikeouts = mean(V9))


dados6 <- as.data.frame(dados5[,2:7])

rownames(dados6) <- dados5$Batter

stars(dados6, draw.segments=FALSE,
      key.loc = c(13,2), main = "", full = T)

stars(dados6, key.loc = c(13, 2),
      main = "", full = TRUE, draw.segments=TRUE)


library(fmsb)


colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

dados7 <- as.data.frame(dados6[1:5,1:6])



radarchart(dados7 , axistype=1 , 
           pcol=colors_border , pfcol=colors_in , plwd=2 , plty=3,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,4,1),
           cglwd=0.8,vlcex=0.8
)

legend(x=1.4, y=1, legend = rownames(dados6[1:5,]), bty = "n", pch=20 ,
       col=colors_in ,text.col = "grey", cex=0.75, pt.cex=2)

