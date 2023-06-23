#################################
#################################
####   Code for Galli et al 
#################################
#################################

########################################
### Basic information about the dataset
########################################

library(tidyverse)

#load table and prepare data for analysis
df <- read.csv("TableS1.csv", na.strings = "NA")
gps <- read.csv("TableS2.csv", na.strings = "NA")

df2 <- data.frame(lapply(df, rep, df$count))
df3 <- df2 %>%  select(-count)
Tab_1630 <- left_join(df2, gps, by = "Parish")

Tab_1630_plague <- Tab_1630 %>% filter(Death_cause == "Plague")

#total number deaths --> 8152
nrow(Tab_1630)

#total number of plague-related and not plague-related deaths 
table(Tab_1630$Death_cause)

#total number of plague death cases with no parish information --> 333
length(Tab_1630_plague$Parish[is.na(Tab_1630_plague$Parish)])

#total number of not georeferenced plague cases (no parish + missing coordinates of parishes) --> 369
length(Tab_1630_plague$Parish[is.na(Tab_1630_plague$Latitude)])

# % of georeferenced cases --> 93%
((5261-369)/5261)*100

#number of parishes recorded for plague cases --> 94
length(unique(Tab_1630_plague$Parish[!is.na(Tab_1630_plague$Parish)]))

#number of georeferenced parishes for plague cases --> 79
tmp_df <- Tab_1630_plague[!is.na(Tab_1630_plague$Latitude),]
length(unique(tmp_df$Parish))

#number of not georeferenced parishes for plague cases --> 15
tmp_df <- Tab_1630_plague[is.na(Tab_1630_plague$Latitude),]
length(unique(tmp_df$Parish[!is.na(tmp_df$Parish)]))

# Number of weeks with at least one plague death -> 42
Tab_1630_plague$Date <- as.Date(Tab_1630_plague$Date)
length(unique(format(Tab_1630_plague$Date, "%W")))


#Plague deaths before and after the procession of 11 June 1630
Tab_1630$Date <- as.Date(Tab_1630$Date)

before <- Tab_1630 %>% filter(Death_cause == "Plague") %>%  filter(Date < as.Date("1630-06-11")) %>%  nrow()
before
#876
after <- Tab_1630 %>% filter(Death_cause == "Plague") %>%  filter(Date >= as.Date("1630-06-11")) %>%  nrow()
after
#4385

########################################
### Figure 1
######################################## 
library(ggplot2)
library(gplots)
library(reshape2)

#load tabels and prepare data for analysis
df <- read.csv("TableS1.csv", na.strings = "NA")
gps <- read.csv("TableS2.csv", na.strings = "NA")

df2 <- data.frame(lapply(df, rep, df$count))
df3 <- df2 %>%  select(-count)
tab <- left_join(df2, gps, by = "Parish")


tab$Date <- as.Date(tab$Date)

#sort by date
tab <- tab[order(tab$Date),]

plague_tab <- subset(tab, tab$Death_cause == "Plague")
no_plague_tab <- subset(tab, tab$Death_cause != "Plague")

# build Figure 1

pdf("Figure_1.pdf", 24,9)
ggplot(tab, aes(x=Date, fill=Death_cause))+
  geom_bar(stat="count")+
  annotate("rect", xmin = as.Date("1630-08-04"), xmax = as.Date("1630-08-31"), ymin = -Inf, ymax = Inf, fill="grey", alpha=0.3)+
  scale_fill_manual(values=c("darkgrey","indianred1"), labels=c("Unrelated to Plague","Plague")) +
  scale_x_date(date_breaks = "7 day", date_labels = "%d/%m/%Y", expand=c(0,0))+
  # ggtitle("TOTAL deaths in 1630")+
  facet_wrap(~Death_cause, ncol = 1) +
  theme_minimal()+
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )+
  theme(axis.text.x = element_text(angle=90, size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        plot.title=element_text(size = 20)
  ) + 
  labs(fill='Death cause',
       y = 'Number of Deaths') 
dev.off()


########################################
### Figure 2
######################################## 

#Plot first plague death cumulative only for parishes of cluster 1 and 2
library(tidyverse)

#load tables and prepare data for analysis
df1 <- read.csv("TableS1.csv", na.strings = "NA")
gps <- read.csv("TableS2.csv", na.strings = "NA")

df2 <- data.frame(lapply(df1, rep, df1$count))
df3 <- df2 %>%  select(-count)
df<- left_join(df2, gps, by = "Parish")

df <- df %>% filter(Death_cause == "Plague")
df$Date <- as.Date(df$Date)

df <- df[order(df$Date),]

df2 <- df[!duplicated(df$Parish),]


days <- seq(as.Date("1630-01-01"), as.Date("1630-12-31"), 1)

count <- c()

for(i in 1:length(days)){
  count <- c(count, sum(df2$Date < days[i]))
}


data <- data.frame(Day = days, Count = count)

data$Count[data$Day == "1630-06-11"] #number of parishes which had at least one case of plague before "1630-06-11" --> 63

data <- data %>% filter(Day < "1630-08-04")

p <- data %>% ggplot(aes(x = Day, y = Count)) +
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = as.Date("1630-06-11"), linetype = "dashed") +
  labs(x = "Date",
       y = "Cumulative number of parishes")


p
ggsave(p, file = "First_day_ALL_PARISHES.png")


########################################
### Figure 3
########################################  

library(ggplot2)
library(reshape2)
library(factoextra)
library(RColorBrewer)
library(gplots)
library(NbClust)
library(grDevices)
library(ggpubr)
library(patchwork)

#load table and prepara data for analysis
df <- read.csv("TableS1.csv", na.strings = "NA")
gps <- read.csv("TableS2.csv", na.strings = "NA")

df2 <- data.frame(lapply(df, rep, df$count))
df3 <- df2 %>%  select(-count)
tab <- left_join(df2, gps, by = "Parish")

tab$Date <- as.Date(tab$Date)
tab <- droplevels(subset(tab, tab$Longitude != ""))


### Cumulative curves
tab_1630_peste <- droplevels(subset(tab, tab$Death_cause == "Plague"))

t_1630_peste <- as.matrix(table(tab_1630_peste$Date, tab_1630_peste$Parish))

# Add days without plague deaths
days1630 <- seq(from=as.Date("1630-01-01"), to=as.Date("1630-12-31"), by=1)
absent_days <- as.Date(setdiff(days1630, as.Date(row.names(t_1630_peste))), origin="1970-01-01")
t_1630_peste_absent <- matrix(ncol=ncol(t_1630_peste), nrow=length(absent_days))
t_1630_peste_absent[is.na(t_1630_peste_absent)] <- 0
row.names(t_1630_peste_absent) <- as.character(absent_days)
t_1630_peste_all <- rbind(t_1630_peste, t_1630_peste_absent)

# Order the table chronologically
t_1630_peste_all_ord <- t_1630_peste_all[order(row.names(t_1630_peste_all)),]

# Cumulative curves
peste_cum <- matrix(ncol=ncol(t_1630_peste_all_ord), nrow=0)

for (i in 2:nrow(t_1630_peste_all_ord)) {
  tmp <- colSums(t_1630_peste_all_ord[1:i,])
  peste_cum <- rbind(peste_cum, tmp)
}

peste_cum <- rbind(t_1630_peste_all_ord[1,], peste_cum)
row.names(peste_cum) <-row.names(t_1630_peste_all_ord)

# Normalize the number of deaths
peste_cum_norm <- apply(peste_cum, 2, function(x) x/max(x))

######### Clustering

# Rename columns
peste_cum_norm_ren <- peste_cum_norm

#Death threshold at 21 plague deaths (more than 1 every 2 weeks of epidemic) for parishes selection
death_count_thr <- 21

parr_sel <- peste_cum[nrow(peste_cum),] > death_count_thr
peste_cum_norm_ren_sel <- peste_cum_norm_ren[,parr_sel]
peste_cum_norm_ren_sel_2 <- peste_cum_norm_ren_sel[,which(apply(peste_cum_norm_ren_sel, 2, var) != 0)]

## PCA
library(ade4)
library(vegan)

dist <- dist(t(peste_cum_norm_ren_sel_2))

pca <- cmdscale(dist)


#silhouette optimal number of clusters
pdf("Clustering_parrocchie_min_21_morti_peste.silhouette.pdf")
fviz_nbclust(peste_cum_norm_ren_sel_2, kmeans, method = "silhouette", k.max=10)
dev.off()

f <- kmeans(pca, 2)

clusters <- as.matrix(f$cluster)

clus <- clusters[as.matrix(row.names(pca)),1]

a <- adonis2(pca ~ clus, method='eu')
p_value <- a$`Pr(>F)`[1]


pdf("Figure3b.pdf")
s.class(pca, 
        as.factor(clus), 
        col = c("#1B9E77","#D95F02"), 
        sub = paste("PCA Axis1-Axis2 Permanova p-value < ", 
                    p_value, 
                    sep = ""),
        xlim = c(-3.8, 3))
dev.off()


# plot cumulative curves
peste_cum_norm_melt <- melt(peste_cum_norm_ren_sel)
colnames(peste_cum_norm_melt) <- c("Date", "Parish", "Count")

clusters <- as.matrix(f$cluster)

####################
#generate table with information about the specific cluster of each parish
####################
palette <- brewer.pal(n = 3 , name = 'Dark2')

tab_clusters <- data.frame(Parish = row.names(clusters), Cluster = as.data.frame(clusters)$V1, Color = palette[as.matrix(clusters)])

write.csv(tab_clusters, file = "Clusters.csv", row.names = F)

###################
###################

peste_cum_norm_melt$Date <- as.Date(peste_cum_norm_melt$Date)
peste_cum_norm_melt$Cluster <- as.character(clusters[as.matrix(peste_cum_norm_melt$Parish), 1])

Figure_3A <- ggplot(peste_cum_norm_melt, aes(x=Date, y=Count*100, group = Parish, color = Cluster)) + 
  geom_line() +  
  scale_color_manual(values = c("1" =  "#1B9E77", "2" = "#D95F02")) + 
  theme_classic() +
  geom_vline(xintercept = as.Date("1630-06-11"), linetype = "dashed") +
  ylab("Cumulative relative frequency of plague deaths (%)")

# ggsave(Figure_3A, file = "Figure_3A.pdf")

#first plague case for each parish
peste_cum_norm_melt_first <- peste_cum_norm_melt[peste_cum_norm_melt$Count > 0,]
peste_cum_norm_melt_first_nodup <- peste_cum_norm_melt_first[!duplicated(peste_cum_norm_melt_first$Parish),]
peste_cum_norm_melt_first_nodup[,3:4]<-NULL
colnames(peste_cum_norm_melt_first_nodup)<-c("Date_first_death","Parish")


# Cumulative curve - 25%
peste_cum_norm_melt_25 <- peste_cum_norm_melt[peste_cum_norm_melt$Count >= 0.25,]
peste_cum_norm_melt_25_nodup <- peste_cum_norm_melt_25[!duplicated(peste_cum_norm_melt_25$Parish),]
peste_cum_norm_melt_25_nodup[,3:4]<-NULL
colnames(peste_cum_norm_melt_25_nodup)<-c("Date_25_death","Parish")


# Cumulative curve - 50%
peste_cum_norm_melt_50 <- peste_cum_norm_melt[peste_cum_norm_melt$Count >= 0.5,]
peste_cum_norm_melt_50_nodup <- peste_cum_norm_melt_50[!duplicated(peste_cum_norm_melt_50$Parish),]
peste_cum_norm_melt_50_nodup[,3:4]<-NULL
colnames(peste_cum_norm_melt_50_nodup)<-c("Date_50_death","Parish")


# Infection points
library("inflection")

infl_date_tab <- matrix(ncol=2, nrow=ncol(peste_cum_norm))
colnames(infl_date_tab) <- c("Inflection_date", "Parish")

for (i in 1:ncol(peste_cum_norm)){
  col = colnames(peste_cum_norm)[i]
  infl_date <- as.Date(bede(as.numeric(as.Date(row.names(peste_cum_norm))), peste_cum_norm[,as.matrix(col)],0)$iplast, origin = "1970-01-01")
  infl_date_tab[i, "Inflection_date"] <- as.character(infl_date)
  infl_date_tab[i, "Parish"] <- col 
}


# Cumulative curve total table
all_tab_tmp <- merge(clusters, peste_cum_norm_melt_first_nodup, by.x="row.names", by.y="Parish")
colnames(all_tab_tmp)[1:2] <- c("Parish", "Cluster")

all_tab_tmp1 <- merge(all_tab_tmp, peste_cum_norm_melt_25_nodup, by="Parish")
all_tab_tmp2 <- merge(all_tab_tmp1, peste_cum_norm_melt_50_nodup, by="Parish")
all_tab <- merge(all_tab_tmp2, infl_date_tab, by="Parish")
all_tab$Inflection_date <- as.Date(all_tab$Inflection_date )

all_tab2 <- melt(all_tab,  id.vars = c("Parish", "Cluster"))

###
write.csv(all_tab2, file = "all_tab2.csv", row.names = F)
###

all_tab2$Cluster <- factor(all_tab2$Cluster, levels = c("1","2"), ordered = TRUE)


my_comparisons <- list(c("1","2"))

labels <- list("First death", "25%", "50%", "Inflection date")

#label name of facet
labels <- list("First_death" = "First death",
               "Date_25_death" = "25%",  
               "Date_50_death" =  "50%",  
               "Inflection_date" = "Inflection date")

facet_labeller <- function(variable,value){
  return(labels[value])
}

Figure_3C <- ggboxplot(all_tab2, x = "Cluster", y = "value", fill = "Cluster") + 
  scale_fill_manual(values=as.matrix(palette)) + 
  geom_jitter(alpha=0.5, position = position_jitter(width = 0.3)) + 
  facet_wrap(~all_tab2$variable, ncol=6, labeller = facet_labeller ) + 
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", size=4, label = "p.signif", hide.ns = TRUE) + 
  geom_hline(data= all_tab2, aes(yintercept=as.Date("1630-06-11")), linetype="dashed", color="black") +
  theme(legend.position = "none") +
  ylab( "Date") +
  theme(strip.text.x = element_text(size = 8))



Figure_3 <- ((Figure_3A / plot_spacer()) | Figure_3C) + plot_layout(guides = 'collect', widths = c(2,2))

ggsave("Figure3.pdf", width = 32, height = 20, units = "cm")

# Median dates for the two clusters
all_tab2 %>%  filter(variable == "Date_first_death") %>% group_by(Cluster) %>% summarize(median = median(value))
# Cluster median    
# <ord>   <date>    
#1       1630-04-29
#2       1630-05-18

all_tab2 %>%  filter(variable == "Date_25_death") %>% group_by(Cluster) %>% summarize(median = median(value))
# Cluster median    
# <ord>   <date>    
#1       1630-06-11
#2       1630-06-24

all_tab2 %>%  filter(variable == "Date_50_death") %>% group_by(Cluster) %>% summarize(median = median(value))
# # A tibble: 2 × 2
#   Cluster median    
#   <ord>   <date>    
#1       1630-06-20
#2       1630-07-10

all_tab2 %>%  filter(variable == "Inflection_date") %>% group_by(Cluster) %>% summarize(median = median(value))
# # A tibble: 2 × 2
# Cluster median    
# <ord>   <date>    
#1       1630-06-17
#2       1630-07-07



########################################
### Figure 4
######################################## 
library(ggplot2)
library(scales)
library(tidyverse)
library(patchwork)

#load table and prepara data for analysis
df <- read.csv("TableS1.csv", na.strings = "NA")
gps <- read.csv("TableS2.csv", na.strings = "NA")

df2 <- data.frame(lapply(df, rep, df$count))
df3 <- df2 %>%  select(-count)
tab <- left_join(df2, gps, by = "Parish")
tab$Date <- as.Date(tab$Date)

#load cluster table
clusters <- read.csv("Clusters.csv")

tab_cluster <- left_join(tab, clusters, by = "Parish")
tab_cluster <- droplevels(subset(tab_cluster, !is.na(tab_cluster$Cluster)))   #drop rows without cluster total number of cases in the 2 clusters = 7002
#tab_plague_cluster <- droplevels(subset(tab_plague_cluster, tab_plague_cluster $Cluster != ""))


#add column with week of the year 1630
tab_cluster$Weeks <- as.numeric(format(tab_cluster$Date, "%W"))
tab_cluster$Cluster <- factor(tab_cluster$Cluster, levels=c(1,2))

tab_cluster <- tab_cluster %>%filter(Death_cause == "Plague")

#subset for cluster 1
tab1 <- tab_cluster[tab_cluster$Cluster == 1,]

p1 <- ggplot(tab1, aes(x=Weeks, fill=Death_cause))+
  geom_bar(stat="count")+
  xlim(0,52) +
  ylim(0, 520) +
  annotate("rect", xmin = 31, xmax = 33, ymin = -Inf, ymax = Inf, fill="white") +
  scale_fill_manual(values="#1B9E77") +
  geom_vline(xintercept = 23, linetype = "dashed")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90,size = 10), legend.position = "none") +
  labs(y = 'Number of Deaths')


tab2 <- subset(tab_cluster, tab_cluster$Cluster == 2)

p2 <-ggplot(tab2, aes(x=Weeks, fill=Death_cause))+
  geom_bar(stat="count")+
  ylim(0, 520) +
  xlim(0,52) +
  annotate("rect", xmin = 31, xmax = 33, ymin = -Inf, ymax = Inf, fill="white") +
  scale_fill_manual(values="#D95F02") +
  geom_vline(xintercept = 23, linetype = "dashed")+
  theme_classic()+
  theme(axis.text.x = element_text(angle=90,size = 10), legend.position = "none")+
  labs(y = 'Number of Deaths')


Figure_4A_B <- p1 + p2 + plot_layout(ncol = 1, guides = "collect")
ggsave("Figure_4A_B.png",  width = 11.7, height = 15, units = "cm")




############# Map clusters
library(jpeg)
library(png)
library(grid)
library(tidyverse)
library(ggpubr)


#Produce summary table
df <- read.csv("TableS1.csv", na.strings = "NA")
gps <- read.csv("TableS2.csv", na.strings = "NA")

df2 <- data.frame(lapply(df, rep, df$count))
df3 <- df2 %>%  select(-count)
tab <- left_join(df2, gps, by = "Parish")
tab$Date <- as.Date(tab$Date)

clusters <- read.csv("Clusters.csv")
tab_cluster <- left_join(tab, clusters, by = "Parish")

df <- tab_cluster %>% filter(!is.na(Latitude))   #remove parishes with no localization

#put parishes that are not in cluster 1 nor 2 in CLuster "0" and give "grey" as color
df$Cluster[is.na(df$Cluster)] <- 0
df$Color[is.na(df$Color)] <- "grey"

df2 <- df %>% group_by(Parish, Cluster,Color, Death_cause, Latitude, Longitude) %>%  
  summarize(Count = n())

# Save intermediate table 
# write.csv(df2, "Summary_ALL_table.csv", row.names = F)

#Parrocchia_map	cluster	cluster_color	Latitudine	Longitudine	Morti_totali_peste
peste_clus_gps <- df2
# peste_clus_gps <- read.csv("Summary_ALL_table.csv")

peste_clus_gps$Cluster <- factor(peste_clus_gps$Cluster, levels = c( "0","1","2"),ordered = TRUE)

#remove not plague deaths
peste_clus_gps <- peste_clus_gps %>%  filter(Death_cause == "Plague")

#remove column about Death cause
peste_clus_gps$Death_cause <- NULL


peste_clus_gps <- peste_clus_gps[order(peste_clus_gps$Cluster),]
colors_palette <- unique(peste_clus_gps[,"Cluster"])

map <- readPNG("crop_Mappa_blu_60.pdf.png")

#uncomment below to have different design in the background map. 
#WARNING: the final map will be ovewritten if you run again.
#map <- readPNG("positron_darker_2023.pdf.png")
#map <- readPNG("satellite_2023.pdf.png")
#Coordinates needed to crop the figure (obtained from QGIS analysis)
gps_map <-data.frame(X = c(9.145413, 9.228967),
                     Y = c(45.43978, 45.49275),
                     fid = c(1,2),
                     crop = c("BottomLeft", "TopRight"))

img_width <- ncol(map)
img_height <- nrow(map)
aspect_ratio <- img_width/img_height 

zoom <- 15

map_2_plot <- rasterGrob(map, interpolate=TRUE)

#fig margins
xmin <- gps_map[gps_map$crop=="BottomLeft","X"]	#Bottom Left xmin
ymin <- gps_map[gps_map$crop=="BottomLeft","Y"]	#Bottom Right ymin
xmax <- gps_map[gps_map$crop=="TopRight","X"]		#Top Right x xmax
ymax <- gps_map[gps_map$crop=="TopRight","Y"]


peste_clus_gps$Latitude <- as.numeric(peste_clus_gps$Latitude)
peste_clus_gps$Longitude <- as.numeric(peste_clus_gps$Longitude)
peste_clus_gps$Count <- as.numeric(peste_clus_gps$Count)


p <-	ggplot(peste_clus_gps, aes(Latitude,Longitude))+
  annotation_custom(map_2_plot, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)+
  geom_point(data=peste_clus_gps, aes( size = Count, color = Cluster))+
  scale_color_manual(values=c("grey30", "#1B9E77", "#D95F02"))+
  labs(size="Plague deaths", color="Cluster") + 
  #annotate(geom="label", x=xmax-((xmax-xmin)/2), y=45.492,label = paste("title here"), fill="white",label.size=NA, size=zoom/2.7, fontface=2) +
  xlim(xmin,xmax)+	#deve rispettare proprozioni dell'immagine
  ylim(ymin,ymax)+	#deve rispettare proprozioni dell'immagine
  theme_classic()+		
  theme(
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    legend.text=element_text(size=zoom/2),
    legend.title=element_text(size=zoom/2),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.position = c(.92, .50), #"none"
    #legend.title = element_blank(),        
    #legend.justification = c("right", "top"),
    #legend.box.just = "right",
    #legend.margin = margin(6, 6, 6, 6) 
    #plot.background = element_rect(fill = 'white', colour = 'white') #non fa niente
  )


ggsave(p, filename = "Map_Clustering_plague_+21_deaths.png",
       device="png", units = "cm", 
       width = aspect_ratio*zoom,
       height = zoom)

################### Cluster distance from the center

library(tidyverse)
library(ggpubr)

#Produce summary table
#################
#################
#load table and prepara data for analysis
df <- read.csv("TableS1.csv", na.strings = "NA")
gps <- read.csv("TableS2.csv", na.strings = "NA")

df2 <- data.frame(lapply(df, rep, df$count))
df3 <- df2 %>%  select(-count)
tab <- left_join(df2, gps, by = "Parish")
tab$Date <- as.Date(tab$Date)

clusters <- read.csv("Clusters.csv")
tab_cluster <- left_join(tab, clusters, by = "Parish")
tab_cluster <- droplevels(subset(tab_cluster, !is.na(tab_cluster$Cluster)))


tab_dist <- read.delim("TableS3.tab")
row.names(tab_dist) <- tab_dist$Parish

tab_cluster_dist <- left_join(tab_cluster, tab_dist, by = "Parish")


df <- tab_cluster_dist %>% 
  group_by(Parish, Cluster,Color, Death_cause, Latitude, Longitude, Dist) %>%  
  summarize(Count = n())

# Save intermediate table 
write.csv(df, "Summary_table.csv", row.names = F)

#################
#################

tab <- read.csv("Summary_table.csv")
head(tab)

#remove parishes the are located outside the city walls : "S. Trinità", "S. Rocco", "S. Pietro in Sala"
parishes_out <- list("S. Trinità", "S. Rocco", "S. Pietro in Sala")

tab2 <- tab[!(tab$Parish %in% parishes_out),]

tab3 <- tab2[!duplicated(tab2$Parish),]


cluster1 <- tab2 %>%  filter(Cluster == 1)
cl1_plague <- cluster1 %>% filter(Death_cause == "Plague")

cluster2 <- tab2 %>%  filter(Cluster == 2)
cl2_plague <- cluster2 %>% filter(Death_cause == "Plague")


wilcox.test(cl1_plague$Dist, cl2_plague$Dist, alternative = "greater")    
## p-value = 0.1213

#median distances for the two clusters
tab3 %>%  group_by(Cluster) %>%  summarize(median = median(Dist)) 
# Cluster median
# 1   790.
# 2   515.


my_comparisons <- list(c("1","2"))

Boxplot_distances_clusters <- ggboxplot(tab3, x = "Cluster", y = "Dist", fill = "Cluster") + 
  geom_boxplot(aes(fill = Cluster)) +
  scale_fill_manual(values = c ("#1B9E77", "#D95F02")) +
  geom_jitter(alpha=0.5, position = position_jitter(width = 0.3)) + 
  stat_compare_means(comparisons = my_comparisons, method = "wilcox.test", size=4, label = "p.signif", hide.ns = TRUE, method.args=list (alternative = "greater")) +
  theme(legend.position = "none") +
  labs(y = "Distance from city center (meters)")


# Number of deaths in the two clusters
#tab <- read.csv("Summary_table.csv")
pairwise.wilcox.test(tab$Count, tab$Cluster)
#0.39

tab %>% group_by(Cluster) %>% summarize(sum = sum(Count))
# Cluster   sum
# 1         2640
# 2         4362

tab_plague <- tab %>% filter(Death_cause == "Plague") 
pairwise.wilcox.test(tab_plague$Count, tab_plague$Cluster)
#0.99

tab_plague %>% group_by(Cluster) %>% summarize(sum = sum(Count))
# Cluster   sum
#  1        1625
#  2        2979

