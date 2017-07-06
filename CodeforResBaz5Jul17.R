

# https://gist.github.com/trishnowland/fabf4b8d767b7674e229b9b705d0eb2f

# FACEBOOK GROUP = Psychological Dynamics

# ********    COOKBOK IN R  ******
# http://sachaepskamp.com/files/Cookbook.html


#EXERCISE 1 Load up the libraries 
#first two are generally needed if we're dynamic network modelling

library(bootnet)
library(qgraph)

#This one gives us our data to play with for the Big Five Personality Factors
library(psych)

#EXERCISE 2 Big Five Factors data - create correlation network

data(bfi)                    #This loads up the bfi data from the psych library
bfiSub <- bfi[,1:25]         #This chooses out columns 1-25
head(bfiSub)
bfiCors <- cor_auto(bfiSub)  #This correlates it
head(bfiCors)  # correlation matriz
Names <- scan("http://sachaepskamp.com/files/BFIitems.txt"
              ,what ="character", sep = "\n") #This gives some labels to items

Groups <- rep(c('A','C','E','N','O'),each=5)  #This groups the items

#EXERCISE 2A - create total correlation graph
qgraph(bfiCors,groups=Groups,maximum=1,nodeNames = Names,vsize = 5,legend.cex = 0.4, maximum = .8) #this graphs it

#EXERCISE 2B - create partial correlation graph
qgraph(bfiCors,groups=Groups,maximum=1,vsize = 5, legend.cex = 0.4,graph='pcor', maximum = .8)


#EXERCISE 2C - comparing correlation and partial correlation
layout(t(1:2))
qgraph(bfiCors,groups=Groups,maximum=1,nodeNames = Names,vsize = 5,legend.cex = 0.2, maximum = .8)
qgraph(bfiCors,groups=Groups,maximum=1,vsize = 5, legend.cex = 0.3,graph='pcor', maximum = .8)


#EXERCISE 3 - PTSD DATA 
# data from 221 individuals with sub-threshhold PTSD
# We want to create data frame of variables that we need for analysis: 
# age, gender, all 20 symptoms (_MONTH extension), Sum_GAD2, Sum_PHQ2, Passive_SI_FINAL, Active_SI_FINAL, PCS, MCS, QualityofLife_SUM)
library(bootnet)
PTSD_data<-na.omit(read.csv("PTSD_data_V2.csv"))
head(PTSD_data)
data<-as.data.frame(PTSD_data[,c(2:3,11:37)]) # data for analysis
data <- subset(data, select=c(Q28_01_MONTH:QualityofLife_SUM, PPAGE, PPGENDER)) #put covariates last
data[, 29]<-ifelse(data$PPGENDER=='Male',1,2) # 1 is male; 2 is female
data<-data[,-c(23,27)] # Remove SI_passive & QoL, we decided later we don't want them
data_ptsd<-as.data.frame(data[,c(1:20)]) # PTSD items only
head(data_ptsd)


#the next instruction passes to 'results' the estimated network qgraph/EBICglasso
results<- estimateNetwork(
  data_ptsd, #this is our dataset
  default = "EBICglasso", #this is the instruction to make a network using EBICglasso
  corMethod = "cor_auto", #detect the kind of variable and perform lavaan/correlation
  tuning = 0.5) #apply tuning parameter to 0.5 (which is actually the default)

PTSDNames<-scan ("https://gist.githubusercontent.com/anonymous/334e63154459bd1ba9f34b4fc2e6de5f/raw/90f50f83484d4e680d27f1498ed414bb28515bd0/PTSDItems.txt"
                 ,what ="character", sep = "\n")

results$graph #makes a graph out of the network
#now we want to display it
library("qgraph")

qgraph(results$graph, #make a graph!
       nodeNames = PTSDNames,
       vsize = 5,
       legend.cex = 0.3, 
       cut = 0, #specify scaling of width and colour of edges is not separated 
       layout = "spring") #the layout uses a version of the Fructerman-Reingold algorithm

centralityPlot(results)  # centrality es cuando un item de la encuesta tiene el mismo comportamiento en cada area
#strength takes the sum of absolute edge weights connected to the node
#closeness thakes the inverse of the sum of the distances from one node to all other nodes
#betweenness quantifies how often one node is the shortest path between all other nodes

results_refit<- estimateNetwork(
              data_ptsd, #this is our dataset
              default = "EBICglasso", #this is the instruction to make a network using EBICglasso
              corMethod = "cor_auto", #detect the kind of variable and perform lavaan/correlation
              tuning = 0.5,
              refit = TRUE)

simRes<- netSimulator(results_refit$graph,
                      dataGenerator = ggmGenerator(ordinal = TRUE, nLevels = 5),
                      default = "EBICglasso",
                      nCases = c(100,250,500,1000,2500),
                      nReps = 100,
                      nCores = 8)

plot(simRes)
plot(simRes, yvar = c("strength", "closeness", "betweenness"))

boot1 <-bootnet(results, nCores = 8, nBoots = 1000, type = "nonparametric")
boot1