require("cluster")
require("fpc")
require("factoextra")
require("gridExtra")
library(cluster)
library(fpc)
library(factoextra)
library(gridExtra)
library(conjoint)

rm(list = ls())


load("C:/Users/ZJ/Desktop/analytics design and application/assignment/assignment 3/GBA424 - Toy Horse Case Data.Rdata")
### Conjoint analysis for Q1-1 using regression 
### drop missing profile value 

conjointData1 = conjointData[complete.cases(conjointData), ]
conjointDataNA <- conjointData[is.na(conjointData$ratings),]


#create a dataframe and fill each individual's part-utilities

iterations = 200
variables = 6
output <- data.frame(matrix(ncol=variables, nrow=iterations))

for(i in 1:200) {
  a = lm(ratings~price+size+motion+style, data = subset(conjointData1, ID==i))
  output[i,1] = i
  output[i,2:6] = a$coefficient[1:5]
  output
}

colnames(output) <- c('id',"intercept", "price",'size','motion','style')

output1 = output[rep(seq_len(nrow(output)), each = 4), ]

######### Conjoint analysis for Q1-2 for predicting 4 missing profiles rating 
for (i in 1:200){
  for (j in c(3,6,10,16)){
    conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 3] = 
      output[i,2] + 
      conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 4] * output[i,3] + 
      conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 5] * output[i,4] + 
      conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 6] * output[i,5] + 
      conjointDataNA[conjointDataNA$ID == i & conjointDataNA$profile == j, 7] * output[i,6]
  }
}

#merge and reorder the dataframe by ID and profile
conjointData_full <-rbind(conjointDataNA, conjointData1)
conjointData_full <-conjointData_full[order(conjointData_full$ID, conjointData_full$profile),]

##Evaluate number of clusters to use on data with visualizations
##Arguments: 
##  toClust, the data to do kmeans cluster analysis
##  maxClusts=15, the max number of clusters to consider
##  seed, the random number to initialize the clusters
##  iter.max, the max iterations for clustering algorithms to use
##  nstart, the number of starting points to consider
##Results:
##  a list of weighted sum of squares and the pamk output including optimal number of clusters (nc)
##  to create visualizations need to print tmp
clustTest = function(toClust,print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(scale){ toClust = scale(toClust);}
  set.seed(seed);   # set random number seed before doing cluster analysis
  wss <- (nrow(toClust)-1)*sum(apply(toClust,2,var))
  for (i in 2:maxClusts) wss[i] <- sum(kmeans(toClust,centers=i,nstart=nstart,iter.max=iter.max)$withinss)
  ##gpw essentially does the following plot using wss above. 
  #plot(1:maxClusts, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
  gpw = fviz_nbclust(toClust,kmeans,method="wss",iter.max=iter.max,nstart=nstart,k.max=maxClusts) #alternative way to get wss elbow chart.
  pm1 = pamk(toClust,scaling=TRUE)
  ## pm1$nc indicates the optimal number of clusters based on 
  ## lowest average silhoutte score (a measure of quality of clustering)
  #alternative way that presents it visually as well.
  gps = fviz_nbclust(toClust,kmeans,method="silhouette",iter.max=iter.max,nstart=nstart,k.max=maxClusts) 
  if(print){
    grid.arrange(gpw,gps, nrow = 1)
  }
  list(wss=wss,pm1=pm1$nc,gpw=gpw,gps=gps)
}


##Runs a set of clusters as kmeans
##Arguments:
##  toClust, data.frame with data to cluster
##  nClusts, vector of number of clusters, each run as separate kmeans 
##  ... some additional arguments to be passed to clusters
##Return:
##  list of 
##    kms, kmeans cluster output with length of nClusts
##    ps, list of plots of the clusters against first 2 principle components
runClusts = function(toClust,nClusts,print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100){
  if(length(nClusts)>4){
    warning("Using only first 4 elements of nClusts.")
  }
  kms=list(); ps=list();
  for(i in 1:length(nClusts)){
    kms[[i]] = kmeans(toClust,nClusts[i],iter.max = iter.max, nstart=nstart)
    ps[[i]] = fviz_cluster(kms[[i]], geom = "point", data = toClust) + ggtitle(paste("k =",nClusts[i]))
    
  }
  library(gridExtra)
  if(print){
    tmp = marrangeGrob(ps, nrow = 2,ncol=2)
    print(tmp)
  }
  list(kms=kms,ps=ps)
}

##Plots a kmeans cluster as three plot report
##  pie chart with membership percentages
##  ellipse plot that indicates cluster definitions against principle components
##  barplot of the cluster means
plotClust = function(km,toClust,discPlot=FALSE){
  nc = length(km$size)
  if(discPlot){par(mfrow=c(2,2))}
  else {par(mfrow=c(3,1))}
  percsize = paste(1:nc," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
  pie(km$size,labels=percsize,col=1:nc)
  
  clusplot(toClust, km$cluster, color=TRUE, shade=TRUE,
           labels=2, lines=0,col.clus=1:nc); #plot clusters against principal components
  
  if(discPlot){
    plotcluster(toClust, km$cluster,col=km$cluster); #plot against discriminant functions ()
  }
  rng = range(km$centers)
  dist = rng[2]-rng[1]
  locs = km$centers+.05*dist*ifelse(km$centers>0,1,-1)
  bm = barplot(km$centers,beside=TRUE,col=1:nc,main="Cluster Means",ylim=rng+dist*c(-.1,.1))
  text(bm,locs,formatC(km$centers,format="f",digits=1))
}

partworth = output[2:6] #B0 and B1

checks = clustTest(output[2:6],print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)
clusts = runClusts(output[2:6],c(2,3,4,9),print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)

## so the optimal clusters is 3 
checks = clustTest(output[2:6],print=TRUE,scale=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)
clusts = runClusts(output[2:6],c(3),print=TRUE,maxClusts=15,seed=12345,nstart=20,iter.max=100)
plotClust(clusts[[1]][[1]],partworth)
## from the grahics we can conclude that all three segments value price the most.For segement 1, 
## they prefer low price the most, which is $199.99. prefer 26inches, bouncing and racing. 
## for segment 2, they prefer low price the most, prefer 18 inches, rocking and glamour.
## for segment 3, they prefer 26 inches the most, prefer glamours style, prefer low prce and 
## rocking motion.
# these three clusters like 16(40%),14(26%),4(34%)

####QC

#merge two demograpihc and conjoint Data without na for post hoc benefit segmentation 
conjointDemo <- merge(conjointData1, respondentData, by = 'ID')

#without segmentation
summary(lm(ratings~price+size+motion+style, data = conjointData))
#when children's age is 2 years
summary(lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 0)))
#when children's age is 3-4 years
summary(lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 1)))
#when gender is male =0 
summary(lm(ratings~price+size+motion+style, data = subset(conjointDemo, gender == 0)))
#when gender is female =1 
summary(lm(ratings~price+size+motion+style, data = subset(conjointDemo, gender == 1)))
### for 2 and 3-4 years old, they all want lower price and 260inches. but comparing to 2 years old, 3-4 years old care about more 
## 
# for male and female, they all want lower price and 26-inches, but comapring to female, male care more about price, female 
#care more about size. they all care about style more than motion, but male prefer bouncing motion and racing style, while female
# prefer rocking motion and glamour style.

##  with more specific segmentation 
summary(lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 0 & gender == 0)))# for 2 years male, 1110, profile 8 
summary(lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 0 & gender == 1)))# for 2 years female, 1111, profile 16
summary(lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 1 & gender == 0)))# for 3-4 years male, 1100, profile 4
summary(lm(ratings~price+size+motion+style, data = subset(conjointDemo, age == 1 & gender == 1)))# for 3-4 years female, 1111,profile 16
#for two years old child, for male and female, they all want lower price,26-inches and rocking motion. but comparing to female, male care more 
# about price, care less for size and motion. Male prefer racing style and female prefer glamour style. 
# for three-four years old child, for male and female, they all want lower price,26-inches. but comparing to female, male care more 
#about price, care less for size. Male prefer bouncing motion and racing style while female prefer rocking motion and glamour style.


# for male, 

#Base on age and gender separately, with significance test
#base on gender
summary(lm(ratings~price+size+motion+style+gender:price+gender:size+gender:motion+gender:style, data = conjointDemo))
#base on age
summary(lm(ratings~price+size+motion+style+age:price+age:size+age:motion+age:style, data = conjointDemo))




#### Q4 
## firstly we need to figure out the first choice for each ID 
y = conjointData_full[which.max(Y),]
Y = subset(conjointData_full, ID==1)[,3]
## wrong, cause conjointdata_full[which.max] return the original data which contains all ID and always as ID=1,16

Y = subset(conjointData_full, ID==2)[,3]
y = subset(conjointData_full, ID==2)[which.max(Y),]

choice = data.frame(ID = rep(NA,200), profile = rep(NA,200), ratings = rep(NA,200),  price = rep(NA,200), size = rep(NA,200), motion = rep(NA,200),
               style = rep(NA,200))
for(i in 1:200) {
  Y = subset(conjointData_full, ID==i)[,3]
  choice[i,] = subset(conjointData_full, ID==i)[which.max(Y),]
}
choice

### to calculate market shares -version 1 
unique(choice$profile)
nrow(subset(choice, choice$profile == 16))/200

choiceProfile = c(16,6,12,14,4,2,8,15,13)

mktShare = data.frame()
for (z in 1:length(choiceProfile)) {
  marketshare = nrow(subset(choice, profile == choiceProfile[z]))/nrow(choice)
  mktShare[z,1] = choiceProfile[z]
  mktShare[z,2] = marketshare
}
mktShare
colnames(mktShare) = c('Profile', "market_share")
## sort to get market share
mktShare <- mktShare[order(mktShare$market_share, decreasing = TRUE),]
## current products profile 3 is not on the list, and even profile 15 is on the list, but the 
## its market share is very low.
## export choice table to make Tableau graphics 
write.csv(choice, file = 'choice.csv')

#calculate market share - version 2 
## Rate Chart
Rate = as.data.frame(c(1:200))

for (i in 1:200){
  ID_data = conjointData_full[conjointData_full$ID==i,]
  Rate[i,c(2:17)] = t(ID_data[,3])
}

colnames(Rate) <- c('ID','P1','P2','P3','P4',
                    'P5','P6','P7','P8','P9',
                    'P10','P11','P12','P13','P14',
                    'P15','P16')
RateData <- Rate[,-1]
# finallly we can use professor definded code to calculate market share 
simFCSharesA = function(scen,data,ascend=TRUE){ 
  inmkt = data[,scen] #construct the subsetted matrix of options
  scores = matrix(c(rep(0,length(scen))))
  if(ascend){ #if ranks 1 is best
    for (i in seq(1:nrow(inmkt))) {
      z = min(inmkt[i,])
      if (sum(inmkt[i,]==z) == 1) {
        scores[inmkt[i,]==z] = scores[inmkt[i,]==z] + 1
      }
      else {
        j = sum(inmkt[i,]==z)
        scores[inmkt[i,]==z] = scores[inmkt[i,]==z] + 1/j
      }
    }
  } else { #else the best rank is the largest number
    for (i in seq(1:nrow(inmkt))) {
      y = max(inmkt[i,])
      if (sum(inmkt[i,]==y) == 1) {
        scores[inmkt[i,]==y] = scores[inmkt[i,]==y] + 1
      }
      else {
        j = sum(inmkt[i,]==y)
        scores[inmkt[i,]==y] = scores[inmkt[i,]==y] + 1/j
      }
    }
  }
  marketShare = data.frame(t(scores/i))
  colnames(marketShare) = colnames(inmkt)
  marketShare
}
simFCSharesA(1:16, RateData[])


#create scenarios (add products with higher market shares and use different conbination plans)
scen0 = c(13, 5, 7) #current market our products are 13 and 5, competitor has 7 
scen1 =c(13, 5, 4, 14, 16, 7)
scen1 =c(13, 5, 4, 14, 16, 8) 
scen2 = c(13, 5, 4, 7,8) 
scen2 = c(13, 5, 4, 8) 
scen3 = c(13,5,7,4)
scen4 = c(13,5,4,8)
scen4 = c(13,5,4,7,8)
scen3 = c(13,5,14,8) 
scen3 = c(13,5,14,7)
scen4 = c(13,5,16,8) 
scen4 = c(13,5,16,7) 
scen5 = c(13,4,14, 8) 
scen5 = c(13,4,14, 7) 
scen6 = c(13,4,16, 8) 
scen6 = c(13,4,16, 7) 
scen6 = c(13,4, 8)
scen7 = c(13,4,16, 8)
scen8 = c(13, 8)
scen9 = c(4,5, 8) 
scen10 =c(13,5,8)
scen11 = c(13,4,14,16,7)
scen12 = c(5,4,14,16,7)

simFCSharesA(scen0, RateData[])
simFCSharesA(scen1, RateData[])
simFCSharesA(scen2, RateData[])
simFCSharesA(scen3, RateData[])
simFCSharesA(scen4, RateData[])
simFCSharesA(scen5, RateData[])
simFCSharesA(scen6, RateData[])
simFCSharesA(scen7, RateData[])
simFCSharesA(scen8, RateData[])
simFCSharesA(scen9, RateData[])
simFCSharesA(scen10, RateData[])
simFCSharesA(scen11, RateData[])
sc0_pr = (0.385*4000*78.99+0.42*4000*78.99)-20000*2
sc1_pr = (0.395*4000*78.99+0.42*4000*78.99+0.185*4000*66.99)-20000*5-20000/3*3
sc1_pr = (0.365*4000*78.99+0.4*4000*78.99+0.12*4000*66.99)-20000*5-20000/3*3
sc2_pr = (0.395*4000*78.99+0.42*4000*78.99+0.185*4000*66.99)-20000*3-20000/3*1
sc2_pr = (0.365*4000*78.99+0.4*4000*78.99+0.12*4000*66.99)-20000*3-20000/3*1
sc3_pr = (0.475*4000*78.99+0.495*4000*78.99)-20000*3-20000*1/3*1
sc4_pr = (0.47*4000*78.99+0.495*4000*78.99+0.01*4000*54.99)-20000*3-20000/3*1 
sc5_pr = (0.705*4000*78.99+0.26*4000*66.99+0.035*4000*62.99)-20000*3-20000/3*2
sc6_pr = (0.725*4000*78.99+0.275*4000*66.99)-20000*2-20000/3*1
sc7_pr = (0.725*4000*78.99+0.275*4000*66.99)-20000*3-20000/3*2
sc8_pr = (0.935*4000*78.99)-20000*1 
sc9_pr = (0.215*4000*66.99+0.785*4000*78.99)-20000*2-20000/3*1
sc10_pr = (0.475*4000*78.99+0.495*4000*78.99)-20000*2-20000*1/3*1

sc0_prl = (0.385*4000*78.99+0.42*4000*78.99)*5-20000*2*5
sc9_prl= (0.215*4000*66.99+0.785*4000*78.99)*5-20000*2*5-20000/3*1
########################################################################

# ## Turf  
# measFrequency = function(data){
#     ret = sum(data)
#     ret
# }
# 
# evalNext = function(nextSet,set,data,measure=measReach){
#     vals = numeric(length(nextSet)) #set up storage for return value
#     for(k in 1:length(nextSet)){ #loop over the options in nextSet
#         if(length(set)==0){         #if no existing options
#             vals[k] = measure(simFCDecisions(nextSet[k],data)) 
#         } else {                    #if existing options
#             vals[k] = measure(simFCDecisions(c(set,nextSet[k]),data))
#         }
#     }
#     vals
# }
# 
# evalFull = function(fullSet,data,origSet=numeric(0),measure=measFrequency){
#     curSet = origSet; #the current set of included options
#     remSet = fullSet[!(fullSet%in%origSet)]; #the remaining set of options to consider
#     K = length(remSet)
#     optVals = numeric(K); #create storage for the optimal values (optVals)
#     ordSet = numeric(K); #create storage for ordered set
#     for(i in 1:K){          #loop over the remaining set consider
#         tmpVals = evalNext(remSet,curSet,data,measure); #calculate vector of next evaluations
#         k = which.max(tmpVals) #pick the option that gives max measure, note will pick first case if a tie!
#         optVals[i] = tmpVals[k] #add optimal value
#         ordSet[i] = remSet[k]   #add index of option that creates optimal value
#         curSet = c(curSet,ordSet[i]); #add optimal next option to current set
#         remSet = remSet[-k];          #delete optimal next option from remaining set
#     }
#     #creaets a "TURF object" containing ordSet, optVals, origSet, origVal, measure, and pnames
#     turf = list(ordSet=ordSet,optVals=optVals,origSet=origSet,origVal=measure(simFCDecisions(origSet,data)),measure=measure,pnames=colnames(data))
#     class(turf)="TURF" #makes the list into a TURF object so that can call plot.TURF
#     turf  #return turf
# }
# 
# #creates ggplot barplot for a turf object
# plot.TURF=function(turf,...){
#     if(class(turf)!="TURF"){
#         cat("Object not a turf.")
#     } else {
#         df = with(turf,data.frame(vals = c(origVal,optVals),titles=paste(0:length(ordSet),c("Original",pnames[ordSet]),sep=":")))
#         # with(turf,barplot(c(origVal,optVals),names.arg=c("Original",pnames[ordSet])))
#         dodge = position_dodge(width=.75); ##to form constant dimensions positioning for all geom's
#         gp = ggplot(df,aes(y=vals,x=titles,label=vals))
#         gp + geom_bar(position=dodge,stat="identity",col=1,fill=4,width=.75) + 
#             aes(reorder(titles, vals)) + 
#             xlab("Flavor Sets") + ylab("")
#     }
# }
# 
# turf = evalFull(c(1:23),RateData,c(1),measure = measFrequency)
# plot(turf)

## current products' profits 
profit_13=mktShare[9,2]*4000*139.99-20000-mktShare[9,2]*4000*33
profit_5 
scen0 = c(13, 5)

# other scenarios according to choice data marketshare 
#current profile 13 and 5
scen0 = c(13, 5,7)#current situation
scen1 =c(13, 5, 16,4,14,8)#current situation with all segment preference and highest market share
scen2 = c(13, 16,4,14,8)#delete 5 which has no market share 
scen3 = c(16,4,14,8)# delet 13 which has lowest market share among others
scen4 = c(16,4,8)#delete 4
scen5 = c(16,14,8)#delete 14
scen6 = c(4, 14,8)#delete 16
# calculate profit of different scenarios 
sc0_pr = (0.005*4000)*(111.99-33)-20000*2
sc1_pr = 0.005*4000*78.99+0.3*4000*54.99+0.265*4000*66.99+0.13*4000*62.99-20000*5-20000/3*3
sc2_pr = 0.005*4000*78.99+0.3*4000*54.99+0.265*4000*66.99+0.13*4000*62.99-20000*4-20000/3*3
sc3_pr = 0.3*4000*54.99+0.265*4000*66.99+0.13*4000*62.99-20000*3-20000*1/3*3
sc4_pr = 0.3*4000*54.99+0.265*4000*66.99-20000*2-20000*2*1/3
sc5_pr = 0.3*4000*54.99+0.13*4000*62.99-20000*2-20000*2*1/3
sc6_pr = 0.265*4000*66.99+0.13*4000*62.99-20000*2-20000*2*1/3
# the highest profit is Scenario 3, which has not considered competitiors responses yet. 
#if considering long-term profits 
sc0_prl = (0.005*4000)*(111.99-33)*5-20000*2*5
sc1_prl = (0.005*4000*78.99+0.3*4000*54.99+0.265*4000*66.99+0.13*4000*62.99)*5-20000*5*5-20000/3*3
sc2_prl = (0.005*4000*78.99+0.3*4000*54.99+0.265*4000*66.99+0.13*4000*62.99)*5-20000*4*5-20000/3*3
sc3_prl = (0.3*4000*54.99+0.265*4000*66.99+0.13*4000*62.99)*5-20000*3*5-20000*1/3*3
sc4_prl = (0.3*4000*54.99+0.265*4000*66.99)*5-20000*2*5-20000*2**1/3
sc5_prl = (0.3*4000*54.99+0.13*4000*62.99)*5-20000*2*5-20000*2**1/3
sc6_prl = (0.265*4000*66.99+0.13*4000*62.99)*5-20000*2*5-20000*2*1/3
# for 5 years long-term profit, scenario 3 also drive highest profit 


#if considering competitiors responses, since our product sets do not conflict with competitors product(which is profile 7 or)






