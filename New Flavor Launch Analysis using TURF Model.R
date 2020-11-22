rm(list=ls())

###########load data##########
setwd("C:/Users/ZJ/Desktop/analytics design and application/assignment/assignment 2")
data = read.csv("survResponses.csv")

### remove V10 = 0 not finished ones ###
yogurt <- subset(data, data$V10 == 1)
yogurt <- data
#####Q3###########################
###select columns about flavor questions
yogurt1 <- yogurt[15:37]
#### get respondents preferences
yogurt1[yogurt1 == 0] <- 5
yogurt[15:37][yogurt[15:37] == 2] <- 0
yogurt[15:37][yogurt[15:37] == 1] <- 2
### if NA should be 0###
yogurt1[yogurt1 == 0] <- 1
yogurt1[yogurt1 == 2] <- 0
yogurt1[is.na(yogurt1)] <- 0

yogurtnew <- yogurt1

## drop NA response which are not meaningful for further analysis
yogurtnew =na.omit(yogurt1)

## change the clolumn name to the flavor
colnames(yogurtnew) <- c('Almond','Banana','Black Cherry', 'Blueberry','Caramel', 'Chai','Chocolate','Cinnamon',
                         'Coconut','Honey','Key Lime Pie','Lemon','Mango','Maple', 'Peach','Vanilla Banana','Pineapple','Plain', 'Pomegranate', 
                         'Raspberry','Strawberry', 'Strawberry Banana','Vanilla')

##### Get the frequency of Survey respondents denoting with average times to purchase this yogurt flavor
avg_fre1 <- colSums(yogurtnew)/nrow(yogurtnew)
avg_fre2 <- as.data.frame(avg_fre1)

calcUnitShares = function(decisions){
  colSums(yogurtnew)/sum(yogurtnew) #assumes that total decisions is market size
}

simFCShares=function(scen,data,ascend=TRUE){
  decs = simFCDecisions(scen,data,ascend) #determine decisions
  calcUnitShares(decs) #calculate shares and return
}

simFCShares(statusQuo,data[,2:6])

### plot to check
library(ggplot2)
avg_fre2$flavor <- rownames(avg_fre2)
avg_fre2

ggplot(data=avg_fre2, aes(x=flavor, y=avg_fre1)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=flavor), vjust=-0.8, size=3.0)+
  theme_minimal()

### sort to get the preferences 
preferences <- sort(avg_fre1,decreasing=TRUE)
preferences1 <- as.data.frame(preferences)
preferences1$flavor <- rownames(preferences1)
preferences1

### plot to check
ggplot(data=preferences1, aes(x=reorder(flavor,-preferences), y=preferences)) +
  geom_bar(stat="identity", fill="steelblue",width = 0.6, position = position_dodge(0.7))+
  geom_text(aes(label=flavor), vjust=-1.0, size=3.0)+
  theme_minimal()

### survey respondents' top 10 preferences are Blueberry,Raspberry,Pomegranate,Strawberry Banana,Black Cherry, Peach
#Pineapple,Mango,Plain,Lemon

##################Q4 TURF ANALYSIS#############
yogurt2 <- yogurtnew

#assign the regulr and occasionally purchase as 1, 
#which means the survey respondt would buy this flavor while assign never as 0 , denoting 
# that this survey respondt would not buy this flavor
yogurt2[yogurt2 == 2] <- 1
yogurt2[yogurt2 == 5] <- 1
yogurt2[yogurt2 == 0] <- 0


measReach = function(data){
  if(is.null(dim(data))){ #if data is a vector
    ret = sum(data>0,na.rm=TRUE)/length(data)
  } else if(ncol(data)==1){ #if data has only one column
    ret = sum(data>0,na.rm=TRUE)/length(data)
  }
  else { #if data has multiple columns
    ret = sum(apply(data>0,1,any),na.rm=TRUE)/nrow(data)
  }
}


evalNext = function(nextSet,set,data,measure=measReach){
  vals = numeric(length(nextSet)) #set up storage for return value
  for(k in 1:length(nextSet)){ #loop over the options in nextSet
    if(length(set)==0){         #if no existing options
      vals[k] = measure(data[,nextSet[k]]) 
    } else {                    #if existing options
      vals[k] = measure(data[,c(set,nextSet[k])])
    }
  }
  vals
}


evalFull = function(fullSet,data,origSet=numeric(0),measure=measReach){
  curSet = origSet; #the current set of included options
  remSet = fullSet[!(fullSet%in%origSet)]; #the remaining set of options to consider
  K = length(remSet)
  optVals = numeric(K); #create storage for the optimal values (optVals)
  ordSet = numeric(K); #create storage for ordered set
  for(i in 1:K){          #loop over the remaining set consider
    tmpVals = evalNext(remSet,curSet,data,measure); #calculate vector of next evaluations
    k = which.max(tmpVals) #pick the option that gives max measure, note will pick first case if a tie!
    optVals[i] = tmpVals[k] #add optimal value
    ordSet[i] = remSet[k]   #add index of option that creates optimal value
    curSet = c(curSet,ordSet[i]); #add optimal next option to current set
    remSet = remSet[-k];          #delete optimal next option from remaining set
  }
  #creaets a "TURF object" containing ordSet, optVals, origSet, origVal, measure, and pnames
  turf = list(ordSet=ordSet,optVals=optVals,origSet=origSet,origVal=measure(data[,origSet]),measure=measure,pnames=colnames(data))
  class(turf)="TURF" #makes the list into a TURF object so that can call plot.TURF
  turf  #return turf
}
#creates ggplot barplot for a turf object
plot.TURF=function(turf,...){
  if(class(turf)!="TURF"){
    cat("Object not a turf.")
  } else {
    df = with(turf,data.frame(vals = c(origVal,optVals),titles=paste(0:length(ordSet),c("Original",pnames[ordSet]),sep=":")))
    #with(turf,barplot(c(origVal,optVals),names.arg=c("Original",pnames[ordSet])))
    dodge = position_dodge(width=.75); ##to form constant dimensions positioning for all geom's
    gp = ggplot(df,aes(y=vals,x=titles))
    gp + geom_bar(position=dodge,stat="identity",col=1,fill=4,width=.75)
  }
}


brandsPurch = yogurt2
turf = evalFull(c(1:23),brandsPurch,c(4, 10, 15, 18, 21, 23))
plot(turf)
turf
#try different combanination 
turf = evalFull(c(1:23),brandsPurch,c(4,18))
plot(turf)
turf

turf = evalFull(c(1:23),brandsPurch,c(4,10,18))
plot(turf)
turf


#so the seventh flavor which is the new flavor should be pineapple which greatly increase the reach of flavors sets 
