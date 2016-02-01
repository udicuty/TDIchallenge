#For the answer below I took the "brute force" approach. Namely, I am measuring the mean and sd from a distribution of 10^6 jackblack games. 
# I recognize that this is not the best way to do it. I would prefer to write a recursive function that generates all possible combinations that reach/surpass the target.
# Afterwards, calculating the probability for each combination (by raising 1/7 to the power of draws) and multiply by the corresponding score, to get the mean, similiarly I can exteact the sd.
#I started with this approach but due to time constraints had to ditch it in favour of the brute force approach
#To compensate for processing time I have also experimented with paralelling the run utlizing 4 CPU cores

require(dplyr)
#Creating deck
deck<-c(1,2,4,8,16,32,64)
deck11<-c(11,2,4,8,16,32,64)


#Jackblack game function
jackblack<-function(deck,target,draws=0) { #Plays jackblack, returns score and number of draws to target
  if (target<=0) { #Exiting when hitting or surpassing target
    return(c(-target,draws))
  }
  draw<-sample(deck,1,replace=TRUE) #Drawing a card with replacement (Deck is large)
  target<-target-draw #Getting closer to the target with each draw.
  draws<-draws+1
  jackblack(deck,target,draws)
}


rounds<-1000000 # Number of games to play
N<-21

game_results<-NULL
for (i in 1:rounds) {

  game_results<-c(game_results,jackblack(deck,N))

}

scores<-game_results[seq(1,length(game_results),2)]
draws<-game_results[seq(2,length(game_results),2)]
df<-data.frame(scores,draws)
mean(df$scores) #Q1 The more we play the mean and sd are closer to 25 and 19.25 respectively  
sd(df$scores)



draw8<-filter(df,draws>=8) #All draws>=8 games
score5<-sum(draw8$score<=5) # Number of times scores <=5 when drawing 8 cards 
cond_p<-score5/nrow(draw8) #Dividing number of times with total draws
cond_p # Cond_p approaches 0.33 




########################################################
#Paralelled version
##########################################################3
require(parallel)
cores<-4
cl <- makeCluster(cores)
varlist=c("jackblack","deck","deck11")
clusterExport(cl=cl, varlist=varlist,envir=environment())



r<-1:1000 #Each CPU core would be accessed r times
  
  game_results<-parSapply(cl,r,function (r)  { #Parralleling
    
    game_results<-NULL
    
    for (i in 1:1000) { # Number of games assigned to each CPU core total: r*i = 1000*1000=10^6
      game_results<-c(game_results,jackblack(deck11,1000))
    }
    
    return(game_results)
    
  })

game_results<-as.numeric(game_results)
scores<-game_results[seq(1,length(game_results),2)] #Scores are at the odd indices, number of draws are at the even indices
draws<-game_results[seq(2,length(game_results),2)]
df<-data.frame(scores,draws)
mean(df$scores)
sd(df$scores)

#Conditional probability
draw8<-filter(df,draws>=8) #All draws>=8 games
score5<-sum(draw8$score<=5) # Number of times scores <=5 when drawing 8 cards 
cond_p<-score5/nrow(draw8) #Dividing number of times with total draws
cond_p # Cond_p approaches 0.33 



