
#For the answer below I took the "brute force" approach, generating distribution of 10^6 jackblack games, and measuring the mean and sd of the scores.
# I recognize that this is not the best way to do it. I would prefer to write a recursive function that generates all possible combinations that reach/surpass the target.
# Then calculate probability for each combination (raising 1/7 to the power of draws) and then multiply by the score, the answer would be the mean score.
#Similiarly, The sd would then be calculated.....
#I started with this approach but due to time constraints had to ditch it in favour of the brute force approach,
#to compensate for processing time I paralleled the process and using 4 cores. 



require(dplyr)
#Creating deck
deck<-c(1,2,4,8,16,32,64)
deck11<-c(11,2,4,8,16,32,64)




#Jackblack game function
jackblack<-function(deck,target,draws=0) { #Plays jackblack, returns score and number of draws
  if (target<=0) { #Exiting when hitting or surpassing target
    return(c(-target,draws))
  }
  draw<-sample(deck,1,replace=TRUE) #Drawing a card with replacement (Deck is large)
  target<-target-draw #Getting closer to the target with each draw.
  draws<-draws+1
  jackblack(deck,target,draws)
}




# Target=21, regular deck, 10^5 games

scores_list<-list()
for (i in 1:100000) { 
      scores_list[[i]]<-jackblack(deck11,21) #Recording score and number of draws for each game 
    }
df<-t(as.data.frame(scores_list,row.names = c("score","draws")))
df<-as.data.frame(df)
mean(df$score)
sd(df$score)




draw8<-filter(df,draws>=8) #All draws>=8 games
score5<-sum(draw8$score<=5) # sum of all scores <=5 when drawing 8 cards 
cond_p<-score5/nrow(draw8)
cond_p # Cond_p approaches 0.33 
#The more we play the mean and sd are closer to 25 and 19.25 respectively  



# Target=21, deck, 10^5 games

scores<-NULL
draws<-NULL
for (i in 1:100000) { 
  scores[i]<-jackblack(deck,1000) #Recording score for each game 
}
#The more we play the mean and sd are closer to 21 and 18 respectively  

mean(scores)
sd(scores)














jackblack<-function(deck,target,draws=NULL) {
  if (target<=0) { #Exiting when hitting or surpassing target
    return(c(-target,draws))
  }
  draw<-sample(deck,1)
  draws<-c(draws,draw)
  target<-target-draw #Getting closer to the target with each draw: sample(deck,1)
  jackblack(deck,target,draws)
}