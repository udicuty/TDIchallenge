#Creating deck
deck<-c(1,2,4,8,16,32,64)

card<-list()
a<-NULL
sum<-0
N<-1000
for (i in 1:500000) {
  while (sum<N) {
    sum<-sum+sample(deck,1,replace =F)  
  }
  a[i]<-sum-N
  sum<-0
}

hist(a)
mean(a)
c<-NULL
for (i in 1:100000) {
  while (sum<N) {
    
    card[i]<-sample(deck,1,replace =T)
    sum<-sum+card[i]
  }
  a[i]<-sum-N
  sum<-0
}



sum-N
length(a)
hist(a)
mean(a)
sd(a)
mean(deck)
sd(deck)

sd(a)
sd(deck)

coins = c(1, 5, 10, 25, 50)

count<-function (remainder) {
  if (remainder<0) return (0)
  if (remainder==0) return (1)
  for (coin in coins) {
    sum(count(remainder - coin))
  }
}

count(50)

def count(remainder):
  if remainder < 0:
  return 0
if remainder == 0:
  return 1
return sum(count(remainder - coin) for coin in coins)
count(50)






function (draw) 

