draws=c() ##draws took by the dice
dice=c() ##dice number probabilities  


p_b <- function(draws,dice) {
  prob=c()
  for (draw in draws){
    for (i in 1:length(dice))
      {
        if (draw>dice[i]) {
      prob[i]=0
        } 
      else if (is.na(prob[i])) {prob[i]=(1/6)*(1/dice[i])}
      else  {prob[i]=(prob[i]*(1/dice[i]))}
        }
  } 
  def=sum(prob)
  for (i in 1:length(prob)){
    prob[i]=prob[i]/def
  }
    options(scipen=999)
    prob=data.frame(t(prob))
    names(prob)=dice
    row.names(prob)='probs'
    return(prob)
  }

p_b(draws,dice)

