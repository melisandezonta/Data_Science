# prediction3 <- function(df,attribute){
#   a = numdf[[attribute]] 
#   numdf[[attribute]] = NULL
#   numdf[[attribute]] = a
#   
#   car = names(numdf)[1:length(names(numdf))-1]
#   
#   car = as.list(car)
#   car1 = str_c("log10(",car,")", collapse = '+')
#   car1 = paste("log10(", attribute,")", "~",car1)
#   car1 = paste(car1,"+0")
#   M = lm(as.formula(car1),numdf)
#   return(M)
# }

# prediction5 <- function(numdf,attribute){
# a = numdf[[attribute]] 
# numdf[[attribute]] = NULL
# numdf[[attribute]] = a
# 
# car = names(numdf)[1:length(names(numdf))-1]
# 
# car = as.list(car)
# car1 = str_c(car,collapse='*') 
# car1 = paste(attribute,"~",car1)
# car1 = paste(car1,"+0")
# M = lm(as.formula(car1),numdf)
# return(M)
# }

score.dictionnary = function(sentences, dic.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, dic.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    dic.matches = match(dic.words, words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    dic.matches = !is.na(dic.matches)
    dic.matches = as.numeric(dic.matches)
    
    return(dic.matches)
    
  }, dic.words,.progress=.progress )
  
  scores.df = data.frame(score=score)
  return(scores.df)
}



score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# feelings = unique((get_sentiments("nrc"))$sentiment)
# positive_feelings = c("joy","surprise","positive","anticipation")
# nrcpositive = c()
# for (i in seq(1,4)){
#   nrcpositive = rbind(nrcpositive,(get_sentiments("nrc") %>% 
#   filter(sentiment == positive_feelings[i])))
# }
# 
# negative_feelings = c("trust","fear","negative","anger","disgust")
# nrcnegative = c()
# for (i in seq(1,4)){
#   nrcnegative = rbind(nrcnegative,(get_sentiments("nrc") %>% 
#   filter(sentiment == negative_feelings[i])))
# }

nrcpositive = c()
for (i in seq(1,4)){
  nrcpositive = rbind(nrcpositive,(get_sentiments("bing") %>%
                                     filter(sentiment == "positive")))
}

nrcnegative = c()
for (i in seq(1,4)){
  nrcnegative = rbind(nrcnegative,(get_sentiments("bing") %>%
                                     filter(sentiment == "negative")))
}

score = score.sentiment(df$tomatoConsensus, nrcpositive[,1], nrcnegative[,1], .progress='none')
score1 = score.sentiment(df$Plot, nrcpositive[,1], nrcnegative[,1], .progress='none')
