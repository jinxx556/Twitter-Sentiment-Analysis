## https://rpubs.com/abNY2015/90345
library(RCurl)
require(twitteR)
library(ROAuth)
library(base64enc)
library(httr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)

setwd('proj2/')
api_key = "uFLF6MzeRIQuPDxXWFW2PfVMB" 
api_secret = "FUySQ23gZlcKlJMZfk99zs4ZvMpmmqYD74HK4Z4enzyJ5YxO8q" 
access_token = "705500923087290368-th97PdUNaDqjJnQ5xgxPwxnBptDO2YZ" 
access_token_secret = "Pp54HDug1m3OM2awI4rbnBpLe6RiAB43Be421YF8vqDHy"
setup_twitter_oauth(api_key,api_secret,access_token,
                    access_token_secret)

positive=scan('positive-words.txt',what='character',comment.char=';')
negative=scan('negative-words.txt',what='character',comment.char=';')
positive[20:30]
negative[500:510]

start <- proc.time()
findfd= "CyberSecurity"
number= 5000
tweet=searchTwitter(findfd,number)
proc.time() - start

tweetT=lapply(tweet,function(t) t$getText())
head(tweetT,5)


tryTolower = function(x)
{
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, "error")){
    y = tolower(x)
  }else{
    y = tolower(iconv(x, "latin1", "ASCII", sub=""))
  }
  return(y)
}

clean=function(t){
  t=gsub('[[:punct:]]','',t)
  t=gsub('[[:cntrl:]]','',t) 
  t=gsub('\\d+','',t)
  t=gsub('[[:digit:]]','',t)
  t=gsub('@\\w+','',t)
  t=gsub('http\\w+','',t)
  t=gsub("^\\s+|\\s+$", "", t)
  t=sapply(t,function(x) tryTolower(x))
  t=str_split(t," ")
  t=unlist(t)
  return(t)
}

tweetclean=lapply(tweetT,function(x) clean(tryTolower(x)))
head(tweetclean,5)

returnpscore=function(tweet) {
  pos.match=match(tweet,positive)
  pos.match=!is.na(pos.match)
  pos.score=sum(pos.match)
  return(pos.score)
}

positive.score=lapply(tweetclean,function(x) returnpscore(x))

pcount=0
for (i in 1:length(positive.score)) {
  pcount=pcount+positive.score[[i]]
}
pcount

## 
poswords=function(tweets){
  pmatch=match(tweets,positive)
  posw=positive[pmatch]
  posw=posw[!is.na(posw)]
  return(posw)
}

negwords=function(tweets){
  pmatch=match(tweets,negative)
  posw=negative[pmatch]
  posw=posw[!is.na(posw)]
  return(posw)
}


words=NULL
pdatamart=data.frame(words)

for (t in tweetclean) {
  pdatamart=c(poswords(t),pdatamart)
}
head(pdatamart,10)

words=NULL
ndatamart=data.frame(words)

for (t in tweetclean) {
  ndatamart=c(negwords(t),ndatamart)
}
head(ndatamart,10)


pwords <- unlist(pdatamart)
nwords <- unlist(ndatamart)
dpwords=data.frame(table(pwords))
dnwords=data.frame(table(nwords))

dpwords=dpwords%>%
  mutate(pwords=as.character(pwords))%>%
  filter(Freq>25)


ggplot(dpwords,aes(pwords,Freq))+geom_bar(stat="identity",fill="lightblue")+theme_bw()+
  geom_text(aes(pwords,Freq,label=Freq),size=4)+
  labs(x="Major Positive Words", y="Frequency of Occurence",title=paste("Major Positive Words and Occurence in \n '",findfd,"' twitter feeds, n =",number))+
  geom_text(aes(1,5,label=paste("Total Positive Words :",pcount)),size=4,hjust=0)+theme(axis.text.x=element_text(angle=45))



###

tweetscorpus=Corpus(VectorSource(tweetclean))
tweetscorpus=tm_map(tweetscorpus,removeWords,stopwords("english"))
stopwords("english")[30:50]
wordcloud(tweetscorpus,scale=c(5,0.5),random.order = TRUE,rot.per = 0.20,use.r.layout = FALSE,colors = brewer.pal(6,"Dark2"),max.words = 300)


##
dtm=DocumentTermMatrix(tweetscorpus)
# #removing sparse terms
dtms=removeSparseTerms(dtm,.99)
freq=sort(colSums(as.matrix(dtm)),decreasing=TRUE)
#get some more frequent terms
findFreqTerms(dtm,lowfreq=200)

wf=data.frame(word=names(freq),freq=freq)
wfh=wf%>%
  filter(freq>=75,!word==tolower(findfd))

ggplot(wfh,aes(word,freq))+geom_bar(stat="identity",fill='lightblue')+theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  geom_text(aes(word,freq,label=freq),size=4)+labs(x="High Frequency Words ",y="Number of Occurences", title=paste("High Frequency Words and Occurence in \n '",findfd,"' twitter feeds, n =",number))+
  geom_text(aes(1,max(freq)-100,label=paste("# Positive Words:",pcount,"\n","# Negative Words:",ncount,"\n",result(ncount,pcount) )),size=5, hjust=0)
