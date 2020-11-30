busst<-read.csv("business_steakhouse.csv")
pos<-read.csv('pos_bsns.csv')
neg<-read.csv('neg_bsns.csv')
negmore<-neg[c(58,198),]
posr<-rbind(pos,negmore)
posmore<-pos[225,]
negr<-rbind(neg,posmore)
posr[c(313,314),7:10]<-c(0,0,0,0)
negr[314,7:10]<-c(0,0,0,0)
posr<-rbind(posr[1:57,],posr[313,],posr[58:196,],posr[314,],posr[197:312,])
negr<-rbind(negr[1:226,],negr[314,],negr[227:313,])
steak<-cbind(posr[1:10],negr[7:10],posr[11],negr[11])

for(i in 1:dim(steak)[1]){
  if(as.vector(steak$state[i])=="PA"){
    steak[i,17]=1
  }else{
    steak[i,17]=0
  }
}
for(i in 1:dim(steak)[1]){
  if(as.vector(steak$state[i])=="OH"){
    steak[i,18]=1
  }else{
    steak[i,18]=0
  }
}
for(i in 1:dim(steak)[1]){
  if(as.vector(steak$state[i])=="IL"){
    steak[i,19]=1
  }else{
    steak[i,19]=0
  }
}
names(steak)<-c("index","business_id","name",'city','state','stars','v1','v2','v3','v4','v5','v6','v7','v8','postext','negtext','pa','oh','il')
for(i in 11:14){
  steak[,i]<-steak[,i]*steak$negtext/(steak$postext+steak$negtext)
}
for(i in 7:10){
  steak[,i]<-steak[,i]*steak$postext/(steak$postext+steak$negtext)
}


  
mtable<-left_join(busst,steak)
m<-drop_na(mtable)
write.csv(m,"finalsteak.csv")

