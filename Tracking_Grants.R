require(ggplot2)
require(dplyr)
require(scales)

#Download the files and unzip it for past 10 years
files<-c("https://exporter.nih.gov/CSVs/final/RePORTER_PRJ_C_FY2018.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PRJ_C_FY2017.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PRJ_C_FY2016.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PRJ_C_FY2014.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PRJ_C_FY2013.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PRJ_C_FY2012.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PRJ_C_FY2011.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PRJ_C_FY2010.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PRJ_C_FY2009.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PRJ_C_FY2008.zip")

loadfiles<-function(){
  Grants<-data.frame()
  for (file in files){
    temp <- tempfile()
    download.file(file,temp)
    file<-sub(".*/", "", file)
    file<-gsub("zip","csv",file)
    data <- read.csv(unz(temp, file))
    if (ncol(Grants)!=0){
      colnames(data)<-toupper(colnames(data))
      common_cols <- intersect(colnames(data), colnames(Grants))
      Grants<-rbind(subset(Grants, select = common_cols),subset(data, select = common_cols))
    }
    else{
      Grants<-rbind(Grants,data)
    }
    unlink(temp)
  }
  return(Grants)
}
Grants<-loadfiles()
head(Grants)

#Fill ORG_DEPT missing values based on IC_NAME
# get all unique institutes
institutes<-as.vector(unique(Grants$IC_NAME))
#Grants_revised<-data.frame()
for (institute in institutes){
  Center<-subset(Grants,IC_NAME %in% institute)
  #check top discipline for mental health
  discipline<-names(sort(table(Center$ORG_DEPT[Center$ORG_DEPT!=""]),decreasing=TRUE)[1])
  Grants$ORG_DEPT[Grants$ORG_DEPT=="" & Grants$IC_NAME %in% institute]<-discipline
}
Grants<-Grants[!(Grants$ORG_DEPT=="NO CODE ASSIGNED" | Grants$ORG_DEPT==""| Grants$ORG_DEPT=="n/a" | Grants$ORG_DEPT=="N/A"),]

#Format the Project start and end dates and find how long a project took
time<- as.Date(Grants$PROJECT_END, "%m/%d/%Y")-as.Date(Grants$PROJECT_START, "%m/%d/%Y")
Grants$TIME<-round(time/365)

#Preparing Data for finding cost of papers by discipline
Grants<-Grants %>% mutate(COST = coalesce(TOTAL_COST,TOTAL_COST_SUB_PROJECT))

files<-c("https://exporter.nih.gov/CSVs/final/RePORTER_PUBLNK_C_2018.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PUBLNK_C_2017.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PUBLNK_C_2016.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PUBLNK_C_2015.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PUBLNK_C_2014.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PUBLNK_C_2013.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PUBLNK_C_2012.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PUBLNK_C_2011.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PUBLNK_C_2010.zip"
         ,"https://exporter.nih.gov/CSVs/final/RePORTER_PUBLNK_C_2009.zip")

#Load the publication to map with proposal
Publications_links<-loadfiles()

#subset Projects by project numbers which got published
Project_Num<-unique(Publications_links$PROJECT_NUMBER)
Projects_Published<-subset(Grants, CORE_PROJECT_NUM %in% Project_Num)

#Plot the average time taken to publish by Discipline
ggplot(Projects_Published[which(!is.na(Projects_Published$TIME)),], aes(x=factor(reorder(ORG_DEPT,-TIME)), y=TIME,fill="blue"))+
  stat_summary(fun.y="mean", geom="bar")+
  theme(axis.text=element_text(angle=90,size=9))+
  theme(legend.position = "none")+
  labs(title="Time taken vs. Discipline", y="Time(in Years)", x="Discipline")

#Plot the average amount taken to publish by Discipline
ggplot(Projects_Published[which(!is.na(Projects_Published$COST)),], aes(x=factor(reorder(ORG_DEPT,-COST)), y=COST,fill="blue"))+
  stat_summary(fun.y="mean", geom="bar")+
  theme(axis.text=element_text(angle=90,size=12))+
  scale_y_continuous(labels = comma)+
  theme(legend.position = "none")+
  labs(title="Total Cost vs. Discipline", y="Total Cost(in $)", x="Discipline")

#Text mining application and word cloud
require("tm")
require("SnowballC")
require("wordcloud")
require("RColorBrewer")

IC_Name<-as.character(Projects_Published$IC_NAME)
#NIH_Spending<-as.character(Projects_Published$NIH_SPENDING_CATS)
docs <- Corpus(VectorSource(IC_Name))
#Transform the corpus to remove stopwords, remove numbers and punctuation, strip whitespaces
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
#Convert to document term matrix
dtm <- TermDocumentMatrix(docs,control = list(weighting = weightTfIdf))
dtm <- as.matrix(dtm)
#Get the word frequencies
wordsfreq <- sort(rowSums(dtm),decreasing=TRUE)
wordsfreq <- data.frame(word = names(wordsfreq),freq=wordsfreq)
#Create word clouds
png("research_topics.png", width = 800, height = 600,units='in', res=300)
wordcloud(words = wordsfreq$word, freq = wordsfreq$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
dev.off()
