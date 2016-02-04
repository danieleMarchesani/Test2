Cascada <- read.csv("C:/Users/Daniele/Desktop/Nuova cartella/PhD/PROGETTO/Analytics/Scraping/Scraped Reviews/Booking/Cascada - Booking.csv")
Cascada$MeseProgressivo<-NULL
substrRight <- function(x,n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in 1:nrow(Cascada))
{
  Cascada[i,'mese']<-gsub('[0-9, ]',"",Cascada[i,'Date'])
  if(Cascada[i,'mese']=='January')
    Cascada[i,'MeseProgressivo']="01"
  if(Cascada[i,'mese']=='February' )
    Cascada[i,'MeseProgressivo']="02"
  if(Cascada[i,'mese']=='March' )
    Cascada[i,'MeseProgressivo']="03"
  if(Cascada[i,'mese']=='April' )
    Cascada[i,'MeseProgressivo']="04"
  if(Cascada[i,'mese']=='May' )
    Cascada[i,'MeseProgressivo']="05"
  if(Cascada[i,'mese']=='June' )
    Cascada[i,'MeseProgressivo']="06"
  if(Cascada[i,'mese']=='July' )
    Cascada[i,'MeseProgressivo']="07"
  if(Cascada[i,'mese']=='August' )
    Cascada[i,'MeseProgressivo']="08"
  if(Cascada[i,'mese']=='September')
    Cascada[i,'MeseProgressivo']="09"
  if(Cascada[i,'mese']=='October' )
    Cascada[i,'MeseProgressivo']="10"
  if(Cascada[i,'mese']=='November' )
    Cascada[i,'MeseProgressivo']="11"
  if(Cascada[i,'mese']=='December' )
    Cascada[i,'MeseProgressivo']="12"
  a<-substrRight(toString(Cascada[i,'Date']), 5)
  Cascada$anno <- substr(a,1,nchar(a)-1)
  Cascada[i,'AnnoMese']<-paste(Cascada[i,'anno'],Cascada[i,'MeseProgressivo'],"01",sep="/")
}

hist(as.Date(format(Cascada$AnnoMese, format="%Y%m%d")), breaks="quarter", format="%Y", col="light green", freq=TRUE, main="Booking - Cascada, by Quarter", xlab="quarters", ylab="Reviews")
hist(as.Date(format(Cascada$AnnoMese, format="%Y%m%d")), breaks="month", format="%m/%Y", col="light green", freq=TRUE, main="Booking - Cascada, by Month", xlab="months", ylab="Reviews")

ABC <- read.csv("C:/Users/Daniele/Desktop/Nuova cartella/PhD/PROGETTO/Analytics/Scraping/Scraped Reviews/Booking/ABC - Booking.csv")
ABC$MeseProgressivo<-NULL
substrRight <- function(x,n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in 1:nrow(ABC))
{
  ABC[i,'mese']<-gsub('[0-9, ]',"",ABC[i,'Date'])
  if(ABC[i,'mese']=='January')
    ABC[i,'MeseProgressivo']="01"
  if(ABC[i,'mese']=='February' )
    ABC[i,'MeseProgressivo']="02"
  if(ABC[i,'mese']=='March' )
    ABC[i,'MeseProgressivo']="03"
  if(ABC[i,'mese']=='April' )
    ABC[i,'MeseProgressivo']="04"
  if(ABC[i,'mese']=='May' )
    ABC[i,'MeseProgressivo']="05"
  if(ABC[i,'mese']=='June' )
    ABC[i,'MeseProgressivo']="06"
  if(ABC[i,'mese']=='July' )
    ABC[i,'MeseProgressivo']="07"
  if(ABC[i,'mese']=='August' )
    ABC[i,'MeseProgressivo']="08"
  if(ABC[i,'mese']=='September')
    ABC[i,'MeseProgressivo']="09"
  if(ABC[i,'mese']=='October' )
    ABC[i,'MeseProgressivo']="10"
  if(ABC[i,'mese']=='November' )
    ABC[i,'MeseProgressivo']="11"
  if(ABC[i,'mese']=='December' )
    ABC[i,'MeseProgressivo']="12"
  a<-substrRight(toString(ABC[i,'Date']), 5)
  ABC$anno <- substr(a,1,nchar(a)-1)
  ABC[i,'AnnoMese']<-paste(ABC[i,'anno'],ABC[i,'MeseProgressivo'],"01",sep="/")
}

hist(as.Date(format(ABC$AnnoMese, format="%Y%m%d")), breaks="quarter", format="%Y", col="light green", freq=TRUE, main="Booking - ABC, by Quarter", xlab="quarters", ylab="Reviews")
hist(as.Date(format(ABC$AnnoMese, format="%Y%m%d")), breaks="month", format="%m/%Y", col="light green", freq=TRUE, main="Booking - ABC, by Month", xlab="months", ylab="Reviews")

Belvoir <- read.csv("C:/Users/Daniele/Desktop/Nuova cartella/PhD/PROGETTO/Analytics/Scraping/Scraped Reviews/Booking/Belvoir - Booking.csv")
Belvoir$MeseProgressivo<-NULL
substrRight <- function(x,n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in 1:nrow(Belvoir))
{
  Belvoir[i,'mese']<-gsub('[0-9, ]',"",Belvoir[i,'Date'])
  if(Belvoir[i,'mese']=='January')
    Belvoir[i,'MeseProgressivo']="01"
  if(Belvoir[i,'mese']=='February' )
    Belvoir[i,'MeseProgressivo']="02"
  if(Belvoir[i,'mese']=='March' )
    Belvoir[i,'MeseProgressivo']="03"
  if(Belvoir[i,'mese']=='April' )
    Belvoir[i,'MeseProgressivo']="04"
  if(Belvoir[i,'mese']=='May' )
    Belvoir[i,'MeseProgressivo']="05"
  if(Belvoir[i,'mese']=='June' )
    Belvoir[i,'MeseProgressivo']="06"
  if(Belvoir[i,'mese']=='July' )
    Belvoir[i,'MeseProgressivo']="07"
  if(Belvoir[i,'mese']=='August' )
    Belvoir[i,'MeseProgressivo']="08"
  if(Belvoir[i,'mese']=='September')
    Belvoir[i,'MeseProgressivo']="09"
  if(Belvoir[i,'mese']=='October' )
    Belvoir[i,'MeseProgressivo']="10"
  if(Belvoir[i,'mese']=='November' )
    Belvoir[i,'MeseProgressivo']="11"
  if(Belvoir[i,'mese']=='December' )
    Belvoir[i,'MeseProgressivo']="12"
  a<-substrRight(toString(Belvoir[i,'Date']), 5)
  Belvoir$anno <- substr(a,1,nchar(a)-1)
  Belvoir[i,'AnnoMese']<-paste(Belvoir[i,'anno'],Belvoir[i,'MeseProgressivo'],"01",sep="/")
}

hist(as.Date(format(Belvoir$AnnoMese, format="%Y%m%d")), breaks="quarter", format="%Y", col="light green", freq=TRUE, main="Booking - Belvoir, by Quarter", xlab="quarters", ylab="Reviews")
hist(as.Date(format(Belvoir$AnnoMese, format="%Y%m%d")), breaks="month", format="%m/%Y", col="light green", freq=TRUE, main="Booking - Belvoir, by Month", xlab="months", ylab="Reviews")

Belvedere <- read.csv("C:/Users/Daniele/Desktop/Nuova cartella/PhD/PROGETTO/Analytics/Scraping/Scraped Reviews/Booking/Belvedere - Booking.csv")
Belvedere$MeseProgressivo<-NULL
substrRight <- function(x,n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in 1:nrow(Belvedere))
{
  Belvedere[i,'mese']<-gsub('[0-9, ]',"",Belvedere[i,'Date'])
  if(Belvedere[i,'mese']=='January')
    Belvedere[i,'MeseProgressivo']="01"
  if(Belvedere[i,'mese']=='February' )
    Belvedere[i,'MeseProgressivo']="02"
  if(Belvedere[i,'mese']=='March' )
    Belvedere[i,'MeseProgressivo']="03"
  if(Belvedere[i,'mese']=='April' )
    Belvedere[i,'MeseProgressivo']="04"
  if(Belvedere[i,'mese']=='May' )
    Belvedere[i,'MeseProgressivo']="05"
  if(Belvedere[i,'mese']=='June' )
    Belvedere[i,'MeseProgressivo']="06"
  if(Belvedere[i,'mese']=='July' )
    Belvedere[i,'MeseProgressivo']="07"
  if(Belvedere[i,'mese']=='August' )
    Belvedere[i,'MeseProgressivo']="08"
  if(Belvedere[i,'mese']=='September')
    Belvedere[i,'MeseProgressivo']="09"
  if(Belvedere[i,'mese']=='October' )
    Belvedere[i,'MeseProgressivo']="10"
  if(Belvedere[i,'mese']=='November' )
    Belvedere[i,'MeseProgressivo']="11"
  if(Belvedere[i,'mese']=='December' )
    Belvedere[i,'MeseProgressivo']="12"
  a<-substrRight(toString(Belvedere[i,'Date']), 5)
  Belvedere$anno <- substr(a,1,nchar(a)-1)
  Belvedere[i,'AnnoMese']<-paste(Belvedere[i,'anno'],Belvedere[i,'MeseProgressivo'],"01",sep="/")
}

hist(as.Date(format(Belvedere$AnnoMese, format="%Y%m%d")), breaks="quarter", format="%Y", col="light green", freq=TRUE, main="Booking - Belvedere, by Quarter", xlab="quarters", ylab="Reviews")
hist(as.Date(format(Belvedere$AnnoMese, format="%Y%m%d")), breaks="month", format="%m/%Y", col="light green", freq=TRUE, main="Booking - Belvedere, by Month", xlab="months", ylab="Reviews")

Derby <- read.csv("C:/Users/Daniele/Desktop/Nuova cartella/PhD/PROGETTO/Analytics/Scraping/Scraped Reviews/Booking/Derby - Booking.csv")
Derby$MeseProgressivo<-NULL
substrRight <- function(x,n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in 1:nrow(Derby))
{
  Derby[i,'mese']<-gsub('[0-9, ]',"",Derby[i,'Date'])
  if(Derby[i,'mese']=='January')
    Derby[i,'MeseProgressivo']="01"
  if(Derby[i,'mese']=='February' )
    Derby[i,'MeseProgressivo']="02"
  if(Derby[i,'mese']=='March' )
    Derby[i,'MeseProgressivo']="03"
  if(Derby[i,'mese']=='April' )
    Derby[i,'MeseProgressivo']="04"
  if(Derby[i,'mese']=='May' )
    Derby[i,'MeseProgressivo']="05"
  if(Derby[i,'mese']=='June' )
    Derby[i,'MeseProgressivo']="06"
  if(Derby[i,'mese']=='July' )
    Derby[i,'MeseProgressivo']="07"
  if(Derby[i,'mese']=='August' )
    Derby[i,'MeseProgressivo']="08"
  if(Derby[i,'mese']=='September')
    Derby[i,'MeseProgressivo']="09"
  if(Derby[i,'mese']=='October' )
    Derby[i,'MeseProgressivo']="10"
  if(Derby[i,'mese']=='November' )
    Derby[i,'MeseProgressivo']="11"
  if(Derby[i,'mese']=='December' )
    Derby[i,'MeseProgressivo']="12"
  a<-substrRight(toString(Derby[i,'Date']), 5)
  Derby$anno <- substr(a,1,nchar(a)-1)
  Derby[i,'AnnoMese']<-paste(Derby[i,'anno'],Derby[i,'MeseProgressivo'],"01",sep="/")
}

hist(as.Date(format(Derby$AnnoMese, format="%Y%m%d")), breaks="quarter", format="%Y", col="light green", freq=TRUE, main="Booking - Derby, by Quarter", xlab="quarters", ylab="Reviews")
hist(as.Date(format(Derby$AnnoMese, format="%Y%m%d")), breaks="month", format="%m/%Y", col="light green", freq=TRUE, main="Booking - Derby, by Month", xlab="months", ylab="Reviews")

Sedartis <- read.csv("C:/Users/Daniele/Desktop/Nuova cartella/PhD/PROGETTO/Analytics/Scraping/Scraped Reviews/Booking/Sedartis - Booking.csv")
Sedartis$MeseProgressivo<-NULL
substrRight <- function(x,n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in 1:nrow(Sedartis))
{
  Sedartis[i,'mese']<-gsub('[0-9, ]',"",Sedartis[i,'Date'])
  if(Sedartis[i,'mese']=='January')
    Sedartis[i,'MeseProgressivo']="01"
  if(Sedartis[i,'mese']=='February' )
    Sedartis[i,'MeseProgressivo']="02"
  if(Sedartis[i,'mese']=='March' )
    Sedartis[i,'MeseProgressivo']="03"
  if(Sedartis[i,'mese']=='April' )
    Sedartis[i,'MeseProgressivo']="04"
  if(Sedartis[i,'mese']=='May' )
    Sedartis[i,'MeseProgressivo']="05"
  if(Sedartis[i,'mese']=='June' )
    Sedartis[i,'MeseProgressivo']="06"
  if(Sedartis[i,'mese']=='July' )
    Sedartis[i,'MeseProgressivo']="07"
  if(Sedartis[i,'mese']=='August' )
    Sedartis[i,'MeseProgressivo']="08"
  if(Sedartis[i,'mese']=='September')
    Sedartis[i,'MeseProgressivo']="09"
  if(Sedartis[i,'mese']=='October' )
    Sedartis[i,'MeseProgressivo']="10"
  if(Sedartis[i,'mese']=='November' )
    Sedartis[i,'MeseProgressivo']="11"
  if(Sedartis[i,'mese']=='December' )
    Sedartis[i,'MeseProgressivo']="12"
  a<-substrRight(toString(Sedartis[i,'Date']), 5)
  Sedartis$anno <- substr(a,1,nchar(a)-1)
  Sedartis[i,'AnnoMese']<-paste(Sedartis[i,'anno'],Sedartis[i,'MeseProgressivo'],"01",sep="/")
}

hist(as.Date(format(Sedartis$AnnoMese, format="%Y%m%d")), breaks="quarter", format="%Y", col="light green", freq=TRUE, main="Booking - Sedartis, by Quarter", xlab="quarters", ylab="Reviews")
hist(as.Date(format(Sedartis$AnnoMese, format="%Y%m%d")), breaks="month", format="%m/%Y", col="light green", freq=TRUE, main="Booking - Sedartis, by Month", xlab="months", ylab="Reviews")

Seehotel <- read.csv("C:/Users/Daniele/Desktop/Nuova cartella/PhD/PROGETTO/Analytics/Scraping/Scraped Reviews/Booking/Seehotel - Booking.csv")
Seehotel$MeseProgressivo<-NULL
substrRight <- function(x,n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in 1:nrow(Seehotel))
{
  Seehotel[i,'mese']<-gsub('[0-9, ]',"",Seehotel[i,'Date'])
  if(Seehotel[i,'mese']=='January')
    Seehotel[i,'MeseProgressivo']="01"
  if(Seehotel[i,'mese']=='February' )
    Seehotel[i,'MeseProgressivo']="02"
  if(Seehotel[i,'mese']=='March' )
    Seehotel[i,'MeseProgressivo']="03"
  if(Seehotel[i,'mese']=='April' )
    Seehotel[i,'MeseProgressivo']="04"
  if(Seehotel[i,'mese']=='May' )
    Seehotel[i,'MeseProgressivo']="05"
  if(Seehotel[i,'mese']=='June' )
    Seehotel[i,'MeseProgressivo']="06"
  if(Seehotel[i,'mese']=='July' )
    Seehotel[i,'MeseProgressivo']="07"
  if(Seehotel[i,'mese']=='August' )
    Seehotel[i,'MeseProgressivo']="08"
  if(Seehotel[i,'mese']=='September')
    Seehotel[i,'MeseProgressivo']="09"
  if(Seehotel[i,'mese']=='October' )
    Seehotel[i,'MeseProgressivo']="10"
  if(Seehotel[i,'mese']=='November' )
    Seehotel[i,'MeseProgressivo']="11"
  if(Seehotel[i,'mese']=='December' )
    Seehotel[i,'MeseProgressivo']="12"
  a<-substrRight(toString(Seehotel[i,'Date']), 5)
  Seehotel$anno <- substr(a,1,nchar(a)-1)
  Seehotel[i,'AnnoMese']<-paste(Seehotel[i,'anno'],Seehotel[i,'MeseProgressivo'],"01",sep="/")
}

hist(as.Date(format(Seehotel$AnnoMese, format="%Y%m%d")), breaks="quarter", format="%Y", col="light green", freq=TRUE, main="Booking - Seehotel, by Quarter", xlab="quarters", ylab="Reviews")
hist(as.Date(format(Seehotel$AnnoMese, format="%Y%m%d")), breaks="month", format="%m/%Y", col="light green", freq=TRUE, main="Booking - Seehotel, by Month", xlab="months", ylab="Reviews")

Seminarhotel <- read.csv("C:/Users/Daniele/Desktop/Nuova cartella/PhD/PROGETTO/Analytics/Scraping/Scraped Reviews/Booking/Seminarhotel Sempachersee - Booking.csv")
Seminarhotel$MeseProgressivo<-NULL
substrRight <- function(x,n){
  substr(x, nchar(x)-n+1, nchar(x))
}
for(i in 1:nrow(Seminarhotel))
{
  Seminarhotel[i,'mese']<-gsub('[0-9, ]',"",Seminarhotel[i,'Date'])
  if(Seminarhotel[i,'mese']=='January')
    Seminarhotel[i,'MeseProgressivo']="01"
  if(Seminarhotel[i,'mese']=='February' )
    Seminarhotel[i,'MeseProgressivo']="02"
  if(Seminarhotel[i,'mese']=='March' )
    Seminarhotel[i,'MeseProgressivo']="03"
  if(Seminarhotel[i,'mese']=='April' )
    Seminarhotel[i,'MeseProgressivo']="04"
  if(Seminarhotel[i,'mese']=='May' )
    Seminarhotel[i,'MeseProgressivo']="05"
  if(Seminarhotel[i,'mese']=='June' )
    Seminarhotel[i,'MeseProgressivo']="06"
  if(Seminarhotel[i,'mese']=='July' )
    Seminarhotel[i,'MeseProgressivo']="07"
  if(Seminarhotel[i,'mese']=='August' )
    Seminarhotel[i,'MeseProgressivo']="08"
  if(Seminarhotel[i,'mese']=='September')
    Seminarhotel[i,'MeseProgressivo']="09"
  if(Seminarhotel[i,'mese']=='October' )
    Seminarhotel[i,'MeseProgressivo']="10"
  if(Seminarhotel[i,'mese']=='November' )
    Seminarhotel[i,'MeseProgressivo']="11"
  if(Seminarhotel[i,'mese']=='December' )
    Seminarhotel[i,'MeseProgressivo']="12"
  a<-substrRight(toString(Seminarhotel[i,'Date']), 5)
  Seminarhotel$anno <- substr(a,1,nchar(a)-1)
  Seminarhotel[i,'AnnoMese']<-paste(Seminarhotel[i,'anno'],Seminarhotel[i,'MeseProgressivo'],"01",sep="/")
}

hist(as.Date(format(Seminarhotel$AnnoMese, format="%Y%m%d")), breaks="quarter", format="%Y", col="light green", freq=TRUE, main="Booking - Seminarhotel Sempachersee, by Quarter", xlab="quarters", ylab="Reviews")
hist(as.Date(format(Seminarhotel$AnnoMese, format="%Y%m%d")), breaks="month", format="%m/%Y", col="light green", freq=TRUE, main="Booking - Seminarhotel Sempachersee, by Month", xlab="months", ylab="Reviews")

ABC$X<-NULL
Belvedere$X<-NULL
Cascada$X<-NULL
Derby$X<-NULL
Sedartis$X<-NULL
Seehotel$X<-NULL
Seminarhotel$X<-NULL

names(ABC) <- names(Belvedere)
names(Belvoir) <- names(Belvedere)
names(Cascada) <- names(Belvedere)
names(Derby) <- names(Belvedere)
names(Sedartis) <- names(Belvedere)
names(Seehotel) <- names(Belvedere)
names(Seminarhotel) <- names(Belvedere)

totale<-rbind(ABC, Belvedere, Belvoir, Cascada, Derby, Sedartis, Seehotel, Seminarhotel)


hist(as.Date(format(totale$AnnoMese, format="%Y%m%d")), breaks="quarter", format="%Y", col="light green", freq=TRUE, main="All Innotour Hotels, by Quarter", xlab="quarters", ylab="Reviews")
hist(as.Date(format(totale$AnnoMese, format="%Y%m%d")), breaks="month", format="%m/%Y", col="light green", freq=TRUE, main="All Innotour Hotels, by Month", xlab="months", ylab="Reviews")
