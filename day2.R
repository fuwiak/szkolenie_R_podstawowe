
a<-21
b<-23

# dodawanie
print(a+b)

# odejmowanie
print(a-b)

# mnożenie
print(a*b)

# dzielenie
print(a/b)

# potęgowanie
print(2^3)

print(2**3)


print(7/3)


print(a!=b)

print(abs(-3))

# abs<-3
# 
# abs(-3)


# pierwiastek kwadratowy
print(sqrt(a))

# logarytm naturalny
print(log(a))

# logarytm o podstawie 10
print(log10(a))

# logarytm o podstawie 2
print(log2(a))

# funkcja sinus
print(sin(a))





a<-12.5678

# zaokrąglenie do najbliższej liczby całkowitej
print(round(a))

# zaokrąglenie do najbliższej liczby całkowitej w dół
print(floor(a))

# zaokrąglenie do najbliższej liczby całkowitej w górę

print(ceiling(a))

#zaokrąglanie do n miejsc po przecinku

# zaokrąglenie do 3 miejsc po przecinku
print(round(a,3 ))

# zaokrąglenie do 2 miejsc po przecinku w dół
print(floor(a*100)/100)

# zaokrąglenie do 2 miejsc po przecinku w górę

print(ceiling(a*100)/100)


# wartość maksymalna
print(max(a,b))

# wartość minimalna
print(min(a,b))

# wartość średnia
print(mean(c(a,b)))

# wartość mediany
print(median(c(a,b)))

# wartość odchylenia standardowego
print(sd(c(a,b)))

# wartość wariancji
print(var(c(a,b)))

# wartość sumy
print(sum(c(a,b)))


# wartość iloczynu
print(prod(c(a,b)))


data<- c(1,2,3,3,4,4,5)

silnia<-c(1,2,3,4)
cumsum(silnia)






print(is.character('1'))

# 1 jest rozne od '1'




is.double(2)


zmienna<-readline("Wprowadz dowolna wartosc ")

as.numeric(zmienna)*2


#zadanie bojowe
#stworz zmiennie x,y,z typu double
#wprowadz do nich wartosci z klawiatury
#oblicz pole trojkata o podstawie x i wysokosci y
#oblicz pole kola o promieniu z, uzyj zmiennej pi
pi<-3.14

x<-readline("podaj x ")
x <-as.numeric(x)


a<-21.370000


# formatowanie liczb
print(format(a,scientific=TRUE)) # format naukowy
print(format(a,scientific=FALSE)) # format normalny
print(format(a,scientific=FALSE,trim=TRUE)) # format normalny bez zer na końcu
print(format(a,scientific=FALSE,trim=TRUE,width=10)) # format normalny bez zer na końcu i szerokość 10 znaków
print(format(a,scientific=FALSE,trim=TRUE,width=5,decimal.mark=",")) # format normalny bez zer na końcu i szerokość 10 znaków i  przecinek zamiast kropki




a<-21.370000
#sprintf - styl jezyka C
print(sprintf("%f",a)) # formatowanie liczb zmiennoprzecinkowych
print(sprintf("%e",a)) # formatowanie liczb zmiennoprzecinkowych w notacji naukowej
print(sprintf("%g",a)) # formatowanie liczb zmiennoprzecinkowych w notacji naukowej lub normalnej




napis1<-"Hello"
napis2<-"World"

#dlugosc napisu, nie uzywaj funkcji length!!
print(nchar(napis1))
print(length(napis1))

print(napis1[5:1])


string_split <- strsplit(napis1, NULL)[[1]]

string_split[5:1]
paste(string_split[5:1], collapse = "") # odwrócenie napisu

library(stringi)
stri_reverse(napis1)

stri_reverse("World")

stringi::stri_reverse("World")


# łączenie napisów
print(paste(napis1,napis2))

# łączenie napisów z wykorzystaniem stringi
print(stringi::stri_join(napis1,napis2))

# łączenie napisów z separatorem
print(paste(napis1,napis2,sep=","))

# łączenie napisów z separatorem i wykorzystaniem stringi
print(stri_join(napis1,napis2,sep=","))
print(stri_join(napis1,napis2,sep="+"))



# substr

print(substr(napis1,1,3)) # wyświetlenie 3 pierwszych znaków

# wyświetlenie 3 pierwszych znaków z wykorzystaniem stringi
print(stri_sub(napis1,1,3))

# wyswietlenie 3 ostatnich znaków
print(substr(napis1,nchar(napis1)-2,nchar(napis1)))

# wyswietlenie 3 ostatnich znaków z wykorzystaniem stringi
print(stri_sub(napis1,nchar(napis1)-2,nchar(napis1)))

#wyswietlenie od 1 do 4 znaku
print(substr(napis1,1,4))

#wyswietlenie od 1 do 4 znaku z wykorzystaniem stringi
print(stri_sub(napis1,1,4))

print(toupper(napis1))



#zwieksczenie napisu z wykorzystaniem stringi
print(stringi::stri_trans_toupper(napis1))
#zmniejszenie napisu

print(tolower(napis1))

#zmniejszenie napisu z wykorzystaniem stringi

print(stringi::stri_trans_tolower(napis1))


print(sub("H","Y",napis1))

print(stringi::stri_replace_first_regex(napis1,"H","Y"))



#zamiana litery o pozycji 3 na Y (liczymy od 1), nie znalazlem funkcji w stringi

substr(napis1,1,1)<-"J"
print(napis1)

#sprawdz czy litera H jest w napisie
napis1<-"Hello"
print(grepl("H",napis1))

#sprawdz czy litera H jest w napisie z wykorzystaniem stringi
print(stringi::stri_detect_regex(napis1,"J"))

library(stringr) # wczytanie pakietu stringr
print(str_count(napis1, "[A-Z]"))

#policz duze litery w napisie z wykorzystaniem stringi
# print(stringi::stri_count_regex(napis1,"[A-Z]")

napis1<-"Jan"
napis2<-"Kowalski"
n1 <- toupper(napis1)
n2 <- toupper(napis2)
print(paste(n1,n2))


#operacje logiczne
A<-TRUE
B<-FALSE

#AND
print(A&B)

#OR
print(A|B)

#NOT
print(!B)

# praca z datami i czasem
sdate1 <- "6aug2005"
sdate2 <- "jan151999"
sdate3 <- "12-15-2001"

# konwersja napisu na date
date1 <- as.Date(sdate1, "%d%b%Y")
date1

# konwersja napisu na date
date2 <- as.Date(sdate2, "%b%d%Y")
date2

# konwersja napisu na date
date3 <- as.Date(sdate3, "%m-%d-%Y")
date3

dates1 <- c("06sep2001","21jun2004","04jul2006","6aug2005")

as.Date(dates1, "%d%b%Y")


date1 <- as.Date("06sep2001", "%d%b%Y")
date2 <- as.Date("21jun2004", "%d%b%Y")


# roznica miedzy datami
date2 - date1

# roznica miedzy datami
as.numeric(date2 - date1)

# roznica miedzy datami w godzinach
as.numeric(date2 - date1, units = "hours")

# roznica miedzy datami w minutach
as.numeric(date2 - date1, units = "mins")

# roznica miedzy datami w sekundach
as.numeric(date2 - date1, units = "secs")

# roznica miedzy datami w latach
# as.numeric(date2 - date1, units = "year") # nie dziala
# help()
lubridate::time_length(difftime(date2, date1), "years")



#stworz wektor z 10 elementami

wektor1<-c(1,2,3,4,5,6,7,8,9,10)
wektor1

wektor2<-seq(1,100,0.5)
wektor2

wektor3<-seq(1, 3, 0.2)
wektor3

wektor4<-seq(1, 100, length.out=5)
wektor4


wektor5<-seq(5, 1, by=-1)
wektor5


wektor6<-seq(5, -1)
wektor6


wektor6[1] # pierwszy element
wektor6[2] # drugi element



#ostaatni element
wektor6[length(wektor6)-2]

wektor6[1:3]

wektor6[c(1,3,5)]

wektor6[c(-1,-3,-5)]


wektor6<-wektor6[length(wektor6):1]
wektor6

#filtruj wektor

wektor7<-c(1,2,3,4,5,6,7,8,9,10)
wektor7

wektor7[wektor7>5] # elementy wieksze od 5

wektor7[wektor7%%2==0] # elementy podzielne przez 2

wektor7[wektor7%%2==1] # elementy niepodzielne przez 2

wektor7[wektor7%%2==0 & wektor7>5] # elementy podzielne przez 2 i wieksze od 5

wektor7[wektor7%%2==0 | wektor7>5] # elementy podzielne przez 2 lub wieksze od 5


wektor8<-c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE)
wektor8

wektor7[wektor8]

wektor10<-c(9,1,-1,2,2,3,0,9,-11)

sort(wektor10)

sort(wektor10, decreasing=TRUE)


#tworzenie wektora z losowymi wartosciami

wektor12<-sample(99:109, 10, replace=FALSE)
wektor12


#tworzenie wektora z losowymi wartosciami z wykorzystaniem funkcji runif

wektor13<-runif(10, min=1, max=10) # 10 losowych liczb z przedzialu 1-10
wektor13

#tworzenie wektora z losowymi wartosciami z wykorzystaniem funkcji rnorm

wektor14<-rnorm(10, mean=0, sd=1) # 10 losowych liczb z rozkladu normalnego o sredniej 0 i odchyleniu standardowym 1
wektor14

# replikacja wektora

wektor15<-c(1,2,3,4,5,6,7,8,9,10)
wektor15

wektor15[rep(1, 10)] # 10 razy pierwszy element

wektor15[rep(1:3, 10)] # 10 razy elementy 1,2,3

wektor15[rep(1:3, each=10)] # 10 razy element 1, 10 razy element 2, 10 razy element 3


#stworz wektor z losowymi literami

wektor16<-sample(letters, 10, replace=TRUE)
wektor16



#suma dwoch wektorow

wektor17<-c(1,2,3,4,5,6,7,8,9,10)
wektor17

wektor18<-c(10,9,8,7,6,5,4,3,2,1)
wektor18

wektor17+wektor18

#iloczyn dwoch wektorow

wektor17*wektor18

#czesc wspolna dwoch wektorow

wektor19 <- c(1,2,3,4)
wektor20 <- c(3,4,5,6)

intersect(wektor19, wektor20)

#suma dwoch wektorow

union(wektor19, wektor20)

#roznica dwoch wektorow

setdiff(wektor19, wektor20)
setdiff(wektor20, wektor19)


wektir1 <- sample(1:20, 20, replace = TRUE)
wektir1
mean(wektir1[wektir1%%2==0])



factor1<-factor(c("low", "medium", "high", "low", "medium", "high", "low", "medium", "high", "low"))

sort(factor1)


factor2<-factor(c("low", "medium", "high", "low", "medium", "high", "low", "medium", "high", "low"), levels=c("high", "medium", "low"), ordered=TRUE)
factor2

#sortuj factor

sort(factor2)


factor <- factor(c("wiosna", "lato", "jesien", "zima", "zima"), levels =c("zima", "jesien", "wiosna", "lato"))
sort(factor)


M <- matrix(letters, nrow = 2, ncol=13)
print(M)

M[1,]
M[2,]

M[,1]
M[,2]

M[1,1]


M <- matrix(runif(6), nrow = 2, ncol=3)

print(M*6)

print(M/8)
print(M *1/8)


log(M[,1])


#macierz z nazwami kolumn i wierszy
M <- matrix(1:6, nrow = 2, ncol=3, dimnames = list(c("a", "b"), c("c", "d", "e")))
print(M)



M["a",]


M["b",]

M["a","c"]

M

print(t(M))

x <- runif(3)
y <- runif(3)

M <- cbind(x, y) # c - columns
print(M)


M <- rbind(x, y)
M

print(M[c(1,2),c(1,3)])

M_letter <- matrix(sample(letters, 9, replace = TRUE), nrow = 3, ncol=3)
M_letter

A <- array(1:24, dim = c(2,3,4))
print(A)

A[, , 2]

#listy

L <- list(1, "a", TRUE, 4)
print(L)


#dodaj element do listy
L <- append(L, "xxx")
L


L <- L[-7]



#polaczenie list
L1 <- list(1, "a", TRUE, 1+4i)
L2 <- list(2, "b", FALSE, 2+8i)

L <- c(L1, L2)
print(L)

cbind(L1, L2)

L <- rev(L)
print(L)


L[1:3]

L


L[!sapply(L, is.character)]



for (i in 1:10) {
  print(paste("nazwa_pliku",i,sep="_"))
}


list1 <- list(a = 1: 20, b = 25:30, c = 40:60) # stworz liste o nazwach a, b, c
list1


lapply(list1, length)

lapply(list1, shapiro.test)

library(readODS)
df<-read_ods("/Users/user/Downloads/daneMCW.ods", sheet = 1)




Numerical_variables <- which(sapply(df, is.numeric))

dane_num <-df[Numerical_variables]

lapply(dane_num,shapiro.test )



df$Miejscowosc


kwadrat <- function(x){
  return (x**2)
}

kwadrat(6)

podatek<-function(roczne_zarobki, czy_ryczal){
  if (czy_ryczal==TRUE){
    pod = 0.2*roczne_zarobki;
    return(pod)
  }
  else{
    pod = 0.4*roczne_zarobki
    return(pod)
  }
}

podatek(10, TRUE)



kasa<-100

wycieczka<-function(kasa){
  if(kasa>80){
    print("pijemy wodke")
  }
  else if (kasa<=80 & kasa>20){
    print("pijemy piwo")
  }
  
  else if (kasa<=20 & kasa>5){
    print("jemy chipsy")
  }
  
  else{
    print("Zostajemy w domu")
  }
  
}

wycieczka(0)


median(df$`%tluszcz`)


kodowanie_tluszczu<-function(x){
  if(x>2){
    return("high")
  }
  else if (x<=2 & x>1){
    return("medium")
  }
  else{
    return("low")
  }
  
}

kodowanie_tluszczu(1)


df$kodowany_tluszcz<-sapply(df$`%tluszcz`, kodowanie_tluszczu)



#a,b,c 
#delta=b^2-4*a*c
#if delta>0, 2 roz, delta==0, 1 rozwiazanie, delta<0, brak rozwiazan

delta<-function(a,b,c){
  d <-b^2-4*a*c;
  if(d>0){
    return("2 rozw")
  }
  else if (d==0){
    return("1 rozw")
  }
  else{
    return("o rozw")
  }
  
}

delta(1,1,1)


# f(x)=x^2


kwadrat(3)

lapply(list1,kwadrat)

df$log_SR <-log(df$SR) #tworzymy kolumne o nazwie log_SR w dataframe, i do niej przypisujemy rezulat logarytmu(mozemy cokolwiek innego przypisac)



library(stringi)

last_4chars<-function(word){
  four_chars <- substr(word,nchar(word)-3,nchar(word))
  return(four_chars)
  
}

napis1<-'Hello2004'
last_4chars(napis1)



df$year<-last_4chars(df$code)


update_sr <- function(x){
  if(x>20){
    x = x+100
    return(x)
  }
  else{
    x = x-50
    return(x)
  }
}

df$dziwny_sr<-lapply(df$SR, update_sr)


koduj_loc<-function(x){
  if (x=='z'){
    return('szklarnia')
  }
  else if (x=='w'){
    return('kontrola')
  }
}



df$kodowany_loc<-lapply(df$Localisation, koduj_loc)


#############DATA FRAME I WYKRESY ########################

df <- data.frame(a = 1:5, b = letters[1:5], c = TRUE, d = 1+4i)
df


df$'dowolna_nazwa' <-c(9,10,11,12,99)
df$`dowolna nazwa`<-NULL

df <- rbind(df, c(8, "f", FALSE, 6+8i,88))

df[1,]<- NA

df <- na.omit(df)

write.csv(df, file = "df.csv")

write.csv(df, file="/Users/user/Downloads/df.csv")


#odczytaj data frame z pliku csv
df2 <- read.csv("/Users/user/Downloads/df.csv")
df2



colnames(df2)

names(df2)[6]<-"tutaj miala byc nazwa"


rownames(df2)[1]<-"wiersz1"

#polacz data frame
df1 <- data.frame(a = 1:5, b = letters[1:5], c = TRUE, d = 1+4i)
df2 <- data.frame(a = 6:10, b = letters[6:10], c = FALSE, d = 6+8i)

df1
df2

df <- rbind(df1, df2)
df

#polacz data frame
df1 <- data.frame(a = 1:5, b = letters[1:5], c = TRUE, d = 1+4i)
df1
df2 <- data.frame(a = 6:10, b = letters[6:10], c = FALSE, d = 6+8i)
df2
df <- cbind(df1, df2)

df[names(df) == 'd']

df[names(df) == 'd'][2]



#join data frame
df1 <- data.frame(a = 1:5, b = letters[1:5], c = TRUE, d = 1+4i)
df2<- data.frame(a = 1:5, b = letters[1:5], c = FALSE, d = 6+8i)

df1
df2


df <- merge(df1, df2, by = "a") # inner join 
df

#lewy join data frame

df1 <- data.frame(a = 1:5, b = letters[1:5], c = TRUE, d = 1+4i)
df2<- data.frame(a = 1:5, b = letters[1:5], c = FALSE, d = 6+8i)

df <- merge(df1, df2, by = "a", all.x = TRUE)
df

#prawy join data frame

df1 <- data.frame(a = 1:5, b = letters[1:5], c = TRUE, d = 1+4i)
df2<- data.frame(a = 1:5, b = letters[1:5], c = FALSE, d = 6+8i)

df <- merge(df1, df2, by = "a", all.y = TRUE)


library(readODS)
df1<-read_ods("/Users/user/Downloads/daneMCW.ods", sheet = 1)
df1

df2<-readxl::read_excel("/Users/user/Downloads/fitomcw.xlsx")
df2

df <- merge(df1, df2, by = "code", all.x = TRUE) # inner join 
df

################ FILTROWANIE I WYKRESY################

#pobierz dane z internetu
titanic <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")
head(titanic)

#policz ilosc pasazerow
nrow(titanic)
ncol(titanic)


#policz ilosc pasazerow w klasie pierwszej
sum(titanic$Pclass == 1)


#policz ilosc pasazerow w klasie pierwszej i drugiej
sum(titanic$Pclass == 1 | titanic$Pclass == 2)

sum(titanic$Pclass %in% c(1,2))


table(titanic$Sex, titanic$Survived)/nrow(titanic)*100

titanic$FamilySize <- titanic$SibSp + titanic$Parch


titanic$AgeGroup <- cut(titanic$Age, breaks = c(0, 18, 35, 60, 100), labels = c("child", "young", "adult", "old"))
# head(titanic)
table(titanic$AgeGroup)


titanic$TicketType <- ifelse(titanic$Fare > 100, "PREMIUM", "STANDARD")


table(titanic$TicketType)


table(titanic$Sex=='female' & titanic$Survived==1 & titanic$AgeGroup == 'child')

table(titanic$Sex=='male' & titanic$Survived==0 & titanic$AgeGroup == 'old')



titanic$Name <- as.character(titanic$Name)
titanic$Name <- strsplit(titanic$Name, ",")
titanic$Name <- sapply(titanic$Name, function(x) x[2])
titanic$title <- sapply(titanic$Name, function(x) strsplit(x, " ")[[1]][2])


titanic$title <- gsub("\\.", "", titanic$title)
titanic$title <- gsub(" ", "", titanic$title)
titanic$title <- gsub("Mlle", "Miss", titanic$title)
titanic$title <- gsub("Mme", "Mrs", titanic$title)
titanic$title <- gsub("Ms", "Miss", titanic$title)
titanic$title <- gsub("Dr", "Officer", titanic$title)
titanic$title <- gsub("Major", "Officer", titanic$title)

#policz ilosc tytulow

table(titanic$title)

table(titanic$title)[which.max(table(titanic$title))]

table(titanic$title)[order(table(titanic$title), decreasing = TRUE)[1:3]]


titanic[titanic$title == "Miss",]

# na.omit(titanic[titanic$title == "Miss" & titanic$Age <= 25 & titanic$Age >= 18,])

head(titanic[order(titanic$Age),], 5)


head(titanic[order(titanic$Age, decreasing = TRUE),], 20)

head(titanic[order(titanic$Age, titanic$Pclass),])

aggregate(Age ~ Pclass, data = titanic, FUN = mean)


aggregate(Fare~Sex, data=titanic, FUN=mean)


aggregate(Fare~Sex, data=titanic, FUN=max)

aggregate(Fare~Sex+Pclass, data=titanic, FUN=mean)

titanic$AgeGroup <- cut(titanic$Age, breaks = c(0, 18, 35, 60, 100), labels = c("child", "young", "adult", "old"))


aggregate(Fare~Sex+Pclass+AgeGroup, data=titanic, FUN)


#sprawdz czy wiecej kobiet czy mezczyzn przezylo katastrofe w kazdej klasie(Plass)


library(tidyverse)
library(dplyr)

# install.packages("tidyverse")

# head(titanic)

titanic %>% head(10) %>% select(Sex, Ticket)  -> dane


write.csv(dane, file="/Users/user/Downloads/dane.csv")


