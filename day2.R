

#macierze

M <- matrix(1:6, nrow = 2, ncol=3)
print(M)

det(M)

#wyznacznik macierzy

square_matrix <- matrix(1:4, nrow = 2, ncol=2)

square_matrix

det(square_matrix) # jaki jest warunek, by mozna bylo policzyc wyznacznik?




#macierz z wartosciami losowymi
M <- matrix(runif(6), nrow = 2, ncol=3)
print(M*6)

print(M/8)
print(M *1/8)

#macierz z wartosciami losowymi z rozkladu normalnego
M <- matrix(rnorm(6), nrow = 2, ncol=3)
print(M)

#macierz losowych liter
M <- matrix(sample(letters, 6, replace = TRUE), nrow = 2, ncol=3)
print(M)


M[1,]
M[,2]
M[,3]

M[1,1]
M[1,3]



#macierz z nazwami kolumn i wierszy
M <- matrix(1:6, nrow = 2, ncol=3, dimnames = list(c("a", "b"), c("c", "d", "e")))
print(M)


#wyciaganie elementow z kolumny o nazwie c
print(M[,"c"])



#wyciaganie elementow z wiersza o nazwie a
print(M["a",])

#transponowanie macierzy
print(t(M))

#stworz macierz z dwoch losowych wektorow

x <- runif(3)
y <- runif(3)

M <- cbind(x, y) # c - columns
print(M)

#stworz macierz z dwoch losowych wektorow
M <- rbind(x, y) # r- rows

M



# Indeksowanie macierzy

M <- matrix(1:6, nrow = 2, ncol=3)
print(M)

#pierwszy wiersz
print(M[1,])

#pierwsza kolumna
print(M[,1])

#pierwszy element
print(M[1,1])

#ostatni element
print(M[2,3])

#ostatni wiersz
print(M[2,])

#ostatnia kolumna
print(M[,3])

#pierwszy i ostatni wiersz
print(M[c(1,2),])

#pierwsza i ostatnia kolumna
print(M[,c(1,3)])

#pierwszy i ostatni element
print(M[c(1,2),c(1,3)])

M_letter <- matrix(sample(letters, 6, replace = TRUE), nrow = 2, ncol=3)

#array

A <- array(1:24, dim = c(2,3,4))
print(A)

#pierwszy element
print(A[1,1,1])

print(A[,,])


#ostatni element
print(A[2,3,4])

#pierwszy i ostatni element
print(A[c(1,2),c(1,3),c(1,4)])

A

#listy

L <- list(1, "a", TRUE, 4)
print(L)

#dodaj element do listy
L <- append(L, "b")
L

#usun element z listy
L <- L[-3]

L <-L[-4]


#polaczenie list
L1 <- list(1, "a", TRUE, 1+4i)
L2 <- list(2, "b", FALSE, 2+8i)

L <- c(L1, L2)
print(L)

rbind(L1, L2)

#odwroc kolejnosc elementow listy
L <- rev(L)
print(L)

#wybiere elementy z listy
L[1:3]

L[4:7]

L


#wybierz tylko elementy z listy, ktore sa liczbami
L[sapply(L, is.numeric)]

#wybierz tylko elementy z listy, ktore nie sa liczbami
L[!sapply(L, is.numeric)]

#wybierz tylko elementy z listy, ktore sa literami
L[sapply(L, is.character)]

#wybierz tylko elementy z listy, ktore sa liczbami zespolonymi
L[sapply(L, is.complex)]




list1 <- list(a = 1: 20, b = 25:30, c = 40:60) # stworz liste o nazwach a, b, c
list1

# lapply output as list

lapply(list1, length) # policz dlugosc kazdego elementu listy

lapply(list1, sum) # policz sume kazdego elementu listy

lapply(list1, mean) # policz srednia kazdego elementu listy

lapply(list1, sd) # policz odchylenie standardowe kazdego elementu listy

lapply(list1, var) # policz wariancje kazdego elementu listy

lapply(list1, min) # policz minimum kazdego elementu listy

lapply(list1, max) # policz maksimum kazdego elementu listy

lapply(list1, range) # policz zakres kazdego elementu listy


#sapply output as  matrix

sapply(list1, length) # policz dlugosc kazdego elementu listy
sapply(list1, sum) # policz sume kazdego elementu listy
sapply(list1, mean) # policz srednia kazdego elementu listy
sapply(list1, sd) # policz odchylenie standardowe kazdego elementu listy
sapply(list1, var) # policz wariancje kazdego elementu listy
sapply(list1, min) # policz minimum kazdego elementu listy
sapply(list1, max) # policz maksimum kazdego elementu listy
sapply(list1, range) # policz zakres kazdego elementu listy


M <- matrix(1:6, nrow = 2, ncol=3)
print(M)

kwadrat <- function(x){
  return (x**2)
}

#zastosuj funkcje do kazdego elementu macierzy
lapply(M, kwadrat)
sapply(M, kwadrat)

apply(M, 2, kwadrat) # zastosuj funkcje do kazdego wiersza
apply(M, 1, kwadrat) # zastosuj funkcje do kazdej kolumny
apply(M, c(1,2), kwadrat)

#zadanie bojowe

#stworz macierz 3x3 9:17

M1 <- matrix(9:17, nrow = 3, ncol=3)
M1


#napisz kod/funkcje, ktora policzy sume elementow w kazdym wierszu

apply(M,1, sum)

sum(M[1,])+sum(M[2,])+sum(M[3,])

#napisz kod/funkcje, ktora policzy sume elementow w kazdej kolumnie

apply(M, 2, sum)



#napisz kod/funkcje, ktora policzy sume elementow na przekatnej

sum(M[1,1])+sum(M[2,2])+sum(M[3,3])

#napisz kod/funkcje, ktora policzy sume elementow na przekatnej drugiej




#stworz nastepna macierz 3x3

M2 <- matrix(1:9, nrow = 3, ncol=3)
M2

cbind(M1,M2)
cbind(M2,M1)

M1
M2

rbind(M1,M2)
rbind(M2,M1)

#polacz macierze w jedna za pomoca funkcji cbind
#polacz macierze w jedna za pomoca funkcji rbind

#odwroc znaki elementow macierzy uzywajac jednej funkcji z apply, lapply, sapply

M1

minus <- function(x){
  return (-x)
}


apply(M1, c(1,2), minus)


apply(M1, 2, minus)
M1

lapply(M1, minus)

#data frame

#stworz data frame
df <- data.frame(a = 1:5, b = letters[1:5], c = TRUE, d = 1+4i)
df

#dodaj kolumne do data frame
df$e <- c(1,2,3,4,5)
df

df$f<-c(9,10,11,12,13)
df

#usun kolumne z data frame
df$e <- NULL
df

#dodaj wiersz do data frame
df <- rbind(df, c(6, "f", FALSE, 6+8i, 9))
df

#usun wiersz z data frame
df[6,]<- NA
df <- na.omit(df)
df

df[3,]<-NA
df <- na.omit(df)
df

#zapisz data frame do pliku csv
write.csv(df, file = "df.csv")


write.csv(df, file="/Users/user/Downloads/dupa.csv")



#odczytaj data frame z pliku csv
df <- read.csv("df.csv")
df

names(df)


#zmien nazwe kolumny
names(df)[1] <- "kolumna1"
df

rownames(df)

#zmien nazwe wiersza
rownames(df)[1] <- "wiersz1"
df

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

#titanic

#pobierz dane z internetu
titanic <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")
head(titanic)

class(titanic)

#policz ilosc pasazerow
nrow(titanic)
ncol(titanic)

#policz ilosc pasazerow w klasie pierwszej
sum(titanic$Pclass == 1)

#policz ilosc pasazerow w klasie pierwszej i drugiej
sum(titanic$Pclass == 1 | titanic$Pclass == 2)

#policz ilosc pasazerow w klasie pierwszej i drugiej i trzeciej
sum(titanic$Pclass == 1 | titanic$Pclass == 2 | titanic$Pclass == 3)

#policz ilosc pasazerow w klasie pierwszej i drugiej i trzeciej

sum(titanic$Pclass %in% c(1,2))

#policz ilosc pasazerow w klasie pierwszej i drugiej i trzeciej

sum(titanic$Pclass %in% 1:2)

#policz ilosc pasazerow w klasie pierwszej i drugiej i trzeciej

sum(titanic$Pclass <= 3)

#policz ile kobiet i mezczyzn przezylo katastrofe

table(titanic$Sex, titanic$Survived)

head(titanic)
#polacz kolumny SiblingsSpouse i ParentsChildren w jedna kolumne FamilySize

titanic$FamilySize <- titanic$SibSp + titanic$Parch
head(titanic)

#podziel pasazerow na kategorie wiekowe

titanic$AgeGroup <- cut(titanic$Age, breaks = c(0, 18, 35, 60, 100), labels = c("child", "young", "adult", "old"))
# head(titanic)
table(titanic$AgeGroup)

#stworz kolumne, ktora bedzie miala wartosc PREMIUM, jezeli pasazer zaplacil wiecej niz 100 dol, a STANDARD, jezeli zaplacil mniej

titanic$TicketType <- ifelse(titanic$Fare > 100, "PREMIUM", "STANDARD")

table(titanic$TicketType)

head(titanic,10)

tail(titanic,9)

#uzywajac funckji table, podac ile przezylo mlodych dziewczynek, a ile nie przezylo starszych mezczzyn

table(titanic$Sex=='female' & titanic$Survived==1 & titanic$AgeGroup == 'child')

table(titanic$Sex=='male' & titanic$Survived==0 & titanic$AgeGroup == 'old')

table(titanic$Survived, titanic$Sex, titanic$AgeGroup)


#podziel kolumne Name po przecinku

head(titanic)


titanic$Name <- as.character(titanic$Name)
titanic$Name <- strsplit(titanic$Name, ",")
titanic$Name <- sapply(titanic$Name, function(x) x[2])
titanic$title <- sapply(titanic$Name, function(x) strsplit(x, " ")[[1]][2])





#usun kropki z tytulow

titanic$title <- gsub("\\.", "", titanic$title)
titanic$title <- gsub(" ", "", titanic$title)
titanic$title <- gsub("Mlle", "Miss", titanic$title)
titanic$title <- gsub("Mme", "Mrs", titanic$title)
titanic$title <- gsub("Ms", "Miss", titanic$title)
titanic$title <- gsub("Dr", "Officer", titanic$title)
titanic$title <- gsub("Major", "Officer", titanic$title)

#policz ilosc tytulow

table(titanic$title)

#najpopularniejszy tytul

table(titanic$title)[which.max(table(titanic$title))]

#top 3 najpopularniejsze tytuly

table(titanic$title)[order(table(titanic$title), decreasing = TRUE)[1:3]]

#filtruj po tytule

titanic[titanic$title == "Miss",]

#filtruj po tytule i wieku

head(titanic[titanic$title == "Miss" & titanic$Age < 18,])

head(titanic)

#sortuj po wieku

head(titanic[order(titanic$Age),])

#sortuj po wieku malejaco

head(titanic[order(titanic$Age, decreasing = TRUE),])

#sortuj po wieku i klasie

head(titanic[order(titanic$Age, titanic$Pclass),])

#pokaz srednia wieku dla kazdej klasy

aggregate(Age ~ Pclass, data = titanic, FUN = mean)

#pokaz srednia oplaty za bilet dla kazdej plci

aggregate(Fare~Sex, data=titanic, FUN=mean)

#pokaz maksymalna oplate za bilet dla kazdej plci

aggregate(Fare~Sex, data=titanic, FUN=max)

#pokaz min oplaty za bilet dla kazdej plci i klasy

aggregate(Fare~Sex+Pclass, data=titanic, FUN=min)
aggregate(Fare~Sex+Pclass+AgeGroup, data=titanic, FUN=max)

#zadanie bojowe

head(titanic)

#policz liczbw kobiet i mezczyzn w kazdej klasie

# table(titanic$Sex, titanic$Pclass)

aggregate(PassengerId~Sex+Pclass, data=titanic, FUN=length)


#policz ilosc kobiet i mezczyzn w kazdej klasie, ktore przezyly katastrofe

aggregate(PassengerId~Sex+Pclass, data=titanic, FUN=length)

# aggregate(PassengerId~Sex+Pclass+Survived, data=titanic, FUN=length)
table(titanic$Sex, titanic$Pclass, titanic$Survived)
aaa<-aggregate(PassengerId~Sex+Pclass+Survived, data=titanic, FUN=length)[aaa$Survived == 1,]

#stworz kolumne, ktora bedzie miala wartosc child, jezeli pasazer ma mniej niz 18 lat, a adult, jezeli ma wiecej

titanic$child_group<-ifelse(titanic$Age < 18, "child", "adult")

table(titanic$child_group)


#policz ile srednio placil doroslly pasazer za bilet w kazdej klasie a ile za bilet dziecko

aggregate(Fare~Pclass+child_group, data=titanic, FUN=mean)


#policz ile srednio placil doroslly pasazer za bilet w kazdej klasie a ile za bilet dziecko, ktore przezylo katastrofe

aggregate(Fare~Pclass+child_group+Survived, data=titanic, FUN=mean)

#policz ile srednio placil doroslly pasazer za bilet w kazdej klasie a ile za bilet dziecko, 
# ktore przezylo katastrofe, ktore mialy mniej niz 3 rodzenstwa

aggregate(Fare~Pclass+child_group+Survived, data=titanic, FUN=mean)

aggregate(Fare~Pclass+child_group+Survived+SibSp, data=titanic[titanic$SibSp<3,], FUN=mean)


#sprawdz czy wiecej kobiet czy mezczyzn przezylo katastrofe
#sprawdz czy wiecej kobiet czy mezczyzn przezylo katastrofe w kazdej klasie

#sprawdz ile zaplacilo za bilet najstarszy pasazer

#sprawdz ile zaplacilo za bilet najmlodszy pasazer

#sprawdz ile zaplacila najmlodsza dziewczynka a ile majmlodszy chlopiec

boy<-titanic[titanic$Sex=='male',]
boy[which.min(boy$Age),]
boy[which.min(boy$Age),]["Fare"]

min_fare_by_sex<-function (x){
  temp<-titanic[titanic$Sex==x,]
  temp[which.min(temp$Age),]
  temp[which.min(temp$Age),]["Fare"]
  
  
}

min_fare_by_sex("female")



#tidyverse dplyr data manipulation

library(tidyverse)
library(dplyr)

#select columns by pipe


# ls | cat | less > plik.txt 

head(titanic)

titanic %>% head(10) %>% select(Sex)

titanic %>% select(Sex, Age, Pclass) %>% head()


titanic %>% select(Sex, Age, Pclass) %>% head()

#select columns by pipe and filter


head(titanic[titanic$Age<19,])


titanic %>% filter(Age<19) %>% head()

#select columns by pipe and filter and order

titanic %>% filter(Age<19) %>% arrange(desc(Age)) %>% head()


#select columns by pipe and filter and order and mutate

titanic %>% filter(Age<39) %>% arrange(Age) %>% mutate(AgeGroup = ifelse(Age < 18, "child", "adult")) %>% head()


#select columns by pipe and filter and order and mutate and group_by

titanic %>%
  filter(Age<19) %>% arrange(Age) %>% mutate(AgeGroup = ifelse(Age < 18, "child", "adult")) %>% group_by(AgeGroup) %>% head()

#select columns by pipe and filter and order and mutate and group_by and summarise

titanic %>%
  filter(Age<19) %>% arrange(Age) %>% mutate(AgeGroup = ifelse(Age < 18, "child", "adult")) %>% group_by(AgeGroup) %>% summarise(meanAge = mean(Age))


#select columns by pipe and filter and order and mutate and group_by and summarise and arrange

titanic %>%
  filter(Age<19) %>% arrange(Age) %>% mutate(AgeGroup = ifelse(Age < 18, "child", "adult")) %>% group_by(AgeGroup) %>% summarise(meanAge = mean(Age)) %>% arrange(desc(meanAge))



#za pomoca mutate stworzyc kolumne duza_rodzina, gdzie duza_rodzina Family_Size>3, "Duza", "Mala"

titanic %>% mutate(AgeGroup = ifelse(Age < 18, "child", "adult"))

temp<-titanic %>% 
  mutate(Family_Size=SibSp+Parch)%>%
  mutate(duza_rodzina = ifelse(Family_Size >3, "duza", "mala"))



# group_by(AgeGroup) %>% summarise(meanAge = mean(Age))
# dzieci < 18, 

#policzyc sredni wiek niepelnoletnich wg plci

titanic %>%
  filter(Age<18) %>% group_by(Sex) %>% summarise(meanAge = mean(Age))




library(ggplot2)

#plot histogram

ggplot(titanic, aes(x = Age)) + geom_histogram()

#plot histogram with bins

ggplot(titanic, aes(x = Age)) + geom_histogram(bins = 60)

#plot histogram with bins and fill

ggplot(titanic, aes(x = Age)) + geom_histogram(bins = 60, fill = "blue")

#plot histogram with bins and fill and color

ggplot(titanic, aes(x = Age)) + geom_histogram(bins = 60, fill = "#00ffff", color = "blue",show.legend=TRUE)

#scatter plot

ggplot(titanic, aes(x = Age, y = Fare)) + geom_point()

#scatter plot with color
ggplot(titanic, aes(x = Age, y = Fare)) + geom_point(color = "#00ffff")

#scatter plot with color and shape
ggplot(titanic, aes(x = Age, y = Fare)) + geom_point(color = "red", shape = 4)

#scatter plot with linear regression

ggplot(titanic, aes(x = Age, y = Fare)) + geom_point(color = "red", shape = 4) + geom_smooth(method = "lm")

#scatter plot with linear regression and legend

ggplot(titanic, aes(x = Age, y = Fare))
+ geom_point(color = "red", shape = 2) + geom_smooth(method = "lm", color = "blue") + theme(legend.position = "bottom")

#scatter plot with linear regression and legend and title

ggplot(titanic, aes(x = Age, y = Fare)) + geom_point(color = "red", shape = 2) + geom_smooth(method = "lm", color = "blue")+ theme(legend.position = "bottom") + ggtitle("Age vs Fare")

#facet plot

ggplot(titanic, aes(x = Age, y = Fare)) + geom_point(color = "red", shape = 2) + geom_smooth(method = "lm", color = "blue") + theme(legend.position = "bottom") + ggtitle("Age vs Fare") + facet_wrap(~TicketType)

#facet plot with legend

ggplot(titanic, aes(x = Age, y = Fare)) + geom_point(color = "red", shape = 2) + geom_smooth(method = "lm", color = "blue") + theme(legend.position = "bottom") + ggtitle("Age vs Fare") + facet_wrap(~Sex) + theme(legend.position = "bottom")


#facet plot Age vs Fare with legend and title and facet

ggplot(titanic, aes(x = Age, y = Fare)) + geom_point(color = "red", shape = 2) + geom_smooth(method = "lm", color = "blue") + theme(legend.position = "bottom") + ggtitle("Age vs Fare") + facet_wrap(~AgeGroup) + theme(legend.position = "bottom")

#boxplot

ggplot(titanic, aes(y = Fare)) + geom_boxplot()

ggplot(titanic, aes(x = Fare)) + geom_boxplot()


#facet boxplot

ggplot(titanic, aes(y = Fare)) + geom_boxplot() + facet_wrap(~Sex)




#pokaz wykres rozrzutu dla wieku(geom_point) i ceny biletu z podzialem na plec i klasa
# + facet_wrap(~AgeGroup)
ggplot(titanic, aes(x = Age, y = Fare)) + geom_point(color = "red", shape = 2) + geom_smooth(method = "lm", color = "blue") + theme(legend.position = "bottom") + ggtitle("Age vs Fare") + facet_wrap(~Sex+~Pclass) + theme(legend.position = "bottom")


#boxplot z podzialem(facet) na klase Premium i STANDARD, dodajcie tytul i legende

ggplot(titanic, aes(y = Fare)) + geom_boxplot()+facet_wrap(~TicketType)+theme(legend.position = "bottom") + ggtitle("Age vs Fare boxpplot") + theme(legend.position = "bottom")



titanic %>%filter(Sex=="female")%>%
  ggplot(aes(y = Fare)) + geom_boxplot()+facet_wrap(~TicketType)+theme(legend.position = "bottom") + ggtitle("Age vs Fare boxpplot female") + theme(legend.position = "bottom")


  



