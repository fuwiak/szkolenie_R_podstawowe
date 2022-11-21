
a<-21
b<-37

# dodawanie
print(a+b)

# odejmowanie
print(a-b)

# mnożenie
print(a*b)

# dzielenie
print(a/b)

# potęgowanie
print(a^b) # lub a**b

# dzielenie modulo
print(a%%b)

# dzielenie całkowite
print(a%/%b)

# operatory logiczne

# równość
print(a==b)

# różność
print(a!=b)

# większe
print(a>b)

# mniejsze

print(a<b)

# większe lub równe
print(a>=b)

# mniejsze lub równe

print(a<=b)

#funkcje arytmetyczne

# wartość bezwzględna
print(abs(-a))

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

#zaokrąglanie

# zaokrąglenie do najbliższej liczby całkowitej
print(round(a))

# zaokrąglenie do najbliższej liczby całkowitej w dół
print(floor(a))

# zaokrąglenie do najbliższej liczby całkowitej w górę

print(ceiling(a))

#zaokrąglanie do n miejsc po przecinku

# zaokrąglenie do 2 miejsc po przecinku
print(round(a,2))

# zaokrąglenie do 2 miejsc po przecinku w dół
print(floor(a*100)/100)

# zaokrąglenie do 2 miejsc po przecinku w górę

print(ceiling(a*100)/100)

# funkcje matematyczne

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

# kumuatywna suma
print(cumsum(c(a,b)))
# kumulatywny iloczyn

print(cumprod(c(a,b)))

print(is.numeric(a)) #czy liczba

print(is.logical(a)) # czy wartość logiczna

print(is.character(a)) # czy wartość znakowa

print(is.factor(a)) # czy wartość kategoryczna

print(is.integer(a)) # czy wartość całkowita


print(is.double(a)) # czy wartość rzeczywista

print(is.complex(a)) # czy wartość zespolona


zmienna<-readline("Wprowadz dowolna wartosc ") # to co wprowadzamy jest tekstem

class(zmienna)

zmienna<-as.numeric(zmienna) # zamiana na liczbę
class(zmienna)

zmienna<-as.logical(zmienna) # zamiana na wartość logiczną
class(zmienna)

zmienna<-as.double(zmienna) # zamiana na wartość rzeczywistą

class(zmienna)

#zadanie bojowe
#stworz zmiennie x,y,z typu double
#wprowadz do nich wartosci z klawiatury
#oblicz pole trojkata o podstawie x i wysokosci y
#oblicz pole kola o promieniu z, uzyj zmiennej pi
pi<-3.14
#oblicz pole kwadratu o boku x
#oblicz pole prostokata o bokach x i y
#znajdz najwieksza z tych wartosci, uzyj funkcji max or wektora c
#pokaz sume czesciowa tych wartosci, uzyj funkcji cumsum
#pokaz iloczyn czesciowy tych wartosci, uzyj funkcji cumprod


a<-21.370000


# formatowanie liczb
print(format(a,scientific=TRUE)) # format naukowy
print(format(a,scientific=FALSE)) # format normalny
print(format(a,scientific=FALSE,trim=TRUE)) # format normalny bez zer na końcu
print(format(a,scientific=FALSE,trim=TRUE,width=10)) # format normalny bez zer na końcu i szerokość 10 znaków
print(format(a,scientific=FALSE,trim=TRUE,width=10,decimal.mark=",")) # format normalny bez zer na końcu i szerokość 10 znaków i  przecinek zamiast kropki
#justification to left

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

#odwrocenie napisu, zobaczmy co sie stanie
print(napis1[5:1])

string_split <- strsplit(napis1, NULL)[[1]]
string_split
string_split[5:1]
paste(string_split[5:1], collapse = "") # odwrócenie napisu

# czy da sie latwiej?

install.packages("stringi")  # instalacja pakietu stringi
library(stringi) # wczytanie pakietu stringi
stri_reverse(napis1)



# łączenie napisów
print(paste(napis1,napis2))

# łączenie napisów z wykorzystaniem stringi
print(stri_join(napis1,napis2))

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

#zwieksczenie napisu

print(toupper(napis1))

#zwieksczenie napisu z wykorzystaniem stringi
print(stringi::stri_trans_toupper(napis1))
#zmniejszenie napisu

print(tolower(napis1))

#zmniejszenie napisu z wykorzystaniem stringi

print(stringi::stri_trans_tolower(napis1))

#zamiana litery H na X

print(sub("H","X",napis1))

#zamiana litery H na X z wykorzystaniem stringi

print(stringi::stri_replace_first_regex(napis1,"H","X"))


#zamiana litery o pozycji 3 na Y (liczymy od 1), nie znalazlem funkcji w stringi

substr(napis1,3,3)<-"Y"
print(napis1)

#sprawdz czy litera H jest w napisie
napis1<-"Hello"
print(grepl("H",napis1))

#sprawdz czy litera H jest w napisie z wykorzystaniem stringi
print(stringi::stri_detect_regex(napis1,"H"))

#policz duze litery w napisie
# install.packages("stringr")  # instalacja pakietu stringr

library(stringr) # wczytanie pakietu stringr
print(str_count(napis1, "[A-Z]"))

#policz duze litery w napisie z wykorzystaniem stringi
print(stringi::stri_count_regex(napis1,"[A-Z]"))


#zadanie bojowe

#stworz dwa napisy, pierszy to imie, drugi to nazwisko np Jan Kowalski

napis1<-"Jan"
napis2<-"Kowalski"


#stworz trzeci napis ktory bedzie zawieral imie i nazwisko zapisane z duzych liter np JAN KOWALSKI



#stworz czwarty napis ktory bedzie zawieral imie i nazwisko zapisane z malych liter np jan kowalski


#stworz piaty napis ktory odwroci kolejnosc imienia i nazwiska np Jan Kowalski -> Kowalski Jan



#stworz szosty napis ktory bedzie zawieral imie i nazwisko zapisane z duzych liter i odwroci kolejnosc imienia i nazwiska np Jan Kowalski -> KOWALSKI JAN


#zamien imie Jan na Janusz



#odwroc napis Jan Kowalski -> iksalwoK naK naJ



#operacje logiczne
A<-TRUE
B<-FALSE

#AND
print(A&B)

#OR
print(A|B)

#NOT
print(!A)

#XOR
print(xor(A,B))

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
# lubridate::time_length(difftime(end, start), "years")

#timestamp

#stworz timestamp

timestamp1<-as.POSIXct("2019-01-01 12:00:00", format="%Y-%m-%d %H:%M:%S")


timestamp2<-as.POSIXct("2019-01-01 12:00:00", format="%Y-%m-%d")
timestamp2


timestamp3 <- as.POSIXct("2019-01-01 12:00:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")
timestamp3


#wektory i faktory

#stworz wektor z 10 elementami

wektor1<-c(1,2,3,4,5,6,7,8,9,10)
wektor1

#stworz wektor z 10 elementami z wykorzystaniem funkcji seq

wektor2<-seq(1,10)
wektor2

typeof(wektor2)
class(wektor2)

wektor3<-seq(1, 3, by=0.2)
wektor3

wektor4<-seq(1, 3, length.out=10)
wektor4

wektor5<-seq(5, 1, by=-1)
wektor5

wektor6<-seq(5, -1)
wektor6

# dostep do elementow wektora
wektor6
wektor6[1] # pierwszy element
wektor6[2] # drugi element

#ostaatni element
wektor6[length(wektor6)]

#przed ostatni element
wektor6[length(wektor6)-1]

wektor6[1:3] # pierwszy, drugi i trzeci element

wektor6[c(1,3,5)] # pierwszy, trzeci i piaty element

wektor6[c(-1,-3,-5)] # wszystkie elementy poza pierwszym, trzecim i piatym

wektor6[c(1,3,5)]<-0 # pierwszy, trzeci i piaty element ustaw na 0

#odwroc kolejnosc elementow wektora
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

#filtruj wektor z wartosciami bool

wektor8<-c(TRUE,TRUE,FALSE,TRUE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE)
wektor8

wektor7[wektor8] # elementy wektora wektor7 dla ktorych wartosc wektora wektor8 jest TRUE

#max i min z wektora

wektor9<-c(1,2,3,4,5,6,7,8,9,10)
wektor9

max(wektor9)
min(wektor9)

#sortowanie wektora

wektor10<-c(1,2,3,4,5,6,7,8,9,10)
wektor10

sort(wektor10) # sortowanie rosnaco

sort(wektor10, decreasing=TRUE) # sortowanie malejaco

#sortowanie wektora z wykorzystaniem funkcji order

wektor11<-c(1,2,3,4,5,6,7,8,9,10)
wektor11

wektor11[order(wektor11)] # sortowanie rosnaco

wektor11[order(wektor11, decreasing=TRUE)] # sortowanie malejaco

#tworzenie wektora z losowymi wartosciami

wektor12<-sample(1:10, 10, replace=TRUE)
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


#zadanie bojowe

#stworz wektor z losowymi wartosciami z przedzialu 1-20
#policz ile jest w nim liczb parzystych
#policz ile jest w nim liczb nieparzystych
#policz ile jest w nim liczb wiekszych od 5
#policz ile jest w nim liczb mniejszych od 5
#policz ile jest w nim liczb wiekszych od 5 i parzystych
#policz jego srednia arytmetyczna
#policz srednia arytmetyczna liczb parzystych


# faktory - zmienne kategoryczne

# stworz factor ze wartosciami low, medium, high

factor1<-factor(c("low", "medium", "high", "low", "medium", "high", "low", "medium", "high", "low"))
factor1

#sortuj factor

sort(factor1)

# stworz factor ze wartosciami low, medium, high i nadaj im kolejne numery

factor2<-factor(c("low", "medium", "high", "low", "medium", "high", "low", "medium", "high", "low"), levels=c("low", "medium", "high"), ordered=TRUE)
factor2

#sortuj factor

sort(factor2)

#zmien kolejnosc factor
factor3<-factor(c("low", "medium", "high", "low", "medium", "high", "low", "medium", "high", "low"), levels=c("high", "medium", "low"), ordered=TRUE)

factor3

#sortuj factor

sort(factor3)

#unikalne wartosci factor

unique(factor3)


#stworz factor z porami roku i posortuj je wedlug kolejnosci cieplutkich por roku



