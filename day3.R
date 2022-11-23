library(tidyverse)
library(ggplot2)
library(haven)
library(nortest)
# library(rstatix)
library(corrplot)


# library("ggpubr")

install.packages('rstatix')


#spps
url<-"https://github.com/fuwiak/szkolenie_R_podstawowe/blob/main/PL.sav?raw=true"


# wczytanie danych

PL<-read_sav(url)

# wyświetlenie danych

PL %>% glimpse()

# wyświetlenie pierwszych 6 wierszy

PL %>% head()

# wyświetlenie ostatnich 6 wierszy

PL %>% tail()

# wyświetlenie statystyk

PL %>% summary()
# wyświetlenie statystyk dla kolumny

#rds - plik zapisany w formacie RDS

url<-"https://github.com/fuwiak/szkolenie_R_podstawowe/blob/main/suicides.rds?raw=true"

# wczytanie danych

suicides<-read_rds(url)

# wyświetlenie danych

suicides %>% glimpse()

# wyświetlenie pierwszych 6 wierszy

suicides %>% head()


#  wczytanie danych z pliku csv

titanic <- read.csv("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv")



# tabele kontyngencji - test chi-kwadrat

# przygotowanie danych

tab <- table(titanic$Survived, titanic$Sex)
tab

# test chi-kwadrat
# H0: nie ma zależności między zmiennymi Survived
# H1: istnieje zależność między zmiennymi Sex
chisq.test(tab)


p<-chisq.test(tab)$p.value

if(p<0.05){
  print("Odrzucamy hipotezę zerową")
}
else{
  print("Nie odrzucamy hipotezy zerowej")
}

# Współczynnik V Craméra

V<- sqrt(chisq.test(tab)$statistic/(sum(tab)*(min(dim(tab))-1)))

# Współczynnik V Craméra daje w wyniku wartości pomiędzy 0 a +1 (włącznie), przy czym im wynik jest bliżej 0,
# tym słabszy jest związek między badanymi cechami, a im bliżej jest 1, tym silniejszy jest związek między badanymi cechami

# przygotowanie danych

#zadanie 1
# Zbadac zależność między zmiennymi Survived i Pclass
# a) przygotować tabele kontyngencji
# b) przeprowadzić test chi-kwadrat
# c) obliczyć współczynnik V Craméra

#test anova
# Jednoczynnikowa analiza wariancji

suicides %>% glimpse()

# Sprawdzmy zwiazek miedzy zmienna Sex a suicides_no, do tego celu uzyjemy testu ANOVA.
# Ustalmy na wstepie ze poziom istotnosci alfa wynosi=0.05. Jesli wartosc Pr(>F) jest mniejsza niz zadany poziom istotnosci,
# mozna bedzie stwierdzic zwiazek miedzy zmienna zmienna Sex a suicides_no.

# H0: brak wplywu zmiennej Sex na zmienna suicides_no
# H1: wplyw zmiennej Sex na zmienna suicides_no
anova<-aov(suicides_no~sex, data=suicides)

summary(anova)

p<-anova$`Pr(>F)`[1]
p # wartosc p

# p<0.05
# Wnioski
# Odrzucamy hipotezę zerową, zmienne są zależne

# Współczynnik V Craméra

V<- sqrt(anova$`F value`[1]/(anova$`F value`[1]+anova$`Num DF`[1]))

# Współczynnik V Craméra daje w wyniku wartości pomiędzy 0 a +1 (włącznie), przy czym im wynik jest bliżej 0,
# tym słabszy jest związek między badanymi cechami, a im bliżej jest 1, tym silniejszy jest związek między badanymi cechami

# Zadanie 2

# Zbadac zależność między zmiennymi suicides_no i age, do tego celu uzyjemy testu ANOVA.
# Ustalmy na wstepie ze poziom istotnosci alfa wynosi=0.05. Jesli wartosc Pr(>F) jest mniejsza niz zadany poziom istotnosci,
# mozna bedzie stwierdzic zwiazek miedzy zmienna zmienna age a suicides_no.

# H0: brak wplywu zmiennej age na zmienna suicides_no
# H1: wplyw zmiennej age na zmienna suicides_no


# Test t dla grup niezaleznych

# Zalozenia:
# - dane maja byc normalnie rozkladane
# - dane maja byc niezalezne
# - dane maja byc rownorodne
# - wariancja w grupach jest rowna

suicides %>% ggplot(aes(y = suicides_no)) + geom_boxplot() + facet_wrap(~sex)+ggtitle("samobostwa w zależności od płci")

# Sprawdzenie zalozen testu t-studenta


# Normalnosc rozkladu sprawdzamy testem Shapiro-Wilka. Ustalmy na wstepie ze poziom istotnosci alfa wynosi=0.05. Jezeli p-value z testu bedzie wyzsze niz 0.05, nie mamy podstaw, aby odrzucic hipoteze zerowa o normalnosci rozkladu danej zmiennej.

# H0: dane sa rozkladane normalnie
# H1: dane nie sa rozkladane normalnie

suicides %>% group_by(sex) %>% summarise(shapiro.test(suicides_no)$p.value)

dim(suicides)


library(nortest)
suicides %>% group_by(sex) %>% summarise(ad.test(suicides_no)$p.value)

# Wyniki testu AD pokazują, że dane nie są rozkładane normalnie.

#sprawdzy czy chociaz wariancja jest rowna

suicides %>% var_test(suicides_no~sex)


# Wnioski
# Warunki testu t-studenta nie sa spelnione, nie mozna zastosowac testu t-studenta.

# W tym wypadku skorzystamy z testu nieparametrycznego Mann-Whitney-Wilcoxona.
# Ustalmy na wstepie ze poziom istotnosci alfa wynosi=0.05.
# Jezeli p-value z testu bedzie wyzsze niz 0.05, nie mamy podstaw,
# aby odrzucic hipoteze ze grupy Male i Female dla zmiennej suicides_no pochodza z tej samej populacji.


# suicides %>% drop_na() %>%


# analiza korelacji

# Zadanie 3

# Zbadac zależność między zmiennymi suicides_no i gdp_per_capita, do tego celu uzyjemy testu korelacji Pearsona.

# Ustalmy na wstepie ze poziom istotnosci alfa wynosi=0.05. Jezeli wartosc p-value z testu bedzie mniejsza niz 0.05, mozna bedzie stwierdzic zwiazek miedzy zmienna zmienna gdp_per_capita a suicides_no.

# H0: brak wplywu zmiennej gdp_per_capita na zmienna suicides_no
# H1: wplyw zmiennej gdp_per_capita na zmienna suicides_no

# Zalozenia:
# - dane maja byc normalnie rozkladane
# - dane maja byc niezalezne
# - dane maja byc rownorodne
# - wariancja w grupach jest rowna

# Normalnosc rozkladu sprawdzamy testem ad.test. Ustalmy na wstepie ze poziom istotnosci alfa wynosi=0.05. Jezeli p-value z testu bedzie wyzsze niz 0.05, nie mamy podstaw, aby odrzucic hipoteze zerowa o normalnosci rozkladu danej zmiennej.


# H0: dane sa rozkladane normalnie
# H1: dane nie sa rozkladane normalnie

ad.test(suicides$suicides_no)
ad.test(suicides$gdp_per_capita)

# Wyniki testu AD pokazują, że dane nie są rozkładane normalnie.

# Korelacja Spearmana to analiza pozwalająca korelować ze sobą zmienne na skali porządkowej oraz ilościowym nieposiadające rozkładu normalnego
cor.test(suicides$suicides_no, suicides$gdp_per_capita, method = "spearman")

cor.test(suicides$suicides_no, suicides$gdp_per_capita, method = "spearman")$p.value

# Wnioski

# p<0.05, więc zmienna gdp_per_capita ma wpływ na zmienną suicides_n, ale
# korelacja jest słaba i nie jest istotna statystycznie.


#select only numeric variables for correlation and plot heatmap
suicides_num <- suicides %>% select_if(is.numeric)


#wykres rozrzutu

suicides_num %>% select(suicides_no, gdp_per_capita) %>% ggplot(aes(x = suicides_no, y = gdp_per_capita)) + geom_point() + ggtitle("Zależność między zmiennymi suicides_no i gdp_per_capita")

#plot heatmap annotating with correlation values

suicides_num %>% cor() %>% round(2) %>% corrplot::corrplot(method = "number")

#tau kendall correlation

suicides_num %>% cor(method = "kendall") %>% round(2) %>% corrplot::corrplot(method = "number")

# Zadanie 4

# Zbadac zależność między zmiennymi population i gdp_per_capita, do tego celu uzyjemy testu korelacji tau-b
#stworzyc wykres rozrzutu
#stworzyc wykres heatmapy
#pokaż wyniki testu korelacji tau-b
#ustalic czy zależność jest istotna statystycznie


# modele liniowe

titanic %>% select_if(is.numeric) %>% cor() %>% round(2) %>% corrplot::corrplot(method = "number")


ggplot(titanic, aes(x = Age, y = Fare)) + geom_point() + ggtitle("Zależność między zmiennymi age i fare")

cor.test(titanic$Age, titanic$Fare, method = "pearson")
cor.test(titanic$Age, titanic$Fare, method = "spearman")
cor.test(titanic$Age, titanic$Fare, method = "kendall")

lm(Fare ~ Age, data = titanic)

lm(Fare ~ Age, data = titanic) %>% summary()

ggplot(titanic, aes(x = Age, y = Fare)) + geom_point() + ggtitle("Zależność między zmiennymi age i fare") + geom_smooth(method = "lm")

#sciagawka

# Jak dowiedzieć się, czy model jest najlepiej dopasowany do twoich danych?

# 1. Sprawdź wartość R-squared. Powinna być zbliżona do 1.
# 2. Wartość Adj R-Squared powinna być zbliżona do wartości R-Squared.
# 3. Wartość F-statystyki powinna być istotna statystycznie (p-value < 0.05).
# 4. P-value of the F-statistic powinno być mniejsze niż 0,05.
# 5. Wartości AIC i BIC powinny być niskie.
# 6. Wartości MAE, MSE, RMSE powinny być niskie.

