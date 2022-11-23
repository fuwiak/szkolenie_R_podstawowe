library(tidyverse)
library(ggplot2)
library(haven)
library(nortest)
library(rstatix)
# library(corrplot)

install.packages('nortest')

library("ggpubr")

install.packages('rstatix')


#spps
url<-"https://github.com/fuwiak/szkolenie_R_podstawowe/blob/main/PL.sav?raw=true"


# wczytanie danych

PL<-read_sav(url)

class(PL)

# wyświetlenie danych

PL %>% glimpse()
PL %>% head()

# wyświetlenie pierwszych 6 wierszy

PL %>% head()

# wyświetlenie ostatnich 6 wierszy

PL %>% tail(10)

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

titanic %>% glimpse()


# tabele kontyngencji - test chi-kwadrat

# przygotowanie danych

tab <- table(titanic$Survived, titanic$Sex)
tab

# test chi-kwadrat
# H0: nie ma zależności między zmiennymi Survived a Sex
# H1: istnieje zależność między zmiennymi Survived a Sex
chisq.test(tab)


chisq.test(tab)

p<-chisq.test(tab)$p.value

if(p<0.05){
  print("Odrzucamy hipotezę zerową, H1")
}else{
  print("Nie odrzucamy hipotezy zerowej")
}

# Współczynnik V Craméra

V<- sqrt(chisq.test(tab)$statistic/(sum(tab)*(min(dim(tab))-1)))
V

# Współczynnik V Craméra daje w wyniku wartości pomiędzy 0 a +1 (włącznie), przy czym im wynik jest bliżej 0,
# tym słabszy jest związek między badanymi cechami, a im bliżej jest 1, tym silniejszy jest związek między badanymi cechami

# przygotowanie danych

#zadanie 1
# Zbadac zależność między zmiennymi Survived i Pclass
# a) przygotować tabele kontyngencji
# b) przeprowadzić test chi-kwadrat
# c) obliczyć współczynnik V Craméra



# a) przygotować tabele kontyngencji
tab2 <- table(titanic$Survived, titanic$Pclass)
tab2
# b) przeprowadzić test chi-kwadrat
# test chi-kwadrat
# H0: nie ma zależności między zmiennymi Survived i Pclass
# H1: istnieje zależność między zmiennymi Survived i Pclass
chisq.test(tab2)
p2<-chisq.test(tab2)$p.value
p2
if(p2<0.05){
  print("Odrzucamy hipotezę zerową")
}else{
  print("Nie odrzucamy hipotezy zerowej")
}
# c) obliczyć współczynnik V Craméra
V<- sqrt(chisq.test(tab2)$statistic/(sum(tab2)*(min(dim(tab2))-1)))
V



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

# suicides%>% select(suicides_no, sex) %>% aov(suicides_no~sex)


p<-anova$`Pr(>F)`[1]
p # wartosc p

# p<0.05
# Wnioski
# Odrzucamy hipotezę zerową, zmienne są zależne

# Współczynnik V Craméra

V<- sqrt(anova$`F value`[1]/(anova$`F value`[1]+anova$`Num DF`[1]))
V

# Współczynnik V Craméra daje w wyniku wartości pomiędzy 0 a +1 (włącznie), przy czym im wynik jest bliżej 0,
# tym słabszy jest związek między badanymi cechami, a im bliżej jest 1, tym silniejszy jest związek między badanymi cechami

# Zadanie 2

# Zbadac zależność między zmiennymi suicides_no i age, do tego celu uzyjemy testu ANOVA.
# Ustalmy na wstepie ze poziom istotnosci alfa wynosi=0.05. Jesli wartosc Pr(>F) jest mniejsza niz zadany poziom istotnosci,
# mozna bedzie stwierdzic zwiazek miedzy zmienna zmienna age a suicides_no.

# H0: brak wplywu zmiennej age na zmienna suicides_no
# H1: wplyw zmiennej age na zmienna suicides_no

anova2<-aov(suicides_no~age, data=suicides)

summary(anova2)

p<-anova2$`Pr(>F)`[1]
p

alfa = 0.05

if(p2<0.05){
  print("Odrzucamy hipotezę zerową")
}else{
  print("Nie odrzucamy hipotezy zerowej")

# Współczynnik V Craméra

V<- sqrt(anova2$`F value`[1]/(anova2$`F value`[1]+anova2$`Num DF`[1]))
V




# Test t dla grup niezaleznych

# Zalozenia:
# - dane maja byc normalnie rozkladane
# - dane maja byc niezalezne
# - dane maja byc rownorodne
# - wariancja w grupach jest rowna

suicides %>% ggplot(aes(x = suicides_no)) + geom_boxplot() + facet_wrap(~sex)+ggtitle("samobostwa w zależności od płci")

# Sprawdzenie zalozen testu t-studenta


# Normalnosc rozkladu sprawdzamy testem Shapiro-Wilka. Ustalmy na wstepie ze poziom istotnosci alfa wynosi=0.05. Jezeli p-value z testu bedzie wyzsze niz 0.05, nie mamy podstaw, aby odrzucic hipoteze zerowa o normalnosci rozkladu danej zmiennej.

# H0: dane sa rozkladane normalnie
# H1: dane nie sa rozkladane normalnie

suicides %>% group_by(sex) %>% summarise(shapiro.test(suicides_no)$p.value)


suicides %>% dim()


dim(suicides)


library(nortest)
library(stats)
suicides %>% group_by(sex) %>% summarise(ad.test(suicides_no)$p.value)

# Wyniki testu AD pokazują, że dane nie są rozkładane normalnie.

#sprawdzy czy chociaz wariancja jest rowna

alfa<-0.05

# H0: wariancje w dwoch grupach sa sobie rowne
# H1: wariancje w grupach sa rozne


stats::var.test(suicides_no~sex, data=suicides)



# suicides %>% var_test(suicides_no~sex)


# Wnioski
# Warunki testu t-studenta nie sa spelnione, nie mozna zastosowac testu t-studenta.

# W tym wypadku skorzystamy z testu nieparametrycznego Mann-Whitney-Wilcoxona.
# Ustalmy na wstepie ze poziom istotnosci alfa wynosi=0.05.
# Jezeli p-value z testu bedzie wyzsze niz 0.01, nie mamy podstaw,
# aby odrzucic hipoteze ze grupy Male i Female dla zmiennej suicides_no pochodza z tej samej populacji.

alfa<-0.01

# H0: grupy pochodza z tego samego rozkladu
# H1: grupy nie pochodza z tego samego rozkladu



wilcox.test(suicides_no ~ sex, data = suicides)


temp<-suicides %>% mutate(sex_binary=ifelse(sex=="male", 1, 0)) 


suicides %>% select(sex) %>% unique()

# suicides %>% drop_na() %>% wilcox.test(suicides_no ~ sex_binary)


#spawdzmy czy cena biletu zalezy plci - titanic
#uzyjmy testu t - studenta, sprawdzwmy zalozenia:
# 1) czy rozklad jest normalny(shapiro.test)
# 2) rownonsc wariancji
# 3)jesli beda nie spelnione, uzyj testu wilcoxoan

# 4) sprawdzy t studenta(mimo wsyzstko), zeby sprawdzic, czy wynikii testu z punktu 3 beda sie 
# roznic z punktem 4 t.test(x, ...)



titanic %>% ggplot(aes(x = Fare)) + geom_boxplot() + facet_wrap(~Sex)+ggtitle("cena biletu w zależności od płci")


titanic %>% group_by(Sex) %>% summarise(shapiro.test(Fare)$p.value) #1

stats::var.test(Fare~Fare, data=titanic) #2


wilcox.test(Fare ~ Sex, data = titanic) #3


t.test(Fare ~ Sex, data = titanic) #4







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

suicides %>% ggplot(aes(x = suicides_no)) + ggplot(aes(x = gdp_per_capita)) +geom_histogram()



ggplot() + geom_histogram(data = suicides, aes(x = suicides_no), alpha = 0.3) +geom_histogram(data = suicides, aes(x = gdp_per_capita), alpha = 0.7)+geom_point()+ theme(legend.position = "bottom")





stats::var.test(suicides$gdp_per_capita, suicides$suicides_no)


# Wyniki testu AD pokazują, że dane nie są rozkładane normalnie.

# Korelacja Spearmana to analiza pozwalająca korelować ze sobą zmienne na skali porządkowej oraz ilościowym nieposiadające rozkładu normalnego
cor.test(suicides$suicides_no, suicides$gdp_per_capita, method = "spearman")

cor.test(suicides$suicides_no, suicides$gdp_per_capita, method = "pearson")

cor.test(suicides$suicides_no, suicides$gdp_per_capita, method = "spearman")$p.value

# Wnioski

# p<0.05, więc zmienna gdp_per_capita ma wpływ na zmienną suicides_n, ale
# korelacja jest słaba i nie jest mocno istotna statystycznie.


#select only numeric variables for correlation and plot heatmap
suicides_num <- suicides %>% select_if(is.numeric)


#wykres rozrzutu

suicides_num %>% select(suicides_no, gdp_per_capita) %>% ggplot(aes(x = suicides_no, y = gdp_per_capita)) + geom_point() + ggtitle("Zależność między zmiennymi suicides_no i gdp_per_capita")

#plot heatmap annotating with correlation values

suicides_num %>% cor(method='spearman') %>% round(2) %>% corrplot::corrplot(method = "number")

#tau kendall correlation

suicides_num %>% cor(method = "kendall") %>% round(2) %>% corrplot::corrplot(method = "number") 
# Zadanie 4

# Zbadac zależność między zmiennymi population i gdp_per_capita, do tego celu uzyjemy testu korelacji tau-b
#stworzyc wykres rozrzutu
#stworzyc  heatmapy
#pokaż wyniki testu korelacji tau-b
#ustalic czy zależność jest istotna statystycznie


# ggplot(suicides, aes(x = population, y = gdp_per_capita)) + geom_point() + ggtitle("Zależność między zmiennymi population i gdp_per_capita")


suicides %>% slice_sample(n=100)  %>% ggplot(aes(x = population, y = gdp_per_capita)) + geom_point() + ggtitle("Zależność między zmiennymi population i gdp_per_capita")


suicides  %>% select(population,gdp_per_capita ) %>% cor(method = "kendall") 

# suicides  %>% select(population,gdp_per_capita ) %>% cor(method = "kendall")   %>% summary()

cor.test(suicides$population, suicides$gdp_per_capita, method = "spearman")$p.value

# modele liniowe

suicides %>% slice_sample(n=100)  %>%  select_if(is.numeric) %>% cor(method = "kendall") %>% round(3) %>% corrplot::corrplot(method = "number")


ggplot(titanic, aes(x = Age, y = Fare)) + geom_point() + ggtitle("Zależność między zmiennymi age i fare")

cor.test(titanic$Age, titanic$Fare, method = "pearson")
cor.test(titanic$Age, titanic$Fare, method = "spearman")
cor.test(titanic$Age, titanic$Fare, method = "kendall")

lm(Fare ~ Age, data = titanic)

lm(Fare ~ Age, data = titanic) %>% summary()

ggplot(titanic, aes(x = Age, y = Fare)) + geom_point() + ggtitle("Zależność między zmiennymi age i fare") + geom_smooth(method = "lm")


lm(Fare ~ Age, data = titanic) %>% summary()

#show model coefficients
lm(Fare ~ Age, data = titanic) %>% summary() %>% .$coefficients %>% as.data.frame() %>% select(1)

#show model coefficients on plot

ggplot(titanic, aes(x = Age, y = Fare)) + geom_point() + ggtitle("Zależność między zmiennymi age i fare") + geom_smooth(method = "lm", se = FALSE) + geom_text(aes(label = round(lm(Fare ~ Age, data = titanic) %>% summary() %>% .$coefficients %>% as.data.frame() %>% select(1), 2)), x = 20, y = 200, size = 5)

#show r2
lm(Fare ~ Age, data = titanic) %>% summary() %>% .$r.squared


ggplot(titanic, aes(x = Age, y = Fare)) + geom_point() + ggtitle("Zależność między zmiennymi age i fare") + geom_smooth(method = "lm", se = FALSE) + geom_text(aes(label = round(lm(Fare ~ Age, data = titanic) %>% summary() %>% .$r.squared, 2)), x = 20, y = 200, size = 5)

#show r2 and model coefficients on plot

ggplot(titanic, aes(x = Age, y = Fare)) + geom_point() + ggtitle("Zależność między zmiennymi age i fare") + geom_smooth(method = "lm", se = FALSE) + geom_text(aes(label = round(lm(Fare ~ Age, data = titanic) %>% summary() %>% .$r.squared, 2)), x = 20, y = 200, size = 5) + geom_text(aes(label = round(lm(Fare ~ Age, data = titanic) %>% summary() %>% .$coefficients %>% as.data.frame() %>% select(1), 2)), x = 20, y = 150, size = 5)
#sciagawka

# Jak dowiedzieć się, czy model jest najlepiej dopasowany do twoich danych?

# 1. Sprawdź wartość R-squared. Powinna być zbliżona do 1.
# 2. Wartość Adj R-Squared powinna być zbliżona do wartości R-Squared.
# 3. Wartość F-statystyki powinna być istotna statystycznie (p-value < 0.05).
# 4. P-value of the F-statistic powinno być mniejsze niż 0,05.
# 5. Wartości AIC i BIC powinny być niskie.
# 6. Wartości MAE, MSE, RMSE powinny być niskie.

PL %>% head()


# PL, cases,deaths

#wykres punktowy, dopasowac do niego funckje liniowa, pokaz pametry a i b (y=a*x+b)
# gdzie a = slope, b = intercept
# podac wartosc R, R2 tego dopasowania


ggplot(PL, aes(x = cases, y = deaths)) + geom_point() + ggtitle("Zależność między zmiennymi cases i deaths")+ geom_smooth(method = "lm", se = FALSE)

cor.test(PL$cases, PL$deaths, method = "pearson")
# cor.test(PL$cases, PL$deaths, method = "spearman")
# cor.test(PL$cases, PL$deaths, method = "kendall")

lm(cases ~ deaths, data = PL) 

lm(cases ~ deaths, data = PL) %>% summary() %>% .$r.squared

lm(cases ~ deaths, data = PL) %>% summary() %>% .$coefficients %>% as.data.frame() %>% select(1)




R2<-lm(cases ~ deaths, data = PL) %>% summary() %>% .$r.squared

paste("R2=", R2)
paste("R=", sqrt(R2))


