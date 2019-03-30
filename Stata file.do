

use suicide.dta, clear
describe


/*STATYSTYKI OPISOWE*/
summarize so
help summarize
summarize so,detail

/*HISTOGRAM - ANALIZA ROZKLADU ZMIENNEJ*/
histogram so, normal

/*BOX-PLOT - ANALIZA ROZKLADU, OBSERWACJE NIETYPOWE, BLEDNE*/
graph box so
graph hbox so

/*JARQUE-BERRA TEST - CZY ZMIENNA MA ROZKLAD NORMALNY*/
/*HO - ZMIENNA MA ROZKLAD NORMALNY */
sktest so, noadj /* adj - gdy sa problemy z liczeniem statystyki testowej
/*WNIOSEK: p value = 0 < alfa = 0.05, odrzucam h0, zmienna nie ma rozkladu normalnego*/
*/

/**********************/
/*ZMIENNE OBJASNIAJACE*/
/**********************/

/*DOCH”D - ZMIENNA CIAGLA*/
/*dochg*/

/*STATYSTYKI OPISOWE*/
summarize dochg
summarize dochg, detail

/*HISTOGRAM*/
histogram dochg, normal

/*BOX-PLOT */
graph box dochg

/*JARQUE-BERRA TEST */
sktest dochg, noadj

/*zaleznosc so i dochg*/
scatter so dochg

/*KORELACJA*/
/*PEARSON CZY SPEARMAN?*/
/*KORELACJA PEARSONA - ZMIENNE MUSZA MIEC ROZKLAD NORMALNY*/
pwcorr so dochg, star(.05) /*CZY WSPOLCZYNNIK KORELACJI PEARSONA JEST TU ODPOWIEDNI?*/
                                                                              
spearman so dochg
spearman so dochg, matrix
spearman so dochg, matrix star(.05)


/*SEX - ZMIENNA BINARNA*/

tabulate sex /*UWAGA!!! KOMENDA SUMMARIZE NIE MA TU SENSU*/
/*STOPA OSZCZEDNOSCI A PLEC*/
bys sex: summarize so
tabstat so, by(sex) stat(n mean sd)
histogram so, by(sex)

/*CZY ZMIENNA SO MA ROZKLAD NORMALNY W PODGRUPACH WYZANCZONYCH PRZEZ PLEC*/
sktest so if sex==1, noadj
sktest so if sex==2, noadj

/*CZY SREDNIO STOPA OSZCZEDNOSCI DLA MEZCZYZN JEST TAKA SAMA JAK STOPA OSZCZEDNOSCI DLA KOBIET*/
/*TTEST (TEST PARAMETRYCZNY) CZY WILCOXON MANN-WITHNEY (TEST NIEPARAMETRYCZNY)*/
/*TTEST - W MALYCH GRUPACH WYMAGANY JEST ROZKLAD NORMALNY ZMIENNEJ*/
ttest so, by(sex) unequal
ttest so, by(sex) 
/*Jaka jest roznica miedzy dwoma powyzszymi komendami?*/

/*Chcemy  zbadaÊ, czy úrednio stopa oszczÍdnoúci dla mÍøczyzn jest taka
sama jak stopa oszczÍdnoúci dla kobiet. 
Pos≥uøymy siÍ w tym celu T testem, ale aby go zastosowaÊ,
musimy najpierw  sprawdziÊ, czy wariancje w obu tych podgrupach sπ takie same, 
czy teø nie.*/
sdtest so, by(sex)

/*zatem ktÛry test wybieramy:
ttest so, by(sex) unequal
ttest so, by(sex) 
?*/
ttest so, by(sex) unequal

/*A moze jednak WILCOXON MANN-WITHNEY (TEST NIEPARAMETRYCZNY)?*/
ranksum so, by(sex) // odrzucam h0
/*KIEDY signrank?*/
help signrank 
/*KOMENDA SINGRANK SLUZY DO PRZEPROWADZENIU TESTU ...................*/
signrank so=so2
/*http://www.statajournal.com/sjpdf.html?articlenum=st0007*/

/*WYKSZTALCENIE */
/*gredu*/

/*STATYSTYKI OPISOWE*/
tabulate gredu

/*STOPA OSZCZEDNOSCI A WYKSZTALCENIE*/
bys gredu: summarize so
histogram so, by(gredu)

/*WYKRES WARTOSCI SREDNICH*/
preserve
collapse so, by(gredu) 
list
scatter so gredu, title(Srednia stopa oszczednosci dla poziomow wyksztalcenia)
restore 

/*CZY SO MA ROZKLAD NORMALNY W PODGRUPACH*/
sktest so if gredu==1
sktest so if gredu==2
sktest so if gredu==3
sktest so if gredu==4

/*SREDNIA SO DLA OSOB Z WYKSZTALCENIEM WYZSZYM = SREDNIA SO DLA OSOB Z WYKSZTLACENIEM SREDNIM =...?*/
/*ANOVA CZY KRUSKAL-WALLIS*/
/*ANOVA - DLA MALYCH PROB WYMAGANY JEST ROZKLAD NORMALNY*/

anova so gredu

kwallis so, by(gredu) 
/*WNIOSEK: */
/*ALE GDZIE JEST ROZNICA, KTORE GRUPY SIE ROZNIA?*/
/*TEST TUCKEYA*/
tukeyhsd gredu

/*GENEROWANIE NOWEJ ZMIENNEJ BINARNEJ*/

gen rich = 0 
replace rich = 1 if dochg>5000

tabulate rich
bys rich: summarize so


browse /*opens a spreadsheet, you can look at the data, but yoou can not change the data*/
edit /*you can look and change the data*/
describe /*information on the size of the data set and the names, labels and types of variables*/

codebook, all /* information about the dataset and all variables*/

reg y x1 x2 x3 x4 /*estimation of the initial model*/

/***NOT TYPICAL OBSERVATIONS***/

predict y_fitted, xb /*fitted values - wartoúci dopasowane*/
predict residual, r /*residuals - reszty*/
predict residual_st, rstandard /*standarized residuals - standaryzowane reszty*/
predict leverage, leverage /*leverage - düwignia */
predict cook_dist, cooksd /*Cook distance - odleg≥oúÊ Cooka*/

list residual-cook_dist /*order is important*/ 
list residual-cook_dist, abbreviate(12) /*change heads*/    
   
/*We are interested in observations with high standarized residuals and leverages*/
/*Interesujπ nas obserwacje o duøych resztach standaryzowanych i düwigni*/
sort residual_st 
list y x1 x2 x3 x4 leverage residual_st cook_dist if  leverage > 2*e(df_m)/e(N) 
list y x1 x2 x3 x4 leverage residual_st cook_dist if  abs(residual_st) > 2

list y x1 x2 x3 x4 leverage residual_st cook_dist if  leverage > 2*e(df_m)/e(N) & abs(residual_st) > 2
/*List observations with leverage larger than 2K/N and |standarized residuals| larger than 2*/
/*Wyúwietla te obserwacje, dla ktÛrych düwignia jest wiÍksza od 2K/N 
i wartoúÊ bezwzglÍdna reszt standaryzowanych od 2*/ 
display 2*e(df_m)/e(N) 
display 4/e(N)
/*if Cook distance is larger than tis value we deal with influential observation*/
/* 4/N - przekroczenie tego progu dla odleg≥oúci Cooka wskazuje na obserwacjÍ znaczπcπ*/ 

/***COLLINEARITY - WSP”£LINIOWOå∆***/

graph matrix x1-y
/*graph of dispersal for every pair of variables*/
/*strong relation between y-x2, y-x1 can be seen*/
/*"macierzowy" wykres rozproszenia - dla kaødej pary zmiennych rysowany jest wykres rozproszenia*/
/*Na wykresie jest widoczna silna zaleønoúÊ miÍdzy zmiennymi y-x2, y-x1*/

pwcorr x1-y, star(0.05) 
/*Pearson correlation*/
/*Macierz korelacji*/
/*wyznaczana jest wsp. korelacji Pearsona, jeúli korelacja jest istotna na poziomie 
istotnoúci 0,05 to zostanie zaznaczona przy pomocy gwiazdki*/ 
/*wniosek z wykresÛw zostaje potwierdzony, 
silna i statystycznie istotnej korelacja miÍdzy y-x2, y-x1*/ 

/*regression*/
regress y x1 x2 x3 x4
regress suicide temperature unemployment gdppc  population 
estimates store est1
/*Zmienne x3 oraz x4 nieistotne na poziomie 0,05*/

test x3 x4 
/*are x3 and x4 jointly insignificant, test F*/
/*Testujemy ich ≥πcznπ nieistotnoúÊ za pomocπ statystyki F*/
/*≥πcznie zmienne x3 i x4 sπ nieistotne na poziomie istotnoúci 0,1. 
Gdyby lπcznie byly istotne ich usuniÍcie znaczπco pogarszyloby dopasowanie*/

help vif
/*what is the infuence of correlation on estimation results*/
/*statystyka VIF umoøliwia zmierzenie wplywu korelacji na wynik estymacji*/
/*wspolliniowoúÊ obniøa precyzjÍ oszacowaÒ*/
vif 
/*computes VIF for the last regression*/
/*lack of large values, (VIF>10)*/
/*Wyliczany jest VIF dla ostatnio przeprowadzonej regresji*/
/*brak wysokich wartoúci VIF dla zmiennych*/

quietly regress y x1 x2 x3 x4 
/*no results in RESULTS WINDOW*/
/*quietly - Stata nie wyrzuca wynikÛw do okna RESULTS*/
estimates store model_1 
/*results remembered as model_1*/
/*ZapamiÍtanie wynikÛw pod nazwπ model_1*/

quietly regress y x1 x2 x3
estimates store model_2 

quietly regress y x1 x2
estimates store model_3 

/*Table with the results*/
/*Tworzymy tabelÍ z wynikami*/
estimates table model_1 model_2 model_3, stat(r2 r2_a F) b se p
estimates table model_1 model_2 model_3, stat(r2 r2_a F) b(%4.3f) se(%4.3f) p(%4.3f)  

/*test RESET for function form*/
/* prawid≥owoúÊ formy funkcyjnej - test RESET*/
regress y x1 x2 x3 x4

ovtest, rhs 
/*test for joint insignificance of independent variables to the power of 2,3,4 */
/*w regresji pomocniczej testowana jest ≥πczna nieistotnoúÊ 
zmiennych objaúniajπcych podniesionych do 2,3 i 4 potÍgi. */

/*RESIDUALS NORMALITY - normalnoúÊ reszt*/
regress y x1 x2 x3 x4
predict residuals2, r 

/*gaph analysis*/
/*analiza graficzna*/
histogram residuals2, normal name(r1) title("Histogram")
graph box residuals2, name(r2) title("BOX")
qnorm residuals2, name(r3) title("Wykres kwantylowy") /*Analiza ogonow rozk≥adu*/ /*tails of the distribution*/ 
/*plots the quantiles of varname against the quantiles of the normal distribution (Q-Q plot)*/

pnorm residuals2, name(r4) title("Wykres prawdopodbieÒstwa") 
/*Analizuje úrodkowπ czÍúÊ rozk≥adu, "Standardized normal probability plot"*/
help qnorm
graph combine r1 r2 r3 r4, title("Analiza Graficzna Reszt") 

/*Test Jarque-Berra, H0: normality*/
sktest residuals2

/*P-value = 0.3766, we do not reject H0*/

/*heteroscedasticity*/
regress y x1 x2 x3 x4
hettest, rhs /*Breusch Pagan*/
imtest, white /*White*/

/*heteroscedasticity - OLS estimator unbiased and consistent
but variance-covarience matrix estimation for vector "b" is biased and unconsistent,
what can influence on testing for significant variables*/
/*solution- White heteroscedasticity consistent estimator*/

/*HeteroskedastycznoúÊ - estymator MNK jest nieobciπøony i zgodny, 
ale obciπøone i niezgodne jest oszacowanie macierzy 
wariancji-kowariancji wektora "b", a to moøe mieÊ wp≥yw na wyniki testowania hipotez 
o istotnoúci zmiennych.
Najprostsze rozwiπzanie: zastosowanie "odpornej" macierzy wariancji-kowariancji White 
(zgodny estymator)*/

regress y x1 x2 x3 x4, robust 


/*parameters stability - CHOW TEST*/
/* test stabilnoúci parametrÛw - TEST CHOWA*/
regress y x1 x2 x3 x4

/*in specifying a regression model, we assume 
that its assumptions appy to all the observations in our sample*/
/*CHOW TEST tests for this assumption*/
/* we will check if estimated parameters are the same for the first 50 observations
and for the rest*/

/*W celu stwierdzenia, czy wspÛ≥czynniki regresji sa takie same dla wszystkich obserwacji w
zbiorze, nalezy przeprowadzic test Chowía. Hipoteza zerowa tego testu mÛwi o tym, ze wspÛ≥czynniki
regresji w rÛznych grupach obserwacji sa takie same. Hipoteza alternatywna ñ
ze wspÛ≥czynniki regresji w rÛznych grupach obserwacji rÛznia sie od siebie. Przyk≥adowo,
w celu sprawdzenia, czy parametry regresji sa takie same dla pierwszych 50 obserwacji, jak
dla pozosta≥ych 50 obserwacji, nalezy utworzyc nastepujace zmienne:*/

gen d=0
gen dx1=0
gen dx2=0
gen dx3=0
replace d=1 if t>50
replace dx1=x1 if t>50
replace dx2=x2 if t>50
replace dx3=x3 if t>50

/*variable d defines 2 groups of observations,
variables dx create interactions*/
/*Zmienna d rozrÛznia grupy obserwacji,
zmienne dx tworza odpowiednie interakcje.*/
reg y x1 x2 x3 d dx1 dx2 dx3

/*we get CHOW TEST results testing for joint insignificance of d, dx1, dx2, dx3*/
/*Wyniki testu Chowa otrzymuje sie testujac ≥aczna nieistotnosc zmiennych: d, dx1, dx2, dx3, za
pomoca polecenia:*/
test (d=0) (dx1=0) (dx2=0) (dx3=0)
/*we don't reject the null*/
/*parameter estimators don't differ in created subgroups*/
/*W tym przypadku widac, ze hipoteza zerowa nie jest odrzucana i, ze oszacowania parametrÛw w
obydwu podprÛbach nie sa od siebie istotnie rÛzne.*/


/*Box-Cox Transformation*/

boxcox y x4, notrans(x1 x2 x3) model(lambda) 
help boxcox

/*Box-Cox transformation of dependent variable only*/
...

