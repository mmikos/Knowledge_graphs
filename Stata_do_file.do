
/*Correlation without sparse variables in 2014*/

spearman suicide temperature unemployment gdppc population if year_int==2014, matrix star(.05)

/*Correlation in Europe id=1*/
spearman suicide temperature unemployment gdppc population if continent_id==1, matrix star(.05)

/*Mean by continent in 2008*/
tabstat suicide if year_int==2008, by(continent) stat(n mean sd)

/*Mean by continent in 2014*/
tabstat suicide if year_int==2014, by(continent) stat(n mean sd)

/*Fit the regression line*/
reg suicide temperature unemployment education poverty gdppc migration population


/*Eliminate the variables that have sparsity in data and fit again*/
reg suicide temperature unemployment gdppc population 

reg suicide temperature unemployment gdppc population if continent_id==1

/*Remove irrelevant variable*/
reg suicide temperature unemployment population 
