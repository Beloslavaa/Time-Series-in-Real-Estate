
/**************************************************************************************************************/

/*Vamos a trabajar con la base de datos VIVIENDAS, estudiando la venta de viviendas de segunda mano en España*/

libname c "C:\Users\User\Documents\Series Temporales";

/*preparar datos*/
data viviendas;
	set c.viviendas;
	fecha = mdy(mes, 1, year);
	format fecha date9.;
	keep fecha segunda_mano;
run;

proc print data=viviendas(obs=10);run;


/*representación gráfica*/
symbol i=j;
proc gplot data=viviendas;
	plot segunda_mano*fecha;
run;

/*análisis descriptivo*/
proc means data=viviendas;
    var segunda_mano; 
run;


/*autocorrelaciones*/
proc timeseries data=viviendas plots=corr
outcorr=autocorrel;
var segunda_mano;
id fecha interval=month;
run;

/*medias móviles*/
proc expand data=viviendas out=medias_moviles;
 convert segunda_mano=media_movil12 / transformout=(cmovave 12); /*por año*/
 convert segunda_mano=media_movil4 / transformout=(cmovave 4); /*por trimestre*/
run;

proc gplot data=medias_moviles;
	plot (media_movil12 media_movil4)*time / overlay legend;
run;

/* Observamos una serie temporal que ha sufrido dos bajones, uno en la crisis de 2008 y otro durante la 
pandemia de COVID-19, cuando las ventas de viviendas bajan drasticamente. Fuera de esto, se puede observar una
tendencia creciente en la venta de viviendas - si partimos los datos en venta desde 2008 hasta 2020 la tendencia
es más que clara y también lo es a partir del bajón de 2020 hasta ahora. En media para todo el periodo la venta
ha sido de 3.4 con una desviación típica de 1.376. 

Hablando de autocorrelaciones, podemos observar una alta correlacion que va decreciendo lentamente para
los retardos más tardíos, que puede indicar la existencia de estacionalidad, que también se puede observar
en el gráfico inicial. El en gráfico de PACF podemos ver que las  autocorrelaciones parciales para los 3 primeros
retardos son significativas, tanto como el retardo 13, que puede ser una indicación de un ciclo existiente 
para cada año por ejemplo.

Finalmente, creamos dos filtros de medias móviles centradas para 12 meses (1 año) y 4 meses (1 trimestre)
para poder suavizar la serie del ruido y observar más claramente sus carateristicas. Con el primer filtro
podemos observar la tendencia que sospechamos que será un factor predominante y la estacionalidad de la serie*/

/*descomposición de la serie*/

data viviendas1;
set viviendas;
d=abs(segunda_mano-lag4(segunda_mano));
h=segunda_mano/lag4(segunda_mano);
run;

proc means data=viviendas1 cv;
var d h;
run;

/*h<d,indicación de descomposición multiplicativa*/

/*Salvo los bajones abruptos, gráficamente es dificil concluir si se observan dientes de sierra más agudos con el
paso de tiempo o no, para asegurarnos optamos por un método matematico (coeficiente de variación) con
que concluimos que vamos a hacer una descomposición multiplicativa*/

/* plot media*desviacion*/
data viviendas2; set viviendas; year=year(fecha); run; 

proc means data=viviendas2;
by year;
output out=ele mean=media std=desviacion;
var segunda_mano;
run;

proc print data=ele; run;
proc sort data=ele; by media; run;

proc gplot data=ele;
plot desviacion*media; run;
/*se puede observar que esta creciendo, que también es indicación que hace falta 
descomposición multiplicativa*/

proc timeseries data=viviendas print=decomp plots=(corr decomp) 
outdecomp=descomposicion outcorr=autocorrel;
var segunda_mano;
id fecha interval=month;
decomp orig tcc sc ic sa tc cc/ mode=multiplicative;
run;

proc timeseries data=descomposicion outcorr=corr2
plots=corr;
var ic;
run;

/*.. y también la descomposición multiplicativa de x11. La descomposición x11 funciona mejor con datos 
donde ha "influido el ser humano"*/

proc x11 data=viviendas;
var segunda_mano; 
monthly date=fecha;
tables d8;
output out=dex_x11 b1=original d10=c_estacional d12=c_tendencia d13=c_irregular d11=sa;
run;

proc print data=dex_x11(obs=36); run;

proc timeseries data=dex_x11 outcorr=corr3
plots=corr;
var c_irregular;
run;

/* De los estadísticos obtenidos, se puede confirmar que tenemos una estacionalidad móvil, es decir,
no tenemos una estacionalidad constante a lo largo del tiempo

No hemos conseguido ruido blanco en la componente irregular en ninguna de las descomposiciones, 
pero seguimos estudiando la serie para decidir si vamos a quedarnos con la componente ajustada sin estacionalidad
o con la componente irregular.

*/

/*estudio de tendencias y ciclos*/

proc timeseries data=viviendas plots=(spectrum periodogram)
outdecomp=descomp outspectra=spec;
var segunda_mano;
id fecha interval=month;
spectra p s/ adjmean=yes;
run;

proc spectra data=viviendas out=espectro p s adjmean whitetest;
var segunda_mano;
run;

proc sort data=espectro; by descending  p_01; run;
proc print data=espectro(obs=10); run;

/*Se puede notar la existencia de una tendencia, que se esta "comiendo" los ciclos, es decir, siendo tan predominante
la tendencia, no podemos captar bien los ciclos con el periodograma. Dado que los datos son mensuales, para conseguir
deshacernos de la tendencia para observar ciclos potenicales más claramente, vamos a hacer una diferenciación con 
el mismo més del año pasado*/

data viviendas; set viviendas;
vivlag=segunda_mano-lag12(segunda_mano);
vivlag2=vivlag-lag(vivlag); /*quitar estacionalidad y tendencia*/
run;

proc gplot data=viviendas;
plot vivlag*fecha;
run;

proc timeseries data=viviendas plots=(spectrum periodogram)
outdecomp=descomp_lagged outspectra=spec;
var vivlag;
id fecha interval=month;
spectra p s/ adjmean=yes;
run;

proc spectra data=viviendas out=espectro p s adjmean whitetest;
var vivlag;
run;

proc sort data=espectro; by descending  p_01; run;
proc print data=espectro(obs=10); run;

proc gplot data=viviendas;
plot vivlag2*fecha;
run;

/*Al quitar la tendencia se pueden notar ciclos de 44 meses, alrededor de 4 años, que puede ser influenciado 
por ciclos economicos, ciclos de tasas de interés e incluso políticas gubernamentales (cada 4 años) */

/*Analisis Detrended*/

/*en este apartado vamos a intentar modelizar la serie de forma detrended. Ya que obtuvimos relativamente
buenos resultados en la componente irregular para las descomposiciones multiplicativas, vamos a 
usar estos para obtener la modelización*/

proc timeseries data=viviendas print=decomp plots=(corr decomp)
 outdecomp=descomposicion outcorr=autocorrel;
 var segunda_mano;
 id fecha interval=month;
 decomp orig tcc sc ic sa tc cc/ mode=multiplicative;

data descomposicion; 
set descomposicion;
t+1;
t2=t**2;
t3=t**3;
t4=t**4;
pi=constant("pi");
seno=sin(2*pi*t/44);
coseno=cos(2*pi*t/44);
drop pi;
run;

proc reg data=descomposicion;
model sa= t3 seno coseno / method=stepwise;
output out=regre p=previsto r=residuo;
run;

proc gplot data=regre;
plot (previsto sa)*fecha / overlay legend;
run;

proc timeseries data=regre outcorr=corr4
plots=corr;
var residuo;
run;

proc arima data=regre;
identify var=residuo;
*estimate p=2 noint; run; /*correlacion entre parametros, tenemos que buscar otro modelo*/
estimate p=1 q=1 noint ;
forecast out=datos_con_residuos outresid=residuos;
run;

proc print data=datos_con_residuos; run;

/*(1-0.93991B)Yt =(1-0.31938B)et
?
*/

data solu;
set regre;
	*mimodelo=previsto+0.63883*lag(residuo)+ 0.26179*lag2(residuo);
	mimodelo=0.93991*lag(previsto)+residuo-0.31938*lag(residuo);
	mierror=original-mimodelo;
run;

symbol w=1 v=none;
proc gplot data=solu;
plot (original mimodelo)*fecha /overlay legend;
run;

/*suavizado exponencial*/

/*Dado que hemos estudiado ambos la presencia de estacionalidad y tendencia, hará falta hacer un suavizado triple
de Holt-Winters, multiplicativo, por lo estudiado también previamente*/

proc esm data=viviendas print=estimates 
outfor=winters printdetails;
forecast segunda_mano /model=multwinters;
id fecha interval=month;
run;

proc gplot data=winters;
plot (actual predict)*fecha / overlay legend;
run;

/*ARIMA*/

/*Antes de aplicar ARIMA, necesitamos una serie estacionaria, cuya varianza y media se mantienen constantes a lo largo 
del tiempo. La variable segunda_mano no cumple con ningún requisito, así que vamos a usar vivlag2, que es la
serie original, diferenciada 2 veces, por un lado diferenciada para eliminar la estacionalidad y luego otra vez
para deshacerse de la tendencia. A primera vista es estacionaria*/

/*Primer paso del análisis es comprobar estacionariedad y hacer transformaciones si hace falta*/

proc autoreg data=viviendas;
    model vivlag2= /stationarity=(adf,kpss=(kernel=qs)); 
run; 

/*comprobamos homogenidad de varianza*/

data viviendas;
set viviendas;
year=year(fecha);
run;

proc means data=viviendas noprint;
var vivlag2;
by year;
output out=med_viv mean=m_viv std=std_viv;
run;

data med_viv;
set med_viv;
logm_viv=log(m_viv);
logstd_viv=log(std_viv);
run;

proc reg data=med_viv;
model logstd_viv=logm_viv;
run;
/*hay homogenidad de varianza ya que la relación no es significativa, la variable
independiente logm_viv no es significativa, tenemos varianza constante*/

/*Se ve que se tenemos estacionariedad en esta serie vivlag2, podemos aplicar ARIMA. De todas formas, con fines 
didacticos vamos a analizar la variable segunda_mano y aplicar transformaciones*/

 proc autoreg data=viviendas;
    model segunda_mano= /stationarity=(adf,kpss=(kernel=qs)); 
run; 

/* En el KPSS Stationarity Test firmemente se rechaza la estacionariedad. Dado que esta serie es la original, 
tiene solo valores positivos, así que podemos aplicar transformaciones logaritmicas. */

proc means data=viviendas noprint;
var segunda_mano;
by year;
output out=med_viv mean=m_viv std=std_viv;
run;

data med_viv;
set med_viv;
logm_viv=log(m_viv);
logstd_viv=log(std_viv);
run;

symbol v=dot i =none;
proc gplot data=med_viv;
plot logm_viv*logstd_viv;
run;

/*Hacemos regresion logstd_viv=a+b*logm_viv regresión lambda=1-pendiente de regresión de logaritmo*/

proc reg data=med_viv;
model logstd_viv=logm_viv;
run;

/*La regresión es sifnificativa con un p-valor 0.0194, suponemos que NO hay varianzas constantes, 
hace falta transformar*/

/*b=0.75128 significativo, esto implica que lambda=1-b=0.25

Una regla general que podemos aplicar es que siempre cuando la lamda es ente -0.3 y 0.3 la mejor transformación 
es la logaritmica*/


proc transreg data=viviendas;
model boxcox(segunda_mano / lambda = (-2 to 2 by 0.01))=
identity(year);
run;

/*se consique una corrección lambda para varianza constante de 0.35*/

data viviendas;
set viviendas;
viv_boxcox25=((segunda_mano**0.25)-1)/0.25;
viv_boxcox35=((segunda_mano**0.35)-1)/0.35;
viv_log=log(segunda_mano);
viv_loglag=viv_log-lag(viv_log);/*otra transformación socorrida 
						y en este caso como hay tendencia --> log(yt)-log(yt-1)*/
run;


SYMBOL I=J W=1 v=none;
PROC GPLOT DATA=viviendas; 
  PLOT viv_loglag*fecha; 
  RUN;


proc autoreg data=viviendas;
    *model viv_log= /stationarity=(adf,kpss=(kernel=qs)); 
	*model viv_boxcox25= /stationarity=(adf,kpss=(kernel=qs)); 
	*model viv_boxcox35= /stationarity=(adf,kpss=(kernel=qs)); 
	model viv_loglag= /stationarity=(adf,kpss=(kernel=qs)); 
run; 

/*conseguimos estacionariedad con log(yt)-log(yt-1), podemos seguir construyendo un modelo ARIMA para dicha
serie transformada*/

SYMBOL I=J W=1 v=none;
PROC GPLOT DATA=viviendas; 
  PLOT viv_loglag*year; 
  RUN;

proc arima data=viviendas;
identify var=viv_loglag /*minic scan p=(0:12) q=(0:12)*/; run;
*estimate q=1 p=3 noint; run;
*estimate p=(2,3) noint; run;
*estimate p=5 q=3 noint; run;
estimate p=(1, 2,21) q=(12,24) noint; run; /*AIC=-211.965 BIC=-199.062*/

/*ARIMA para la otra serie vivlag2*/

proc arima data=viviendas;
identify var=vivlag2 minic scan p=(0:12) q=(0:12); run;
estimate p=2 q=(12) noint; run; /*AIC=229.4626 BIC=238.9398*/


/*Comarando por AIC y BIC, elegimos el modelo transformado*/


/*Examen de outliers y determinación de variables escalón*/

symbol i=j;
proc gplot data=viviendas;
plot vivlag2*fecha;
run;

proc arima data=viviendas; 
   identify var=vivlag2;run;
   estimate p=2 q=(12) noint; run;
   outlier maxnum=6 /*los 6 outliers más significativos, menores que 0.001*/ 
	alpha=0.001 id=fecha;run;

/* Se han encontrado dos datos atípicos, que en este caso son impulsos (additive) en ls observaciones 171 y 151.
Vamos a crear una variable ficticia con que podemos explicar estos impulsos*/

data viviendas;
set viviendas;
t+1;
run;

data viviendas; 
set viviendas; 
 I_171=(t=171); /*crea nueva variable que valga 1 solo en caso de t=171, el resto es 0*/
 I_159=(t=159);
 I_177=(t=177);
run; 

proc print data=viviendas; run;

proc arima data=viviendas; 
   identify var=vivlag2 crosscorr=(I_159 I_171 I_177); run;
   estimate p=2 q=(12) input=(I_159 I_171 I_177) noint; run;
   outlier maxnum=6 id=t alpha=0.001;run; /*tenemos otro en 177, que quitamos, no tenemos más outliers*/
   forecast out=predict id=t;run;

/*escalon*/

/*data viviendas;
set viviendas;
t+1;
run;*/

proc autoreg data=predict plots=(acf pacf);
model residual=	/bp=(m=5); 
run;  
/*con esta forma, aplicandolo a los residuos compruebo la estabilidad de la serie, 
que significa que puedo hacer predicciones fiables. Sale que se recomienda solo un break, 
que demuestra la estabilidad del modelo*/

/* Determinación de relación entre series temporales */

   data hipotecas;
	set c.hipotecas;
	fecha = mdy(mes, 1, year);
	format fecha date9.;
	keep fecha interes_fijo interes_variable;
run;

proc print data=hipotecas(obs=10);run;

proc gplot data=hipotecas;
plot interes_variable*fecha; run;

data hipotecas;
set hipotecas;
hiplag=interes_variable-lag12(interes_variable);
hiplag2=hiplag-lag(hiplag);
hiplag=interes_variable-lag(interes_variable);
hip_log=log(interes_variable);
hip_loglag=hip_log-lag(hip_log);
run;


proc gplot data=hipotecas;
plot hiplag2*fecha; run;

proc gplot data=hipotecas;
plot hip_loglag*fecha; run;

proc autoreg data=hipotecas; 
   model hip_loglag= / stationarity=(adf, kpss=(kernel=qs)); 
   run;

proc sql;
   create table conjunto as
   select a.vivlag2, a.viv_loglag, b.interes_variable, b.hip_loglag, a.fecha
   from viviendas as a
   inner join hipotecas as b
   on a.fecha = b.fecha;
quit;

proc print data=conjunto(obs=10); run;


proc arima data=conjunto;
identify  var=viv_loglag crosscorr=hip_loglag; run;
estimate p=(1 2)(12)(21) input=(hip_loglag) noint; run;
*estimate p=(1, 2) q=(12,24) input=(hip_loglag) noint; run;
forecast out=nuevo;

 outlier alpha=0.001; run;; run;
*identify var=vivlag2;
*estimate p=2 q=(12) noint; run;
identify  var=vivlag2 crosscorr=hip_loglag stationarity=(phillips); run;
*estimate p=(1)(12) q=(9) input=(18 hip_loglag) noint; run;
estimate p=(1)(12) input=(18 $ / (2) hip_loglag) noint; 
outlier alpha=0.001; run;
identify  var=viv_loglag crosscorr=hip_loglag stationarity=(phillips); run;
quit;


/* prediccion */

data train test;
    set viviendas;
	nobs=187;
    if _n_ <= nobs - 12 then output train;
    else output test;
run;

proc print data=test; run; 


proc arima data=train; 
   identify var=vivlag2;run;
   estimate p=2 q=(12) noint; run;
   forecast out=forecast_test lead=12 id=fecha;
run;

data last_12_forecast;
    set forecast_test;
    if _N_ > 187 - 12;
	keep forecast fecha;
run;
proc print data=last_12_forecast; run;

data forecast_adjusted;
    set last_12_forecast;
    format fecha date9.;
    start_date = '01AUG2021'd;
    row_number = _N_; 
    fecha = intnx('month', start_date, mod(row_number - 1, 12), 'b');
	keep forecast fecha;
run;

proc print data=test;
run;


data combined;
    merge test(in=inTest) forecast_adjusted(in=inForecast);
    by fecha;
    if inTest;
run;

proc print data=combined; run;

symbol i=j;
proc gplot data=combined;
plot (vivlag2 forecast)*fecha / overlay legend;
run;

data a;
set combined;
a=(forecast-vivlag2)**2;
run;
proc print data=a; run;



/***********************************************************************************************************/
