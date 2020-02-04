# This script is used to create the figures (main text and supplementary) of the article
# Modelling lactation curves of mountain-pastured Braunvieh cows in Switzerland: influence of morphological and environmental factors. S. Duruz et al.

# Load required packages
require("corrplot")
require("graphics")
require("RColorBrewer")
require("HistogramTools")
require("RPostgreSQL")
require("ggplot2")
require("gridExtra")

# Connection to database
con_pos=dbConnect("PostgreSQL", dbname = "climgen_ch",host = "localhost", port = 5433,user = "postgres", password = "postgres") # connexion à la base de données

# Save old plotting parameters
old.par=par()

t=c(1:365)

# Define directory where figures will be saved
setwd('D:/ClimGen-CH/article1/figures/')

# Use function


#####################################
################ SQL ################
#####################################

#Define SQL that will be used (slightly modified each time)

#Define t1 with sql syntax. t1 is the beginning of the alping season: Mid-May
sql_t1="(case when extract(month from kalbedatum)<5 then 155-extract(month from kalbedatum)*30 
              else 275-(extract(month from kalbedatum)-8)*30 end)"

#Before t1 and after t1+115, takes only non-alped records, 
# Between t1 and t1+115 takes only alped records
# 115: predefined duration of the alping season
#field_xxx must be replaced by the name of the field to sum/avg and agregate_xxx by the name of the agregate function (sum or avg)
sql_count=paste0("coalesce(agregate_xxx(case when (dt_probe-kalbedatum)>",sql_t1," and  (dt_probe-kalbedatum)<(",sql_t1,"+115)  
                       then null 
                       when  (dt_probe-kalbedatum)>305 then null
                       when alp_probe is null then field_xxx else null end),
                   
                   agregate_xxx(case when (dt_probe-kalbedatum)<",sql_t1,"
                       then null 
                       when (dt_probe-kalbedatum)>(",sql_t1,"+115) 
                       then null 
                       when alp_probe is not null then field_xxx else null end))")

#sql Select statement with calving month, t1, num cows, milk (according to above schema) and milk properties
sql_select=paste0("select extract(month from kalbedatum) as calving_month,
",sql_t1," as t1,
dt_probe-kalbedatum as testday, 
",gsub('agregate_xxx','sum',gsub('field_xxx','1',sql_count))," as num_cow,
",gsub('agregate_xxx','avg',gsub('field_xxx','milch',sql_count))," as milk_final,
",gsub('agregate_xxx','avg',gsub('field_xxx','(milch*eiweiss)',sql_count))," as eiweiss_final,                  
",gsub('agregate_xxx','avg',gsub('field_xxx','eiweiss',sql_count))," as eiweissp_final, 
",gsub('agregate_xxx','avg',gsub('field_xxx','(milch*fett)',sql_count))," as fat_final,
",gsub('agregate_xxx','avg',gsub('field_xxx','fett',sql_count))," as fatp_final")

#sql from statement
sql_from=' from k33f '

#sql group by statement
sql_groupby=" group by calving_month, testday "

#####################################
############# Figure 1 ##############
#####################################

# Figure 1 was not done using R but with QGIS

#####################################
############## Fig S1 ###############
#####################################

### Check correlations
sql_corr="
create materialized view correlation as (
select k33f.laktation_nr, kalbemonat, min_dt_probe-besamungsdt as preg_stage, 
k3.avg_thi as avg_thi3, k3.avg_csi as avg_csi3, k30.avg_thi as avg_thi30, k30.avg_csi as avg_csi30, k3.max_thi as max_thi3, k3.max_csi as max_csi3, k30.max_thi as max_thi30, k30.max_csi as max_csi30, biogreg_c6 
max_hohe, diff_hohe, format_lange, klauensatz, avg_precip_spring
from 
(select tier_id, laktation_nr from k04f group by tier_id, laktation_nr) k33f -- on k.tier_id=k33f.tier_id and k.laktation_nr=k33f.laktation_nr
left join (select tier_id, laktation_nr, extract(month from kalbedt) as kalbemonat from k04f group by tier_id, laktation_nr, kalbemonat) k2 on k2.tier_id=k33f.tier_id and k2.laktation_nr=k33f.laktation_nr
left join (select tier_id, laktation_nr, min(dt_probe) as min_dt_probe from k33f where alp_hohe>10 group by tier_id, laktation_nr) m on m.tier_id=k33f.tier_id and m.laktation_nr=k33f.laktation_nr
LEFT JOIN ( SELECT tier_id, laktationsnummer,max(besamungsdt) AS besamungsdt FROM k10 GROUP BY tier_id, laktationsnummer) b ON b.tier_id = k33f.tier_id AND b.laktationsnummer = k33f.laktation_nr 
left join (select tier_id, laktation_nr, avg(thi) as avg_thi, max(thi) as max_thi,avg(csi) as avg_csi, max(csi) as max_csi from k33_tindex where alp_hohe>10 group by tier_id, laktation_nr) k3 ON k3.tier_id = k33f.tier_id AND k3.laktation_nr = k33f.laktation_nr 
left join (select tier_id, laktation_nr, avg(thi) as avg_thi, max(thi) as max_thi,avg(csi) as avg_csi, max(csi) as max_csi from k33_tindex30d where alp_hohe>10 group by tier_id, laktation_nr) k30 ON k30.tier_id = k33f.tier_id AND k30.laktation_nr = k33f.laktation_nr 
left join (select tier_id, laktation_nr, max(biogreg_c6) as biogreg_c6 from k33f join b01 on k33f.betriebid=b01.betriebid join biogeo g on st_contains(g.geom,b01.geom) where alp_hohe>10 group by tier_id, laktation_nr) r ON r.tier_id = k33f.tier_id AND r.laktation_nr = k33f.laktation_nr 
left join (select tier_id, laktation_nr, max(alp_hohe) as max_hohe from k33f group by laktation_nr, tier_id) h ON h.tier_id = k33f.tier_id AND h.laktation_nr = k33f.laktation_nr 
left join (select tier_id, laktation_nr, max(alp_hohe)*100-min(ST_Value(rast, 1, geom)) as diff_hohe from k33f k join b01 b on k.betriebid=b.betriebid join dem100 on ST_Intersects(rast,geom) group by laktation_nr, tier_id) dh ON dh.tier_id = k33f.tier_id AND dh.laktation_nr = k33f.laktation_nr 
left join (select tier_id, max(format_lange) as format_lange, max(fund_klauensatz) as klauensatz from k07 group by tier_id) k7 on  k7.tier_id = k33f.tier_id 
left join (select tier_id, laktation_nr, max(betriebid) as betriebid, max(extract(year from dt_probe)) as year2 from k33f group by tier_id, laktation_nr) k33 on k33.tier_id=k33f.tier_id and k33.laktation_nr=k33f.laktation_nr
left join (SELECT f.betriebid, year2, avg(rhiresm) AS avg_precip_spring FROM farm_rhiresm f join k33f k on k.betriebid=f.betriebid WHERE alp_hohe>10 and month_num >= 4 AND month_num <= 7 GROUP BY f.betriebid, year2) r2 on k33.betriebid=r2.betriebid and k33.year2=r2.year2
left join (SELECT k.betriebid, biogreg_c6 as region FROM k33f k join b01 b on k.betriebid=b.betriebid join biogeo bg on st_contains(bg.geom, b.geom) where biogreg_c6 in (3,5)) bg on k33.betriebid=bg.betriebid)"

dbSendQuery(con_pos, sql_corr)

corr_data=dbGetQuery(con_pos, 'select * from correlation')
test=cor(corr_data)
corrplot(cor(corr_data, use='pairwise.complete.obs'), method="color")


#####################################
############# Figure 2 ##############
#####################################

#Data
sql_lact_curve=gsub('\n',' ', paste0(sql_select, sql_from, sql_groupby), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve[lact_curve[,'testday']<lact_curve[,'t1'],'num_cow']<-lact_curve[lact_curve[,'testday']<lact_curve[,'t1'],'num_cow']*100

#Define plot layout and colors and where the figure will be saved
png('Fig2.png', width=16, height=8, pointsize=13, units='cm', res=300, family='serif')
layout( matrix(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,2), nrow=1) )
colors = brewer.pal(3, 'Blues')
j=2 # color selection. 1 is too pale


#Normalize weights between 0.5 and 1 (arbitrarly chosen values)
w_plot=0.5+(lact_curve[,'num_cow']-min(lact_curve[,'num_cow'], na.rm=TRUE))*0.5/(max(lact_curve[,'num_cow'], na.rm=TRUE)-min(lact_curve[,'num_cow'], na.rm=TRUE))

### a) September
#Plot margin and parameter
par(mar=c(5.1, 4.1, 4.1, 0), xpd=FALSE)
plot(NA, xlim=c(0,300), ylim=c(0,30), xlab='Days in milk', ylab='Milk yield [kg]', bty='n')

#t1: time of alping. 245 after calving (on average), for cows calving in September
t1=245
points(lact_curve[lact_curve[,'calving_month']==9,'milk_final']~lact_curve[lact_curve[,'calving_month']==9,'testday'], col=colors[j], pch=19, cex=w_plot)
#Prediction
sql_result_sub=lact_curve[lact_curve[,'calving_month']==9,]
sql_result_sub=sql_result_sub[sql_result_sub[,'testday']<305,]
coeff=calc_coeff('Wilmink', sql_result_sub, 'milk_final', 'testday', t1_field='t1',w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=t1))
pred_t=data.frame(t_obs=c(1:365), t1_obs=rep(t1, 365))
pred_y=predict(coeff, pred_t) 
lines(t(pred_t['t_obs']), pred_y, col='Black')
#Prediction Wilmink
coeff2=calc_coeff('Wilmink', sql_result_sub[sql_result_sub[,'t1']-sql_result_sub[,'testday']>0,], 'milk_final', 'testday', t1_field=NULL,w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
pred_t=data.frame(t_obs=c(1:365))
pred_y=predict(coeff2, pred_t)
lines(t(pred_t['t_obs']), pred_y, col='Black', lty=2)
function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=1000))
title('(a) September', adj=0)
##Gscore (cited in text)
coeff3=calc_coeff('Wilmink', sql_result_sub, 'milk_final', 'testday', t1_field=NULL,w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
lmtest::lrtest(coeff,coeff3)
coeff

#a) February
t1=95
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(NA, xlim=c(0,300), ylim=c(0,30), xlab='Days in milk', ylab=' ', bty='n', yaxt='n')
points(lact_curve[lact_curve[,'calving_month']==2,'milk_final']~lact_curve[lact_curve[,'calving_month']==2,'testday'], col=colors[j], pch=19, cex=w_plot)
#Prediction
sql_result_sub=lact_curve[lact_curve[,'calving_month']==2,]
sql_result_sub=sql_result_sub[sql_result_sub[,'testday']<305,]
coeff=calc_coeff('Wilmink', sql_result_sub, 'milk_final', 'testday', t1_field='t1',w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=TRUE, k_wilmink=0.1)
function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=t1))
pred_t=data.frame(t_obs=c(1:365), t1_obs=rep(t1, 365))
pred_y=predict(coeff, pred_t) 
lines(t(pred_t['t_obs']), pred_y, col='Black')
#Prediction Wilmink
coeff2=calc_coeff('Wilmink', sql_result_sub[sql_result_sub[,'t1']-sql_result_sub[,'testday']>0,], 'milk_final', 'testday', t1_field=NULL,w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
pred_t=data.frame(t_obs=c(1:365))
pred_y=predict(coeff2, pred_t)
lines(t(pred_t['t_obs']), pred_y, col='Black', lty=2)
function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=1000))
title('(b) February', adj=0)
##Gscore and d-param (cited in text)
coeff3=calc_coeff('Wilmink', sql_result_sub, 'milk_final', 'testday', t1_field=NULL,w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
lmtest::lrtest(coeff,coeff3)
coeff

#Legend
legend(x='bottomleft', legend=c('Observation','New model','Wilmink'), col=c(colors[j],'black','black'), pch=c(19,95,133), inset=0.02,bty='n')

# Release the file Fig2.pdf
dev.off()

#####################################
############# Figure 3 ##############
#####################################

#Data
sql_lact_curve=gsub('\n',' ', paste0(sql_select, sql_from, " where laktation_nr=1",sql_groupby), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
res=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', month=c(9,10,11,12,1,2),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE, endCurve=TRUE)

# Open file to draw the plot and define the layout and margin
png('Fig3.png', width=16, height=8, pointsize=13, units='cm', res=300, family='serif')
layout( matrix(c(1,1,1,1,1,1,1,1,2,2,3,4,4,4,4,4,4,4,4,4), nrow=1) )
par(mar=c(5.1,4.1,4.1,0), new=FALSE)

# Subfigure (a)
# Barplot total milk yield
barplot(abs(res$milk_total), ylab='Milk yield [kg]', xlab='calving month', names.arg=c('Sep','Oct','Nov','Dec','Jan','Feb'), las=3, col='grey')
# Barplot milk during high alpine grazing on top
barplot(abs(res$milk_alp), col='grey32',axisnames=FALSE, add=TRUE)
title(main='(a) Milk production',adj=0)
# Legend of both parplots
par(mar=c(0,0,0,0), xpd=TRUE)
plot.new()
legend(x=-0.1, y=0.5, legend=c('alp','main'), fill=c('grey','grey32'), bty='n', inset=0.5)

# Subfigure (b)
plot.new()
par(mar=c(5.1,4.1,4.1,2.1))
# d-parameter
barplot(abs(res$alp_coeff), xlab='calving month', ylab='d-paramater',names.arg=c('Sep','Oct','Nov','Dec','Jan','Feb'), las=3, col='grey')
title(main='(b) d-parameter',adj=0)

# Release file Fig3.pdf
dev.off()
# Put back old layout
layout( matrix(c(1), ncol=1) )

#####################################
############ Figure S2 ##############
#####################################

# A faire
num=dbGetQuery(con_pos, "select month, count(*) as num_lakt from (select tier_id, laktation_nr, extract(month from kalbedatum) as month from k33f group by tier_id, laktation_nr, month) a where (month>=9 or month<=2) and laktation_nr=1 group by month order by month")
num=rbind(num[3:6,], num[1:2,])
#barplot(num[,'num_lakt'], ylab='#', xlab='month', names.arg=c('Sep','Oct','Nov','Dec','Jan','Feb'), las=3,main='c) # animals', col='grey', yaxt="n")
#axis(2, at=c(0,20000,40000), labels=c(0,20000,40000))

#####################################
######## Fig 4 & 5 (calc) ###########
#####################################

# Calculate parameters, p-values, milk production for all factors 

### lactation number
#histo
sql_histo="select laktation_nr, count(*) as num from (select tier_id, laktation_nr from k04f group by tier_id, laktation_nr) k group by laktation_nr order by laktation_nr"
histo=dbGetQuery(con_pos, sql_histo)
barplot(histo[,'num'], names.arg=histo[,'laktation_nr'])
#SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when laktation_nr=1 then '1' when laktation_nr=2 then '2' else '>3' end as lact_number ", sql_from, sql_groupby, ", lact_number"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
# Model and plot
result_ln=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='lact_number', month=c(9,10,11,12,1,2),diff_value=c('>3','1'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', fullInteraction=TRUE, interactionMonth = TRUE, diffLegend='Lact #', endCurve=TRUE)


### Pregnancy stage
# histo and tertile 
sql_histo="select (dt_probe-besamungsdt) as stage_pregnancy from (select tier_id, laktation_nr, min(dt_probe) as dt_probe from k33f where alp_hohe>10 group by tier_id, laktation_nr) k33f LEFT JOIN ( SELECT tier_id, laktationsnummer,max(besamungsdt) AS besamungsdt FROM k10 GROUP BY tier_id, laktationsnummer) b ON b.tier_id = k33f.tier_id AND b.laktationsnummer = k33f.laktation_nr"
histo=dbGetQuery(con_pos, sql_histo)
hist(histo)
quantile(histo, probs=0.33, na.rm=TRUE) #73  
quantile(histo, probs=0.66, na.rm=TRUE) #153 
#SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when (dt_alp-besamungsdt)<",quantile(histo, probs=0.33, na.rm=TRUE)," then 'early' when (dt_alp-besamungsdt)>",quantile(histo, probs=0.33, na.rm=TRUE)," then 'late' end as stage_pregnancy from k33f left join (select tier_id, laktation_nr, min(dt_probe) as dt_alp from k33f where alp_hohe>10 group by tier_id, laktation_nr) sub on k33f.tier_id=sub.tier_id and k33f.laktation_nr=sub.laktation_nr LEFT JOIN ( SELECT tier_id, laktationsnummer,max(besamungsdt) AS besamungsdt FROM k10 GROUP BY tier_id, laktationsnummer) b ON b.tier_id = k33f.tier_id AND b.laktationsnummer = k33f.laktation_nr ", sql_groupby, ", stage_pregnancy"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
# Model and plot
result_ps=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='stage_pregnancy', month=c(9,10,11,12, 1,2),diff_value=c('early','late'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', fullInteraction=TRUE, interactionMonth = TRUE, endCurve=TRUE)


### THI 3 days
#histo and tertile 
sql_histo="select thi from k33_tindex k1  where alp_hohe>10 and thi is not null"
histo=dbGetQuery(con_pos, sql_histo)
hist(histo)
quantile(histo, probs=0.33, na.rm=TRUE) #59.4
quantile(histo, probs=0.66, na.rm=TRUE) #65.43
#SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when alp_hohe is null or alp_hohe=0 then 'hot' when thi>=",quantile(histo, probs=0.66, na.rm=TRUE)," then 'hot' when thi>",quantile(histo, probs=0.33, na.rm=TRUE)," and thi<",quantile(histo, probs=0.66, na.rm=TRUE)," then 'not hot' end as thi_class ", sql_from, " left join (select k1.tier_id, k1.laktation_nr, k1.probe_nr, thi from k33_tindex k1  where alp_hohe>10) kt on k33f.tier_id=kt.tier_id and k33f.laktation_nr=kt.laktation_nr and kt.probe_nr=k33f.probe_nr ", sql_groupby, ", thi_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_thi3=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='thi_class', month=c(9, 10, 11, 12, 1, 2),diff_value=c('not hot','hot'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)

### THI 30 days
#histo and tertile 
sql_histo="select thi from k33_tindex30d k1  where alp_hohe>10 and thi is not null"
histo=dbGetQuery(con_pos, sql_histo)
hist(histo)
quantile(histo, probs=0.33, na.rm=TRUE) #59.6
quantile(histo, probs=0.66, na.rm=TRUE) #63.1
#SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when alp_hohe is null or alp_hohe=0 then 'hot' when thi>=",quantile(histo, probs=0.66, na.rm=TRUE)," then 'hot' when thi>",quantile(histo, probs=0.33, na.rm=TRUE)," and thi<",quantile(histo, probs=0.66, na.rm=TRUE)," then 'not hot' end as thi_class ", sql_from, " left join (select k1.tier_id, k1.laktation_nr, k1.probe_nr, thi from k33_tindex30d k1  where alp_hohe>10) kt on k33f.tier_id=kt.tier_id and k33f.laktation_nr=kt.laktation_nr and kt.probe_nr=k33f.probe_nr ", sql_groupby, ", thi_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_thi30=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='thi_class', month=c(9, 10, 11, 12, 1, 2),diff_value=c('not hot','hot'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)


# CSI, 3 days
#histo and tertile
sql_histo="select csi from k33_tindex k1  where alp_hohe>10 and csi is not null "
histo=dbGetQuery(con_pos, sql_histo)
hist(histo)
quantile(histo, probs=0.33, na.rm=TRUE) #960.1
quantile(histo, probs=0.66, na.rm=TRUE) #1045.9
#SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when alp_hohe is null or alp_hohe=0 then 'cold' when csi>=",quantile(histo, probs=0.66, na.rm=TRUE)," then 'cold' when csi>",quantile(histo, probs=0.33, na.rm=TRUE)," and csi<",quantile(histo, probs=0.66, na.rm=TRUE)," then 'not cold' end as csi_class ", sql_from, " left join (select k1.tier_id, k1.laktation_nr, k1.probe_nr, csi from k33_tindex k1  where alp_hohe>10) kt on k33f.tier_id=kt.tier_id and k33f.laktation_nr=kt.laktation_nr and kt.probe_nr=k33f.probe_nr ", sql_groupby, ", csi_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_csi3=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='csi_class', month=c(9, 10, 11, 12, 1, 2),diff_value=c('cold','not cold'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth=TRUE)

### CSI, 30 days
# histo and tertile
sql_histo="select csi from k33_tindex30d k1  where alp_hohe>10 and csi is not null "
histo=dbGetQuery(con_pos, sql_histo)
hist(histo)
quantile(histo, probs=0.33, na.rm=TRUE) #997
quantile(histo, probs=0.66, na.rm=TRUE) #1042.9
#SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when alp_hohe is null or alp_hohe=0 then 'cold' when csi>=",quantile(histo, probs=0.66, na.rm=TRUE)," then 'cold' when csi>",quantile(histo, probs=0.33, na.rm=TRUE)," and csi<",quantile(histo, probs=0.66, na.rm=TRUE)," then 'not cold' end as csi_class ", sql_from, " left join (select k1.tier_id, k1.laktation_nr, k1.probe_nr, csi from k33_tindex30d k1  where alp_hohe>10) kt on k33f.tier_id=kt.tier_id and k33f.laktation_nr=kt.laktation_nr and kt.probe_nr=k33f.probe_nr ", sql_groupby, ", csi_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_csi30=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='csi_class', month=c(9, 10, 11, 12, 1, 2),diff_value=c('cold','not cold'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth=TRUE)


### Precipitation in spring
# histo and tertile
sql_histo="select avg_precip_spring from (SELECT f.betriebid, year2, avg(rhiresm) AS avg_precip_spring FROM farm_rhiresm f join k33f k on k.betriebid=f.betriebid WHERE alp_hohe>10 and month_num >= 4 AND month_num <= 7 GROUP BY f.betriebid, year2) d where avg_precip_spring is not null"
histo=dbGetQuery(con_pos, sql_histo)
hist(histo)
quantile(histo, probs=0.33, na.rm=TRUE) #120.6
quantile(histo, probs=0.66, na.rm=TRUE) #155.3
# SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when avg_precip_spring>",quantile(histo, probs=0.66, na.rm=TRUE)," then 'wet' when avg_precip_spring<",quantile(histo, probs=0.33, na.rm=TRUE)," then 'dry' end as precip_class ", sql_from, " left join (SELECT f.betriebid, year2, avg(rhiresm) AS avg_precip_spring FROM farm_rhiresm f join k33f k on k.betriebid=f.betriebid WHERE alp_hohe>10 and month_num >= 4 AND month_num <= 7 GROUP BY f.betriebid, year2) rain_spring ON k33f.betriebid = rain_spring.betriebid AND date_part('year', k33f.dt_probe) = rain_spring.year2 ", sql_groupby, ", precip_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_sprec=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='precip_class', month=c(9,10,11,12,1,2),diff_value=c('wet','dry'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)


### Biogeoregion
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when biogreg_c6=3 then 'North' when biogreg_c6=5 then 'East' else null end as biogeoreg ", sql_from, " join (select tier_id, laktation_nr, max(biogreg_c6) as biogreg_c6 from k33f join b01 on k33f.betriebid=b01.betriebid join biogeo g on st_contains(g.geom,b01.geom) where alp_hohe>10 group by tier_id, laktation_nr) sub on k33f.tier_id=sub.tier_id and k33f.laktation_nr=sub.laktation_nr WHERE biogreg_c6=3 or biogreg_c6=5", sql_groupby, ", biogeoreg"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_bgr=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='biogeoreg', month=c(9,10,11,12,1,2),diff_value=c('North','East'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)


### Altitude
#Histo and tertile
sql_histo="select tier_id, laktation_nr, max(alp_hohe) as max_hohe from k33f group by laktation_nr, tier_id"
histo=dbGetQuery(con_pos, sql_histo)
hist(histo[,'max_hohe'])
quantile(histo[,'max_hohe'], probs=0.33, na.rm=TRUE) #16, in *100m
quantile(histo[,'max_hohe'], probs=0.66, na.rm=TRUE) #19
#SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when max_hohe>=",quantile(histo[,'max_hohe'], probs=0.66, na.rm=TRUE)," then 'high' when max_hohe<=",quantile(histo[,'max_hohe'], probs=0.33, na.rm=TRUE)," then 'low' end as max_hohe_class ", sql_from, " join (select tier_id, laktation_nr, max(alp_hohe) as max_hohe from k33f group by laktation_nr, tier_id) kt on k33f.tier_id=kt.tier_id and k33f.laktation_nr=kt.laktation_nr ", sql_groupby, ", max_hohe_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_alt=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='max_hohe_class', month=c(9,10,11,12,1,2),diff_value=c('low','high'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth=TRUE)

# Altitude difference
#Histo and tertile
sql_histo="select diff_hohe from (select tier_id, laktation_nr, max(alp_hohe)*100-min(ST_Value(rast, 1, geom)) as diff_hohe from k33f k join b01 b on k.betriebid=b.betriebid join dem100 on ST_Intersects(rast,geom) group by laktation_nr, tier_id) d where diff_hohe is not null"
histo=dbGetQuery(con_pos, sql_histo)
hist(histo)
quantile(histo, probs=0.33, na.rm=TRUE) #641, in m
quantile(histo, probs=0.66, na.rm=TRUE) #1021
#SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when diff_hohe>=",quantile(histo, probs=0.66, na.rm=TRUE)," then 'high dz' when diff_hohe<=",quantile(histo, probs=0.33, na.rm=TRUE)," then 'low dz' end as diff_hohe_class ", sql_from, " join (select tier_id, laktation_nr, max(alp_hohe)*100-min(ST_Value(rast, 1, geom)) as diff_hohe from k33f k join b01 b on k.betriebid=b.betriebid join dem100 on ST_Intersects(rast,geom) group by laktation_nr, tier_id) kt on k33f.tier_id=kt.tier_id and k33f.laktation_nr=kt.laktation_nr ", sql_groupby, ", diff_hohe_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_altd=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='diff_hohe_class', month=c(9,10,11,12,1,2),diff_value=c('low dz','high dz'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)


### Aspect (Orientation) 100m
# Histo
sql_histo="select tier_id, laktation_nr, max(ST_Value(rast, 1, geom)) as aspect from k33f left join b01 on k33f.betriebid=b01.betriebid join aspect_100m on ST_Intersects(rast,geom) where alp_hohe>10 group by tier_id, laktation_nr"
histo=suppressWarnings(dbGetQuery(con_pos, sql_histo, quietly=TRUE))
hist(histo[,'aspect'])
# SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when aspect>300 or aspect <60 then 'North' when aspect>120 and aspect <240 then 'South' else null end as aspect_class ", sql_from, " join (select tier_id, laktation_nr, max(ST_Value(rast, 1, geom)) as aspect from k33f left join b01 on k33f.betriebid=b01.betriebid join aspect_100m on ST_Intersects(rast,geom) where alp_hohe>10 group by tier_id, laktation_nr) kt on k33f.tier_id=kt.tier_id and k33f.laktation_nr=kt.laktation_nr ", sql_groupby, ", aspect_class"), perl=TRUE)
lact_curve=suppressWarnings(dbGetQuery(con_pos, sql_lact_curve))
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_asp100=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='aspect_class', month=c(9,10,11,12,1,2),diff_value=c('North','South'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)

### Aspect (Orientation) 1km
# SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when aspect>300 or aspect <60 then 'North' when aspect>120 and aspect <240 then 'South' else 'null' end as aspect_class ", sql_from, " join (select tier_id, laktation_nr, max(ST_Value(rast, 1, geom)) as aspect from k33f left join b01 on k33f.betriebid=b01.betriebid join aspect_1km on ST_Intersects(rast,geom) where alp_hohe>10 group by tier_id, laktation_nr) kt on k33f.tier_id=kt.tier_id and k33f.laktation_nr=kt.laktation_nr ", sql_groupby, ", aspect_class"), perl=TRUE)
lact_curve=suppressWarnings(dbGetQuery(con_pos, sql_lact_curve))
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_asp1000=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='aspect_class', month=c(9,10,11,12,1,2),diff_value=c('North','South'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)


### Length of cow
#histo and tertile
sql_histo="select tier_id, max(format_lange) as format_lange from k07 group by tier_id"
histo=dbGetQuery(con_pos, sql_histo)
hist(histo[,'format_lange'])
quantile(histo[,'format_lange'], probs=0.33, na.rm=TRUE) #4
quantile(histo[,'format_lange'], probs=0.66, na.rm=TRUE) #6
# SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when format_lange>",quantile(histo[,'format_lange'], probs=0.66, na.rm=TRUE) ," then 'long' when format_lange<",quantile(histo[,'format_lange'], probs=0.33, na.rm=TRUE)," then 'short' end as length_class ", sql_from, " join (select tier_id, max(format_lange) as format_lange from k07 group by tier_id) k07 on k33f.tier_id=k07.tier_id ", sql_groupby, ", length_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_l=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='length_class', month=c(9,10,11,12,1,2),diff_value=c('long','short'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', fullInteraction = TRUE, interactionMonth=TRUE,endCurve = TRUE)

# Hooves health
#histo and tertile
sql_histo="select tier_id, max(fund_klauensatz) as fund_klauensatz from k07 group by tier_id"
histo=dbGetQuery(con_pos, sql_histo)
hist(histo[,'fund_klauensatz'])
quantile(histo[,'fund_klauensatz'], probs=0.33, na.rm=TRUE) #5
quantile(histo[,'fund_klauensatz'], probs=0.66, na.rm=TRUE) #6
#SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when fund_klauensatz>",quantile(histo[,'fund_klauensatz'], probs=0.66, na.rm=TRUE)," then 'good' when fund_klauensatz<",quantile(histo[,'fund_klauensatz'], probs=0.33, na.rm=TRUE)," then 'bad' end as hooves_class ", sql_from, " join k07 on k33f.tier_id=k07.tier_id and k33f.laktation_nr=k07.laktation_nr", sql_groupby, ", hooves_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_hh=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='hooves_class', month=c(9,10,11,12,1,2),diff_value=c('good','bad'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', fullInteraction = TRUE, interactionMonth = TRUE, endCurve=TRUE)


### Persistency
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when persistency>80 then 1 else 0 end as persistency_class ", sql_from, " join (select tier_id, max(persistenz) as persistency from k04 group by tier_id) k04 on k33f.tier_id=k04.tier_id ", sql_groupby, ", persistency_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', diff_field='persistency_class', month=c(10),diff_value=c(0,1),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', fullInteraction = TRUE)

### Create matrix combining the results of teh different factors
# Milk tot to calculate %
sql_lact_curve=gsub('\n',' ', paste0(sql_select, sql_from,sql_groupby), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
res_tot=plot_f(lact_curve, 'milk_final', 'testday', 'calving_month', month=c(9,10,11,12,1,2),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE, endCurve = TRUE)

# p-val matrix
# Name of variables storing results, name of factors, name of reference group
list_result=c('result_ln','result_ps','result_thi3','result_thi30','result_csi3','result_csi30', 'result_sprec','result_bgr','result_alt','result_altd','result_asp100','result_asp1000','result_l','result_hh')
list_name=c('Age of cow','Pregnacy stage','(a) THI (3d.)','(b) THI (30d.)','CSI(3d.)','CSI(30d.)','(c) Prec sp','(d) B-region','(e) Alt','(f) Alt diff','aspect (100m)','aspect (1km)','Length','Hooves health')
refgroup=c('3rd lact','Early preg','Medium','Medium','Cold','Cold','Dry','North Alp','Low','Low','North','North','Long','Healthy')
nummonth=6 #Number of calving months considered (September-February)
matrix_pval=c()
matrix_pval2=c()
for(l in 1:length(list_result)){ # Loop on factors
  value=get(list_result[l])
  pval=value$pval*length(list_result[l])*nummonth #Bonferroni correction
  matrix_pval=rbind(matrix_pval,c(list_name[l], paste0(sprintf('%.2G',min(pval)),' - ',sprintf('%.2G',max(pval))))) # Formatted p-value range for Table S1
  matrix_pval2=rbind(matrix_pval2,cbind(t(t(rep(list_name[l],6))),pval))
}
# Define colors for plot: light grey if >0.05, dark grey if significant 
matrix_pval_col=matrix_pval2
matrix_pval_col[as.numeric(as.character(matrix_pval_col[,'pval']))<=0.05,'pval']<-'grey21' 
matrix_pval_col[as.numeric(as.character(matrix_pval_col[,'pval']))>0.05,'pval']<-'grey'

#####################################
############## Fig 4 ################
#####################################

crit=c(1,2,13,14) #Criteria to consider (lact number, preg stage, length, hooves)
dma=c()
dmb=c()
deltaf=c()
colorf=c()
for(i in 1:length(crit)){
  name=list_name[crit[i]]
  res=get(list_result[crit[i]])
  # Calculate delta milk during alp [%]
  dma=rbind(dma,cbind(type=rep(name,nummonth), orderx=rep(i, nummonth), order1=1:nummonth,month=c(9,10,11,12,1,2), value=as.double((t(t(res$milk_alp[1,]))-t(t(res$milk_alp[2,])))/t(res_tot$milk_alp)*100 )))
  # Calculate total delta milk [%]
  dmb=rbind(dmb,cbind(type=rep(name,nummonth), orderx=rep(i, nummonth), order1=1:nummonth,month=c(9,10,11,12,1,2), value=as.double((t(t(res$milk_total[1,]))-t(t(res$milk_total[2,])))/t(res_tot$milk_total)*100 ))) 
  # Delta d
  deltad=rbind(deltad,cbind(type=rep(name,nummonth), orderx=rep(i, nummonth), order1=1:nummonth,month=c(9,10,11,12,1,2), value=as.double(res$alpdiff_coeff)))
  # Color according to significance
  colorf=rbind(colorf,matrix_pval_col[((crit[i]-1)*6+1):((crit[i]-1)*6+6),])
}
# Convert to dataframe
delta_milk_alp=as.data.frame(dma)
delta_milk_total=as.data.frame(dmb)
deltad=as.data.frame(deltad)
colorf=as.data.frame(colorf)

# Subtitle displaying the reference group
subtitle=paste('RG:', c('3rd lact','early preg','long','healthy') )
graph_number=c('(a)','(c)','(e)','(g)') # Displaying delta milk [%]
graph_number2=c('(b)','(d)','(f)','(h)') # Displaying delta d
png('Fig4.png', width=16, height=12, pointsize=13, units='cm', res=300, family='serif')
layout( matrix( c(1,2,1,2,1,2,1,2,1,2,1,2,3,4,3,4,3,4,3,4,5,6,5,6,5,6,5,6,7,8,7,8,7,8,7,8,9,10,9,10,9,10), nrow=2) )
for(i in seq(1,19,6)){ # Loop on factors, each factor has 6 lines (6 months)
  # Define margin
  if(i==1){
    par(mar = c(1, 5, 5, 0), xpd=NA)
  } else {
    par(mar = c(1, 1, 5, 0), xpd=NA)
  }
  # difference total milk production
  if(i==1){ #First plot also shwo y-axis
    barplot(as.numeric(as.character(delta_milk_total[i:(i+5),'value'])), ylim=c(0,25), ylab=expression(paste('',Delta,'milk [%]')))
    title(paste0(delta_milk_total[i,'type']), adj=0)
    text(0,28,parse(text=paste0('""','>=','"3rd lact"')),cex=1.2, adj=0)
    
  } else { # Do not show y-axis
    barplot(as.numeric(as.character(delta_milk_total[i:(i+5),'value'])), ylim=c(0,25), yaxt='n')
    title(paste0(delta_milk_total[i,'type']), adj=0)
    text(0,28,subtitle[ceiling(i/6)],cex=1.2, adj=0)
    
  }
  text(0, 25, graph_number[ceiling(i/6)], font=2, cex=1.2,adj=0) # Subgraph number (a,b,c...)
  # Add on top difference milk production during high alpine grazing
  barplot(as.numeric(as.character(delta_milk_alp[i:(i+5),'value'])), density=rep(20,6), yaxt='n', angle=rep(45,6), col='brown',add=TRUE)

  # Define margin
  if(i==1){
    par(mar = c(6, 5, 1.5, 0), xpd=NA)
  } else {
    par(mar = c(6, 1, 1.5, 0), xpd=NA)
  }
  # Delat d barplot
  if(i==1){ #First plot also shwo y-axis
    barplot(as.numeric(as.character(deltad[i:(i+5),'value'])), ylim=c(-0.02,0.03), ylab=expression(paste('',Delta,'d-parameter')),names.arg=c('Sep','Oct','Nov','Dec','Jan','Feb'),col=as.character(colorf[i:(i+6),'pval']),las=3)
  }else { # Do not show y-axis
    barplot(as.numeric(as.character(deltad[i:(i+5),'value'])), ylim=c(-0.02,0.03),yaxt='n', names.arg=c('Sep','Oct','Nov','Dec','Jan','Feb'),col=as.character(colorf[i:(i+6),'pval']),las=3)
  }
  text(0, 0.037, graph_number2[ceiling(i/6)],cex=1.2, font=2, adj=0) # Subgraph number (a,b,c...)
  if(i==13){
    mtext('calving month',1,4) # x-axis legend
  }
  if(i==19){
    # Legend of delta milk production
    plot.new()
    legend(x='center', y='center', legend=c('total','alp'), fill=c('grey','brown'),  density=c(1000,30),angle=c(0,45), cex=1.2, bty='n')
    # Legend of delta d
    plot.new()
    legend(x='center', legend=c('>0.5','<0.5'), fill=c('grey','grey21'), cex=1.2, bty='n')
  }
  
}
# Release Fig4.png
dev.off()
# Define old layout
layout( matrix(c(1), ncol=1) )



#####################################
############## Fig 5 ################
#####################################

crit2=c(3,4,7:10) #Criteria to consider, only significant environmental factors 
dma2=c()
deltad2=c()
colorf2=c()
for(i in 1:length(crit2)){
  name=list_name[crit2[i]]
  res=get(list_result[crit2[i]])
  # Delta milk production during high alpine grazing
  dma2=rbind(dma2,cbind(type=rep(name,nummonth), orderx=rep(i, nummonth), order1=1:nummonth,month=c(9,10,11,12,1,2),refgroup=rep(refgroup[crit2[i]],6), value=as.double((t(t(res$milk_alp[1,]))-t(t(res$milk_alp[2,])))/t(t(res$milk_alp[1,]))*100  )  )) #, value=as.double(res[]))
  # Delta d (not drawn)
  deltad2=rbind(deltad2,cbind(type=rep(name,nummonth), orderx=rep(i, nummonth), order1=1:nummonth,month=c(9,10,11,12,1,2),  value=as.double(res$alpdiff_coeff)))
  # Color accoring to significance
  colorf2=rbind(colorf2,matrix_pval_col[((crit2[i]-1)*6+1):((crit2[i]-1)*6+6),])
}
# Convert to dataframe
delta_milk_alp2=as.data.frame(dma2)
deltad2=as.data.frame(deltad2)
colorf2=as.data.frame(colorf2)

par(old.par)
png('Fig5.png', width=16, height=8, pointsize=13, units='cm', res=300, family='serif')
layout( matrix( c(1,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,6,6,6,6,6,6,6), nrow=1) )
for(i in seq(1,35,6)){
  # Barplot showing the difference in milk production
  if(i==1){ # If first plot, draw y-axis
    par(mar = c(6, 5, 3, 0), xpd=NA)
    barplot(as.numeric(as.character(delta_milk_alp2[i:(i+5),'value'])), ylim=c(-0.2,11), ylab=expression(paste('',Delta,'milk alp [%]')),names.arg=c('Sep','Oct','Nov','Dec','Jan','Feb'),col=as.character(colorf2[i:(i+6),'pval']),las=3)
    title(delta_milk_alp2[i,'type'], adj=0)
    text(0,11,paste0('RG: ',delta_milk_alp2[i,'refgroup']), adj=0) # Add reference group as "subtitle"
  }else {
    par(mar = c(6, 1, 3, 0), xpd=NA)
    if(i==31){
      par(mar = c(6, 1, 3, 2.5), xpd=NA)
    }
    barplot(as.numeric(as.character(delta_milk_alp2[i:(i+5),'value'])), ylim=c(-0.2,11),yaxt='n', names.arg=c('Sep','Oct','Nov','Dec','Jan','Feb'),col=as.character(colorf2[i:(i+6),'pval']),las=3)
    title(delta_milk_alp2[i,'type'], adj=0)
    text(0,11,paste0('RG: ',delta_milk_alp2[i,'refgroup']), adj=0) # Add reference group as "subtitle"
    if(i==19){
      text(3,-3,'calving month',cex=1.2) # Y axis-legend
    }
    if(i==31){ # For last plot, draw significance legend
      legend(title='Significance',x='topright', legend=c(expression("">"0.5"),expression(""<="0.5")), fill=c('grey','grey21'), cex=1.2, inset=c(0.5,0.1), bty='n')
    }
  }
}
# Release file Fig5.png
dev.off()
layout( matrix(c(1), ncol=1) )

