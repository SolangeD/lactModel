rm(list=ls())
# This script is used to create the figures (main text and supplementary) of the article
# Modelling lactation curves of mountain-pastured Braunvieh cows in Switzerland: influence of morphological and environmental factors. S. Duruz et al.

# Load required packages
require("corrplot")
#require("graphics")
require("RColorBrewer")
#require("HistogramTools")
require("RPostgreSQL")

#require("gridExtra")

# Connection to database
con_pos=dbConnect("PostgreSQL", dbname = "climgen_ch",host = "localhost", port = 5433,user = "postgres", password = "postgres") # connexion à la base de données

# Define directory where figures will be saved
setwd('D:/ClimGen-CH/article1/figures/fat')
# Import functions
source('D:/ClimGen-CH/scripts/Rpackage/R/function_lactCurve.R')

# Save old plotting parameters
old.par=par()

t=c(1:365)



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
############# Figure S3 ##############
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
plot(NA, xlim=c(0,300), ylim=c(0,120), xlab='Days in milk', ylab='Fat yield [g]', bty='n')

#t1: time of alping. 245 after calving (on average), for cows calving in September
t1=245
points(lact_curve[lact_curve[,'calving_month']==9,'fat_final']~lact_curve[lact_curve[,'calving_month']==9,'testday'], col=colors[j], pch=19, cex=w_plot)
#Prediction
sql_result_sub=lact_curve[lact_curve[,'calving_month']==9,]
sql_result_sub=sql_result_sub[sql_result_sub[,'testday']<305,]
coeff=calc_coeff('Wilmink', sql_result_sub, 'fat_final', 'testday', t1_field='t1',w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=t1))
pred_t=data.frame(t_obs=c(1:365), t1_obs=rep(t1, 365))
pred_y=predict(coeff, pred_t) 
lines(t(pred_t['t_obs']), pred_y, col='Black')
#Prediction Wilmink
coeff2=calc_coeff('Wilmink', sql_result_sub[sql_result_sub[,'t1']-sql_result_sub[,'testday']>0,], 'fat_final', 'testday', t1_field=NULL,w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
pred_t=data.frame(t_obs=c(1:365))
pred_y=predict(coeff2, pred_t)
lines(t(pred_t['t_obs']), pred_y, col='Black', lty=2)
function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=1000))
title('(a) Autumn', adj=0)
##Gscore (cited in text)
coeff3=calc_coeff('Wilmink', sql_result_sub, 'fat_final', 'testday', t1_field=NULL,w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
lmtest::lrtest(coeff,coeff3)
coeff

#a) February
t1=95
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(NA, xlim=c(0,300), ylim=c(0,120), xlab='Days in milk', ylab=' ', bty='n', yaxt='n')
points(lact_curve[lact_curve[,'calving_month']==2,'fat_final']~lact_curve[lact_curve[,'calving_month']==2,'testday'], col=colors[j], pch=19, cex=w_plot)
#Prediction
sql_result_sub=lact_curve[lact_curve[,'calving_month']==2,]
sql_result_sub=sql_result_sub[sql_result_sub[,'testday']<305,]
coeff=calc_coeff('Wilmink', sql_result_sub, 'fat_final', 'testday', t1_field='t1',w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=TRUE, k_wilmink=0.1)
function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=t1))
pred_t=data.frame(t_obs=c(1:365), t1_obs=rep(t1, 365))
pred_y=predict(coeff, pred_t) 
lines(t(pred_t['t_obs']), pred_y, col='Black')
#Prediction Wilmink
coeff2=calc_coeff('Wilmink', sql_result_sub[sql_result_sub[,'t1']-sql_result_sub[,'testday']>0,], 'fat_final', 'testday', t1_field=NULL,w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
pred_t=data.frame(t_obs=c(1:365))
pred_y=predict(coeff2, pred_t)
lines(t(pred_t['t_obs']), pred_y, col='Black', lty=2)
function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=1000))
title('(b) Winter', adj=0)
##Gscore and d-param (cited in text)
coeff3=calc_coeff('Wilmink', sql_result_sub, 'fat_final', 'testday', t1_field=NULL,w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
lmtest::lrtest(coeff,coeff3)
coeff
#
#Legend
legend(x='bottomleft', legend=c('Observation','New model','Wilmink model'), col=c(colors[j],'black','black'), pch=c(19,NA, NA), lty=c(NA, 1,2), inset=0.02,bty='n')

# Release the file Fig2.pdf
dev.off()

#Fat percentage
png('Fig2b.png', width=16, height=8, pointsize=13, units='cm', res=300, family='serif')
layout( matrix(c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,2), nrow=1) )
colors = brewer.pal(3, 'Blues')
j=2 # color selection. 1 is too pale

#Normalize weights between 0.5 and 1 (arbitrarly chosen values)
w_plot=0.5+(lact_curve[,'num_cow']-min(lact_curve[,'num_cow'], na.rm=TRUE))*0.5/(max(lact_curve[,'num_cow'], na.rm=TRUE)-min(lact_curve[,'num_cow'], na.rm=TRUE))

### a) September
#Plot margin and parameter
par(mar=c(5.1, 4.1, 4.1, 0), xpd=FALSE)
plot(NA, xlim=c(0,300), ylim=c(0,5), xlab='Days in milk', ylab='Fat %', bty='n')

#t1: time of alping. 245 after calving (on average), for cows calving in September
t1=245
points(lact_curve[lact_curve[,'calving_month']==9,'fatp_final']~lact_curve[lact_curve[,'calving_month']==9,'testday'], col=colors[j], pch=19, cex=w_plot)
#Prediction
sql_result_sub=lact_curve[lact_curve[,'calving_month']==9,]
sql_result_sub=sql_result_sub[sql_result_sub[,'testday']<305,]
coeff=calc_coeff('Wilmink', sql_result_sub, 'fatp_final', 'testday', t1_field='t1',w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=t1))
pred_t=data.frame(t_obs=c(1:365), t1_obs=rep(t1, 365))
pred_y=predict(coeff, pred_t) 
lines(t(pred_t['t_obs']), pred_y, col='Black')
#Prediction Wilmink
coeff2=calc_coeff('Wilmink', sql_result_sub[sql_result_sub[,'t1']-sql_result_sub[,'testday']>0,], 'fatp_final', 'testday', t1_field=NULL,w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
pred_t=data.frame(t_obs=c(1:365))
pred_y=predict(coeff2, pred_t)
lines(t(pred_t['t_obs']), pred_y, col='Black', lty=2)
function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=1000))
title('(a) Autumn', adj=0)
##Gscore (cited in text)
coeff3=calc_coeff('Wilmink', sql_result_sub, 'fatp_final', 'testday', t1_field=NULL,w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
lmtest::lrtest(coeff,coeff3)
coeff

#a) February
t1=95
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(NA, xlim=c(0,300), ylim=c(0,5), xlab='Days in milk', ylab=' ', bty='n', yaxt='n')
points(lact_curve[lact_curve[,'calving_month']==2,'fatp_final']~lact_curve[lact_curve[,'calving_month']==2,'testday'], col=colors[j], pch=19, cex=w_plot)
#Prediction
sql_result_sub=lact_curve[lact_curve[,'calving_month']==2,]
sql_result_sub=sql_result_sub[sql_result_sub[,'testday']<305,]
coeff=calc_coeff('Wilmink', sql_result_sub, 'fatp_final', 'testday', t1_field='t1',w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=TRUE, k_wilmink=0.1)
function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=t1))
pred_t=data.frame(t_obs=c(1:365), t1_obs=rep(t1, 365))
pred_y=predict(coeff, pred_t) 
lines(t(pred_t['t_obs']), pred_y, col='Black')
#Prediction Wilmink
coeff2=calc_coeff('Wilmink', sql_result_sub[sql_result_sub[,'t1']-sql_result_sub[,'testday']>0,], 'fatp_final', 'testday', t1_field=NULL,w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
pred_t=data.frame(t_obs=c(1:365))
pred_y=predict(coeff2, pred_t)
lines(t(pred_t['t_obs']), pred_y, col='Black', lty=2)
function_predict=function(t) predict(coeff, data.frame(t_obs=t, t1_obs=1000))
title('(b) Winter', adj=0)
##Gscore and d-param (cited in text)
coeff3=calc_coeff('Wilmink', sql_result_sub, 'fatp_final', 'testday', t1_field=NULL,w_field='num_cow', fullInteraction=FALSE, diff_value=NULL, endCurve=FALSE, k_wilmink=0.1)
lmtest::lrtest(coeff,coeff3)
coeff
#
#Legend
legend(x='bottomleft', legend=c('Observation','New model','Wilmink model'), col=c(colors[j],'black','black'), pch=c(19,NA, NA), lty=c(NA, 1,2), inset=0.02,bty='n')

# Release the file Fig2.pdf
dev.off()



#####################################
########## Fig S5  (calc) ###########
#####################################

# Calculate parameters, p-values, milk production for all factors 

### Breed
#histo
sql_histo="select tier_rasse, count(*) as num from (select tier_id, tier_rasse from k04f group by tier_id, tier_rasse) k group by tier_rasse order by tier_rasse"
histo=dbGetQuery(con_pos, sql_histo)
barplot(histo[,'num'], names.arg=histo[,'tier_rasse'])
#SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when tier_rasse in ('OB','BV') then tier_rasse else null end as breed ", sql_from, sql_groupby, ", breed"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
# Model and plot
result_br=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='breed', month=c(9,10,11,12,1,2),diff_value=c('OB','BV'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', fullInteraction=TRUE, interactionMonth = TRUE, diffLegend='Breed', endCurve=TRUE)


### lactation number
#histo
sql_histo="select laktation_nr, count(*) as num from (select tier_id, laktation_nr from k04f group by tier_id, laktation_nr) k group by laktation_nr order by laktation_nr"
histo=dbGetQuery(con_pos, sql_histo)
barplot(histo[,'num'], names.arg=histo[,'laktation_nr'])
#SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when laktation_nr=1 then '1' when laktation_nr=2 then '2' else '>3' end as lact_number ", sql_from, sql_groupby, ", lact_number"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
# Model and plot
result_ln=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='lact_number', month=c(9,10,11,12,1,2),diff_value=c('>3','1'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', fullInteraction=TRUE, interactionMonth = TRUE, diffLegend='Lact #', endCurve=TRUE)


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
result_ps=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='stage_pregnancy', month=c(9,10,11,12, 1,2),diff_value=c('early','late'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', fullInteraction=TRUE, interactionMonth = TRUE, endCurve=TRUE)


### THI 3 days
#histo and tertile 
sql_histo="select thi from k33_tindex k1  where alp_hohe>10 and thi is not null"
histo=dbGetQuery(con_pos, sql_histo)
hist(histo[,'thi'])
quantile(histo, probs=0.33, na.rm=TRUE) #59.4
quantile(histo, probs=0.66, na.rm=TRUE) #65.43
#SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when alp_hohe is null or alp_hohe=0 then 'hot' when thi>=",quantile(histo, probs=0.66, na.rm=TRUE)," then 'hot' when thi>",quantile(histo, probs=0.33, na.rm=TRUE)," and thi<",quantile(histo, probs=0.66, na.rm=TRUE)," then 'not hot' end as thi_class ", sql_from, " left join (select k1.tier_id, k1.laktation_nr, k1.probe_nr, thi from k33_tindex k1  where alp_hohe>10) kt on k33f.tier_id=kt.tier_id and k33f.laktation_nr=kt.laktation_nr and kt.probe_nr=k33f.probe_nr ", sql_groupby, ", thi_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_thi3=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='thi_class', month=c(9, 10, 11, 12, 1, 2),diff_value=c('not hot','hot'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)

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
result_thi30=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='thi_class', month=c(9, 10, 11, 12, 1, 2),diff_value=c('not hot','hot'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)




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
result_csi3=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='csi_class', month=c(9, 10, 11, 12, 1, 2),diff_value=c('cold','not cold'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth=TRUE)

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
result_csi30=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='csi_class', month=c(9, 10, 11, 12, 1, 2),diff_value=c('cold','not cold'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth=TRUE)


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
result_sprec=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='precip_class', month=c(9,10,11,12,1,2),diff_value=c('wet','dry'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)


### Biogeoregion
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when biogreg_c6=3 then 'North' when biogreg_c6=5 then 'East' else null end as biogeoreg ", sql_from, " join (select tier_id, laktation_nr, max(biogreg_c6) as biogreg_c6 from k33f join b01 on k33f.betriebid=b01.betriebid join biogeo g on st_contains(g.geom,b01.geom) where alp_hohe>10 group by tier_id, laktation_nr) sub on k33f.tier_id=sub.tier_id and k33f.laktation_nr=sub.laktation_nr WHERE biogreg_c6=3 or biogreg_c6=5", sql_groupby, ", biogeoreg"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_bgr=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='biogeoreg', month=c(9,10,11,12,1,2),diff_value=c('North','East'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)


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
result_alt=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='max_hohe_class', month=c(9,10,11,12,1,2),diff_value=c('low','high'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth=TRUE)

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
result_altd=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='diff_hohe_class', month=c(9,10,11,12,1,2),diff_value=c('low dz','high dz'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)


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
result_asp100=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='aspect_class', month=c(9,10,11,12,1,2),diff_value=c('North','South'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)

### Aspect (Orientation) 1km
# SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when aspect>300 or aspect <60 then 'North' when aspect>120 and aspect <240 then 'South' else 'null' end as aspect_class ", sql_from, " join (select tier_id, laktation_nr, max(ST_Value(rast, 1, geom)) as aspect from k33f left join b01 on k33f.betriebid=b01.betriebid join aspect_1km on ST_Intersects(rast,geom) where alp_hohe>10 group by tier_id, laktation_nr) kt on k33f.tier_id=kt.tier_id and k33f.laktation_nr=kt.laktation_nr ", sql_groupby, ", aspect_class"), perl=TRUE)
lact_curve=suppressWarnings(dbGetQuery(con_pos, sql_lact_curve))
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_asp1000=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='aspect_class', month=c(9,10,11,12,1,2),diff_value=c('North','South'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE)


### Length of cow
#histo and tertile
sql_histo="select tier_id, max(format_widerristhohe) as format_widerristhohe from k07 group by tier_id"
histo=dbGetQuery(con_pos, sql_histo)
hist(histo[,'format_lange'])
quantile(histo[,'format_widerristhohe'], probs=0.33, na.rm=TRUE) #4
quantile(histo[,'format_widerristhohe'], probs=0.66, na.rm=TRUE) #6
# SQL
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when format_widerristhohe>",quantile(histo[,'format_widerristhohe'], probs=0.66, na.rm=TRUE) ," then 'high' when format_widerristhohe<",quantile(histo[,'format_widerristhohe'], probs=0.33, na.rm=TRUE)," then 'low' end as height_class ", sql_from, " join (select tier_id, max(format_widerristhohe) as format_widerristhohe from k07 group by tier_id) k07 on k33f.tier_id=k07.tier_id ", sql_groupby, ", height_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
result_l=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='height_class', month=c(9,10,11,12,1,2),diff_value=c('high','low'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', fullInteraction = TRUE, interactionMonth=TRUE,endCurve = TRUE)

# Foot angle
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
result_hh=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='hooves_class', month=c(9,10,11,12,1,2),diff_value=c('good','bad'),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', fullInteraction = TRUE, interactionMonth = TRUE, endCurve=TRUE)


### Persistency
sql_lact_curve=gsub('\n',' ', paste0(sql_select, ", case when persistency>80 then 1 else 0 end as persistency_class ", sql_from, " join (select tier_id, max(persistenz) as persistency from k04 group by tier_id) k04 on k33f.tier_id=k04.tier_id ", sql_groupby, ", persistency_class"), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
lact_curve=lact_curve[lact_curve[,'testday']-lact_curve[,'t1']<115,] # Only until end of high alpine grazing
# Model and plot
plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', diff_field='persistency_class', month=c(10),diff_value=c(0,1),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', fullInteraction = TRUE)

### Create matrix combining the results of teh different factors
# Milk tot to calculate %
sql_lact_curve=gsub('\n',' ', paste0(sql_select, sql_from,sql_groupby), perl=TRUE)
lact_curve=dbGetQuery(con_pos, sql_lact_curve)
res_tot=plot_lc(lact_curve, 'fat_final', 'testday', 'calving_month', month=c(9,10,11,12,1,2),prediction=TRUE, predictionCol='black', t1_field='t1', w_field='num_cow', interactionMonth = TRUE, endCurve = TRUE)

# p-val matrix
# Name of variables storing results, name of factors, name of reference group
list_result=c('result_ln','result_ps','result_thi3','result_thi30','result_csi3','result_csi30', 'result_sprec','result_bgr','result_alt','result_altd','result_asp100','result_asp1000','result_l','result_hh')
list_name=c('Lact #','Preg','THI-3d','THI-30d','CSI-3d','CSI-30d','Prec sp','B-region','Alt','Alt diff','aspect (100m)','aspect (1km)','Height','Ft ang')
refgroup=c('3rd lact','Early preg','Medium','Medium','Cold','Cold','Dry','North Alp','Low','Low','North','North','High','Best')
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
############# Fig S5 ################
#####################################

crit=c(3,4,7:10,1,2,13,14) #Criteria to consider (thi3, thi30,prec, bioreg,alt, alt diff, lact number, preg stage, length, hooves)
dma=c()
dmb=c()
deltad=c()
colorf=c()
for(i in 1:length(crit)){
  name=list_name[crit[i]]
  res_i=get(list_result[crit[i]])
  # Calculate delta milk during alp [%]
  dma=rbind(dma,cbind(type=rep(name,nummonth), orderx=rep(i, nummonth), order1=1:nummonth,month=c(9,10,11,12,1,2), value=as.double((t(t(res_i$milk_alp[1,]))-t(t(res_i$milk_alp[2,])))/t(t(res_i$milk_alp[1,]))*100 )))
  # Calculate total delta milk [%]
  dmb=rbind(dmb,cbind(type=rep(name,nummonth), orderx=rep(i, nummonth), order1=1:nummonth,month=c(9,10,11,12,1,2), value=as.double((t(t(res_i$milk_total[1,]))-t(t(res_i$milk_total[2,])))/t(t(res_i$milk_total[1,]))*100 ))) 
  # Delta d
  deltad=rbind(deltad,cbind(type=rep(name,nummonth), orderx=rep(i, nummonth), order1=1:nummonth,month=c(9,10,11,12,1,2), value=as.double(res_i$alpdiff_coeff)))
  # Color according to significance
  colorf=rbind(colorf,matrix_pval_col[((crit[i]-1)*6+1):((crit[i]-1)*6+6),])
}
# Convert to dataframe
dma_sub=subset(dma, dma[,'month'] %in% c('9','2'))
dmb_sub=subset(dmb, dmb[,'month'] %in% c('9','2'))
deltad_sub=subset(deltad, deltad[,'month'] %in% c('9','2'))
colorf_sub=colorf[deltad[,'month'] %in% c('9','2'),]
delta_milk_alp=as.data.frame(dma_sub)
delta_milk_total=as.data.frame(dmb_sub)
deltad=as.data.frame(deltad_sub)
colorf=as.data.frame(colorf_sub)


# Subtitle displaying the reference group
subtitle1=c('mild','mild','wet','North','low','small','3rd','early','high','steep')
subtitle2=c('hot','hot','dry','East','high','big','1st','late','low','gentle')

graph_number=c('(a)','(c)','(e)','(g)','(i)','(k)','(m)','(p)','(s)','(v)') # Displaying delta milk alp [%]
graph_number2=c(rep('',6),'(n)','(q)','(t)','(w)') # Displaying delta milk [%]
graph_number3=c('(b)','(d)','(f)','(h)','(j)','(l)','(o)','(r)','(u)','(x)') # Displaying delta d
png('Fig4.png', width=16, height=12, pointsize=13, units='cm', res=300, family='serif')
layout( matrix( c(1,1,1,1,1,4,4,4,7,7,7,10,10,10,13,13,13,16,16,16,19,19,19,22,22,22,25,25,25,28,28,28,
                  1,1,1,1,1,4,4,4,7,7,7,10,10,10,13,13,13,16,16,16,19,19,19,22,22,22,25,25,25,28,28,28,
                  1,1,1,1,1,4,4,4,7,7,7,10,10,10,13,13,13,16,16,16,19,19,19,22,22,22,25,25,25,28,28,28,
                  1,1,1,1,1,4,4,4,7,7,7,10,10,10,13,13,13,16,16,16,19,19,19,22,22,22,25,25,25,28,28,28,
                  2,2,2,2,2,5,5,5,8,8,8,11,11,11,14,14,14,17,17,17,20,20,20,23,23,23,26,26,26,29,29,29,
                  2,2,2,2,2,5,5,5,8,8,8,11,11,11,14,14,14,17,17,17,20,20,20,23,23,23,26,26,26,29,29,29,
                  3,3,3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21,21,24,24,24,27,27,27,30,30,30,
                  3,3,3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21,21,24,24,24,27,27,27,30,30,30,
                  3,3,3,3,3,6,6,6,9,9,9,12,12,12,15,15,15,18,18,18,21,21,21,24,24,24,27,27,27,30,30,30), nrow=9, byrow=TRUE) )
a=0
for(i in seq(1,20,2)){ # Loop on factors, each factor has 2 lines (2 months)
  a=a+1
  # Define margin
  if(i==1){
    par(mar = c(1, 4, 8, 0), xpd=NA)
  } else {
    par(mar = c(1, 1.5, 8, 0), xpd=NA)
  }
  # difference total milk production
  if(i==1){ #First plot also show y-axis
    barplot(as.numeric(as.character(delta_milk_alp[i:(i+1),'value'])), ylim=c(0,25), ylab=expression(paste('',Delta,'fat mountain [%]')), col='darkred',panel.first=grid(nx=0, ny=NULL,lty='solid', lwd=0.2))
    title(paste0(delta_milk_total[i,'type']), adj=0.5)
  } else { # Do not show y-axis
    barplot(as.numeric(as.character(delta_milk_alp[i:(i+1),'value'])), ylim=c(0,25), yaxt='n', col='darkred', panel.first=grid(nx=0, ny=NULL,lty='solid', lwd=0.2))
    title(paste0(delta_milk_total[i,'type']), adj=0.5)
    
  }
  if(subtitle1[a]=='3rd'){
    text(1.5,38,parse(text=paste0('""','>=','"3rd"')),cex=1.15, adj=0.5, col='darkred')
  } else {
    text(1.5,38,subtitle1[a],cex=1.15, adj=0.5, col='darkred')
  }
  text(1.5,35,'vs',cex=1.15, adj=0.5)
  text(1.5,32,subtitle2[a],cex=1.15, adj=0.5)
  text(-0.2, 27, graph_number[a], font=2, cex=1.2,adj=0.1) # Subgraph number (a,b,c...)
  # Mlk production during high alpine grazing
  if(i==1){
    par(mar = c(1, 4, 1, 0), xpd=NA)
    barplot(NA, ylim=c(0,25), ylab=expression(paste('',Delta,'fat total [%]')), col='darkred',panel.first=grid(nx=0, ny=NULL,lty='solid', lwd=0.2))
    # Legend of delta d
    legend(x=0.4, y=22,legend=c('>0.05','<0.05'), col=c('grey','grey21'), cex=1.1, title='Signif', pch=19, bg='white')
  }else{
    par(mar = c(1, 1.5, 1, 0), xpd=TRUE)
    if(i>12){
      barplot(as.numeric(as.character(delta_milk_total[i:(i+1),'value'])), ylim=c(0,25), yaxt='n', col='darkred', panel.first=grid(nx=0, ny=NULL,lty='solid', lwd=0.2))
    } else {
      barplot(NA, ylim=c(0,25), yaxt='n',panel.first=grid(nx=0, ny=NULL,lty='solid', lwd=0.2))
    }
  }
  
  text(-0.2, 27, graph_number2[a], font=2, cex=1.2,adj=0)
  # Define margin
  if(i==1){
    par(mar = c(4, 4, 1, 0), xpd=NA)
  } else {
    par(mar = c(4, 1.5, 1, 0), xpd=NA)
  }
  # Delat d barplot
  if(i==1){ #First plot also shwo y-axis
    plot(c(1,2),as.numeric(as.character(deltad[i:(i+1),'value'])), ylim=c(-0.1,0.1),xlim=c(0.5,2.5), xaxt='n',bty='n',xlab='',ylab=expression(paste('',Delta,'d-parameter')),col=as.character(colorf[i:(i+1),'pval']),las=3, pch=19)
  }else { # Do not show y-axis
    plot(c(1,2),as.numeric(as.character(deltad[i:(i+1),'value'])), ylim=c(-0.1,0.1),xlim=c(0.5,2.5), xaxt='n',yaxt='n',xlab='',ylab='',bty='n',col=as.character(colorf[i:(i+1),'pval']),las=3, pch=19,panel.first=grid(nx=0, ny=NULL,lty='solid', lwd=0.2, col=c('lightgrey','lightgrey','grey27','lightgrey','lightgrey','lightgrey')))
  }
  
  mtext(text = c('Autumn','Winter'), side = 1, at = c(1,2), line = -0.7, las=2, cex=0.7)
  text(0, 0.12, graph_number3[a],cex=1.2, font=2, adj=0) # Subgraph number (a,b,c...)
  if(i==11){
    mtext('calving season',1,2.5) # x-axis legend
  }
  if(i==1){
    # Legend of delta d
    #plot.new()
    #legend(x=0.04, legend=c('>0.05','<0.05'), fill=c('grey','grey21'), cex=1.1, title='Signif')
  }
  
}
# Release Fig4.png
dev.off()
# Define old layout
layout( matrix(c(1), ncol=1) )


