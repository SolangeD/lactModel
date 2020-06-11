#ClimGen import data
#####################
# Data was separated between Braunvieh and Original Braunvieh so that I had to import them separately and merge the file
#Parameter and library
require("RPostgreSQL")
require("tables")
setwd('D:/ClimGen-CH/Data/Braunvieh_data_archive2')
rm(list=ls(all=TRUE))
# Fonction import
#----------------
import_fct=function(extension,type,fin,longueur,columnname,numFiles){
	
	con_pos=dbConnect("PostgreSQL", dbname = "climgen_ch",host = "localhost", port = 5433,user = "postgres", password = "postgres") # connexion à la base de données
	type=lapply(type, function(x){gsub("t","varchar",x)}) #t dans type correspond à varchar dans postgres
	type=lapply(type, function(x){gsub("i","integer",x)})
	type=lapply(type, function(x){gsub("f","real",x)})
	data=NULL
	for(nF in 1:numFiles){
		if(numFiles==1){
			fileName=paste('ClimGen.',extension,sep="")
		}
		else{
			fileName=paste('ClimGen_00',nF,'.',extension,sep="")
		}
		conn <- file(fileName,open="r")
		linn <-readLines(conn)
		data2=matrix(nrow=length(linn), ncol=0)
		for(j in 1:length(fin)){ #boucle sur les colonnes
			col=substr(linn,fin[j]-longueur[j]+1,fin[j])
			col=gsub("^\\s+|\\s+$", "", col)
			data2=cbind(data2,col)
		}
		data=rbind(data, data2)
		close(conn)
	}
	
	colnames(data)=columnname #nommer les colonnes pour les avoir dans la BD
	dbWriteTable(con_pos,extension,value=data.frame(data),row.names=FALSE)

	for(j in 1:length(type)){ #boucle sur les colonnes
		sql1=paste("UPDATE ",extension," SET ",columnname[j],"=NULL  WHERE ",columnname[j],"=''", sep="") #remplace les '' par des nulls (autrement problème lors du cast)
		sql1b=paste("UPDATE ",extension," SET ",columnname[j],"=NULL  WHERE ",columnname[j]," like '%#%'", sep="") #remplace les ### par des nulls (autrement problème lors du cast)
		sql1c=paste("UPDATE ",extension," SET ",columnname[j],"=NULL  WHERE ",columnname[j]," like '-'", sep="") #remplace le - par des nulls (autrement problème lors du cast)
		sql2=paste("ALTER TABLE ",extension," ALTER ",columnname[j]," TYPE ",type[j]," USING ",columnname[j],"::",type[j], sep="") #cast en type prédéfini

		dbSendQuery(con_pos,sql1)
		dbSendQuery(con_pos,sql1b)
		dbSendQuery(con_pos,sql1c)
		dbSendQuery(con_pos,sql2)
	}

	on.exit(dbDisconnect(con_pos))
}


# Import b01 file
#----------------
extension='b01'
columnname=c('satzart','version','betriebid','betriedidtvd','prafix','sprachcode',
'anredecode','name','vorname','hofname','strasse_nr','postleitzahl','ort','kanton','land',
'kataster','hohe','region','gdenr','telefon','typ','status','datum','y','x')

fin=c(3,5,15,22,52,54,56,78,100,122,144,149,179,181,184,185,187,189,193,208,212,213,221,229,238) #nombre de caractère du début de la colonne (voir la doc)
longueur=c(3,2,10,7,30,2,2,22,22,22,22,5,30,2,3,1,2,2,4,15,4,1,8,5,9) #nombre de caractère dans la colonne (voir la doc)
type=c('t','i','i','i','t','t','t','t','t','t','t','i','t','t','t','i','i','i','i','t','i','i','i','f','f') #type: f=real, i=integer, t=varchar
import_fct(extension,type,fin,longueur,columnname,1)
#suppl query: ALTER TABLE b01 ADD COLUMN geom geometry('Point',21781)
#UPDATE b01 SET geom=ST_SetSRID(ST_MakePoint(x*1000,y*1000),21781)

# Import k01 file
#----------------
extension='k01'
columnname=c('satzart','version','betriebid','betriebidtvd','tier_id','tier_rasse','tier_name','geburt_datum','vater_id','vater_rasse','mutter_id',
'mutter_rasse','tier_hauptrasse','blutanteil_hrasse','zweite_rasse','blutanteil_zrasse','dritte_rasse','blutanteil_drasse','geschlecht','betriebid_zuchter',
'betriebidtvd_zuchter','zugangdatum','abgangdatum','abgangursache','tiername_lang','haltung_berg_von','haltung_berg_bis','haltung_berg_ort','zusatzk_jahr',
'zusatzk_kt','farbe','original_tierid','genossenschaft','tripla_code','nummer_herde','herkunft_datum')
fin=c(3,5,15,22,36,39,51,59,73,76,90,93,96,99,102,105,108,111,112,122,129,137,145,146,202,210,218,228,232,234,236,256,276,282,286,288)
longueur=c(3,2,10,7,14,3,12,8,14,3,14,3,3,3,3,3,3,3,1,10,7,8,8,1,56,8,8,10,4,2,2,20,20,6,4,2)
type=c('t','i','i','i','t','t','t','i','t','t','t','t','t','i','t','i','t','i','i','i','i','i','i','i','t','i','i','i','i','t','i','t','t','t','t','i')

import_fct(extension,type,fin,longueur,columnname,6)

# Import k04 file
#----------------
extension='k04'
columnname=c('satzart','version','betriebid','betriebidtvd','tier_id','tier_rasse','tier_name','betrieb_id_ort','betriebtvd_id_ort','laktation_nr','kalbedt','kalbealter',
'abschlussart','tage_milch','milch','fett','fett_p','eiweiss','eiweiss_p','laktose','zellzahl','milchharnstoff','persistenz','int_abkalb_besamung','index_laktation',
'index_betrieb','betrieb_durchschnitt','kataster','alpung','laktation_code','region','melk_methode','prufmethode')
fin=c(3,5,15,22,36,39,51,61,68,70,78,83,84,88,93,97,101,105,109,113,118,121,124,127,130,133,138,139,140,141,143,145,147)
longueur=c(3,2,10,7,14,3,12,10,7,2,8,5,1,4,5,4,4,4,4,4,5,3,3,3,3,3,5,1,1,1,2,2,2)
#fin=c(3+1,5+2,15+3,22+4,36+5,39+6,51+7,61+8,68+9,70+10,78+11,83+12,84+13,88+14,93+15,97+16,101+17,105+18,109+19,113+20,118+21,121+22,124+23,127+24,130+25,133+26,138+27,139+28,140+29,141+29,143+30,145+31,147+32) #adaptation dû à des espaces en plus
#longueur=c(4,2,11,8,15,4,13,11,8,3,9,6,2,5,6,5,5,5,5,5,6,4,4,4,4,6,2,2,1,3,3,3)
type=c('t','i','i','i','t','t','t','i','i','i','i','f','i','i','i','i','f','i','f','i','i','i','i','i','i','i','t','t','i','i','i','i','i')
import_fct(extension,type,fin,longueur,columnname,6)

# Import k07 file
#----------------
extension='k07'
columnname=c('satzart','version','betriebid','betriebidtvd','tier_id','tier_rasse','tier_name','laktation_nr','datum_beschreibung','typ','format_widerristhohe','format_beckenlange','format_brustumfang',
'format_lange','format_beckenneigung','format_tiefe','format_breite','format_bemuskelung','format_olinie','fund_wink','fund_auspragung','fund_fesseln','fund_klauensatz','euter_vor_lange','euter_nach',
'euter_brete','euter_hohe','euter_vor_aufhangung','euter_tiefe','euter_zentral','zizten_ausbildung','zitzen_lange','zizten_verteilung_vorne','zitzen_verteilung_hinten','zitzen_stellung','zitzen_zusatz',
'format_note','fund_note','euter_note','zitzen_note','gesamt_note','b_p','bsc','format_lage','format_brustbreie','format_kbhohe','format_ftiefe','format_bbreite','euter_boden','rahmen_note','becken_note')
fin=c(3,5,15,22,36,39,51,53,61,62,67,72,77,82,87,92,97,102,107,112,117,122,127,132,137,142,147,152,157,162,167,172,177,182,187,192,197,202,207,112,217,219,224,229,234,239,244,249,254,259,264)
longueur=c(3,2,10,7,14,3,12,2,8,1,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,2,5,5,5,5,5,5,5,5,5)
type=c('t','i','i','i','t','t','t','i','i','i','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','f','i','f','f','f','f','f','f','f','f','f')
import_fct(extension,type,fin,longueur,columnname,1)


# Import k10 file
#----------------
extension='k10'
columnname=c('satzart','version','betriebid','betriebidtvd','tier_id','tier_rasse','tier_name','betriebid_ort','betriebidtvd_ort','laktationsnummer','kuh_rind','datum_abkalbung','besamungsdt',
'besamung_code','nummer_besamung','stier_id','stier_rasse','stier_name','dt_vor_besamung','code_lieferant','id_lieferant','code_besamer','code_teststier','code_stierenwechsel','stier_kategorie','hofcontainer','reservation','spezialcode',
'ejakulat_dt','samenbehandlung','besamung_id','mutationscode','code_daten','genet_mutter_id','genet_mutter_rasse','gsextes_geschlecht','belegdatum_bis','daten_herkunft')
fin=c(3,5,15,22,36,39,51,61,68,70,71,79,87,88,90,104,107,119,127,129,136,138,139,140,142,143,144,146,154,155,170,171,179,193,196,197,205,207)
longueur=c(3,2,10,7,14,3,12,10,7,2,1,8,8,1,2,14,3,12,8,2,7,2,1,1,2,1,1,2,8,1,15,1,8,14,3,1,8,2)
type=c('t','i','i','i','t','t','t','i','i','i','i','i','i','i','i','t','t','t','i','i','t','i','i','i','t','i','i','i','i','t','i','t','i','i','i','i','i','i')
import_fct(extension,type,fin,longueur,columnname,1)

# Import k11 file
#----------------
extension='k11'
columnname=c('satzart','version','betriebid','betriebidtvd','mutter_id','mutter_rasse','tiername','betribid_ort_abk','betriebidtvd_ort_abk','laktation_nr','kalbedt','kalb_id','kalb_rasse','geschlecht','zwilling',
'vater_id','vater_rasse','abort','zkz','geburt_verlauf','kalb_tod_24','geburt_gewicht','farbe','ausweis','genet_mutter_id','genet_mutter_rasse','besamung','kastriert','totgeburt','zeit_tod','erbfehler_code',
'erbfehler_code2','erbfehler','original_bewegung_tvd','aktuel_bewegung_tvd','mutationscode','missbildung','stanzprobe_nr','geburtdt_mutter','betriebid_ganz_tvd')
fin=c(3,5,15,22,36,39,51,61,68,70,78,92,95,96,97,111,114,115,118,119,120,122,124,125,139,142,150,151,152,155,158,161,197,212,227,228,231,237,245,252)
longueur=c(3,2,10,7,14,3,12,10,7,2,8,14,3,1,1,14,3,1,3,1,1,2,2,1,14,3,8,1,1,3,3,3,36,15,15,1,3,6,8,7)
type=c('t','i','i','i','t','t','t','i','i','i','i','t','t','i','i','t','t','i','i','i','i','i','i','i','t','t','i','i','i','i','i','i','t','i','i','t','i','i','i','i')
import_fct(extension,type,fin,longueur,columnname,1)

# Import k33 file
#----------------
extension='k33'
columnname=c('satzart','version','betriebid','betriebidtvd','tier_id','tier_rasse','tier_name','betrieb_id_ort','betriebtvd_id_ort','kalbedatum','laktation_nr','probe_nr', 'dt_probe',
'milch','fett','eiweiss','laktose','persitenz','zellzahl','milchharnstoff','bemerkung','alp_hohe','citrat','melkmethode','prufmethode','aceton','milch_morgen','milch_abend','fett_p','eiweis_p',
'code_w','code_labor','melkzt_morgen','melkzt_abend','anmeldung_mbk','anmeldung_lbe','kasein_p','laufnummer','wagetyp','datenherkunft')
#fin=c(3+1,5+2,15+3,22+4,36+5,37+6,51+7,61+8,68+9,76+10,78+11,81+12,89+13,93+14,97+15,101+16,105+17,108+18,112+19,115+20,117+21,119+22,122+23,124+24,126+25,129+26,133+27,137+28,141+29,145+30,147+31,149+32,154+33,159+34,161+35,163+36,167+37,171+38,172+39,174+40)
#longueur=c(4,3,11,8,15,4,13,11,8,9,3,4,9,5,5,5,5,4,5,4,3,3,4,3,3,4,5,5,5,5,3,3,6,6,3,3,5,5,2,3)
fin=c(3,5,15,22,36,37,51,61,68,76,78,81,89,93,97,101,105,108,112,115,117,119,122,124,126,129,133,137,141,145,147,149,154,159,161,163,167,171,172,174)
longueur=c(4-1,3-1,11-1,8-1,15-1,4-1,13-1,11-1,8-1,9-1,3-1,4-1,9-1,5-1,5-1,5-1,5-1,4-1,5-1,4-1,3-1,3-1,4-1,3-1,3-1,4-1,5-1,5-1,5-1,5-1,3-1,3-1,6-1,6-1,3-1,3-1,5-1,5-1,2-1,3-1)
type=c('t','i','i','i','t','t','t','i','i','i','i','i','i','f','f','f','f','i','i','i','i','i','i','i','i','i','f','f','f','f','i','i','t','t','i','i','f','i','i','i')
import_fct(extension,type,fin,longueur,columnname,6)

dbDisconnect(con_pos)