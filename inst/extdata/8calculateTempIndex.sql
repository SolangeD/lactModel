-- Créer des silos avec betriebid, year, daynum et valeur météo

--tmax
drop table if exists farm_tmaxd;
create table farm_tmaxd (betriebid integer, year2 integer, day_num integer, tmaxd double precision);

DO $$DECLARE 
	i integer;
	numband2 integer;
	nametable varchar;
	geomname varchar;
BEGIN
	FOR y IN 2001..2016
	LOOP
		IF y >=2014 THEN 
			nametable='tmaxd_'||y||'_21781';
			geomname='geom';
		ELSE nametable='tmaxd_'||y||'_4326';
			geomname='geom_4326';
		END IF;
		EXECUTE 'select st_numbands(rast) as numband  from '||nametable||' limit 1'
   		INTO numband2;
		FOR i IN 1..numband2
		LOOP
			EXECUTE 'INSERT INTO farm_tmaxd 
				(SELECT betriebid,
				'||y||' AS year2,
				'||i||' AS day_num,
				(geomval).val  as tmaxd 
				FROM (select st_pixelaspolygons(rast,'||i||') as geomval from '||nametable||') a JOIN 
				b01 on ST_Intersects((geomval).geom,'||geomname||')
				WHERE x is not null)';
		END LOOP;
	
	END LOOP;
END$$; 
update farm_tmaxd set tmaxd=null where tmaxd<-100;

CREATE TABLE farm_tmaxd2 AS
(SELECT betriebid, year2, day_num, to_date((extract(j from (year2::varchar||'-01-01')::date)+day_num-1)::text,'J') as date2, tmaxd
from farm_tmaxd
);

drop table farm_tmaxd;
alter table farm_tmaxd2 rename to farm_tmaxd;

create index on farm_tmaxd(betriebid);
create index on farm_tmaxd(date2);
create index on farm_tmaxd(year2);

vacuum analyze farm_tmaxd;

--tabsd
drop table if exists farm_tabsd;
create table farm_tabsd (betriebid integer, year2 integer, day_num integer, tabsd double precision);

DO $$DECLARE 
	i integer;
	numband2 integer;
	nametable varchar;
	geomname varchar;
BEGIN
	FOR y IN 2001..2016
	LOOP
		IF y >=2014 THEN 
			nametable='tabsd_'||y||'_21781';
			geomname='geom';
		ELSE nametable='tabsd_'||y||'_4326';
			geomname='geom_4326';
		END IF;
		EXECUTE 'select st_numbands(rast) as numband  from '||nametable||' limit 1'
   		INTO numband2;
		FOR i IN 1..numband2
		LOOP
			EXECUTE 'INSERT INTO farm_tabsd 
				(SELECT betriebid,
				'||y||' AS year2,
				'||i||' AS day_num,
				(geomval).val  as tabsd 
				FROM (select st_pixelaspolygons(rast,'||i||') as geomval from '||nametable||') a JOIN 
				b01 on ST_Intersects((geomval).geom,'||geomname||')
				WHERE x is not null)';
		END LOOP;
	END LOOP;
END$$; 
update farm_tabsd set tabsd=null where tabsd<-100;

CREATE TABLE farm_tabsd2 AS
(SELECT betriebid, year2, day_num, to_date((extract(j from (year2::varchar||'-01-01')::date)+day_num-1)::text,'J') as date2, tabsd
from farm_tabsd
);

drop table farm_tabsd;
alter table farm_tabsd2 rename to farm_tabsd;

create index on farm_tabsd(betriebid);
create index on farm_tabsd(date2);
create index on farm_tabsd(year2);

vacuum analyze farm_tabsd;
--rhiresd
drop table if exists farm_rhiresd;
create table farm_rhiresd (betriebid integer, year2 integer, day_num integer, rhiresd double precision);

DO $$DECLARE 
	i integer;
	numband2 integer;
	nametable varchar;
	geomname varchar;
BEGIN
	FOR y IN 2001..2016
	LOOP 
		IF y >=2015 THEN 
			nametable='rhiresd_'||y||'_21781';
			geomname='geom';
		ELSE nametable='rhiresd_'||y||'_4326';
			geomname='geom_4326';
		END IF;
		EXECUTE 'select st_numbands(rast) as numband  from '||nametable||' limit 1'
   		INTO numband2;
		FOR i IN 1..numband2
		LOOP
			EXECUTE 'INSERT INTO farm_rhiresd 
				(SELECT betriebid,
				'||y||' AS year2,
				'||i||' AS day_num,
				(geomval).val  as rhiresd 
				FROM (select st_pixelaspolygons(rast,'||i||') as geomval from '||nametable||') a JOIN 
				b01 on ST_Intersects((geomval).geom,'||geomname||')
				WHERE x is not null)';
		END LOOP;
	END LOOP;
END$$; 
update farm_rhiresd set rhiresd=null where rhiresd<-100;

CREATE TABLE farm_rhiresd2 AS
(SELECT betriebid, year2, day_num, to_date((extract(j from (year2::varchar||'-01-01')::date)+day_num-1)::text,'J') as date2, rhiresd
from farm_rhiresd
);

drop table farm_rhiresd;
alter table farm_rhiresd2 rename to farm_rhiresd;

create index on farm_rhiresd(betriebid);
create index on farm_rhiresd(date2);
create index on farm_rhiresd(year2);

vacuum analyze farm_rhiresd;

--rhiresm
drop table if exists farm_rhiresm;
create table farm_rhiresm (betriebid integer, year2 integer, month_num integer, rhiresm double precision);

DO $$DECLARE 
	i integer;
	numband2 integer;
	nametable varchar;
BEGIN
	FOR y IN 2001..2016
	LOOP 
		IF y >=2015 THEN 
			nametable='rhiresm_'||y||'_21781';
			geomname='geom';
		ELSE nametable='rhiresm_'||y||'_4326';
			geomname='geom_4326';
		END IF;
		EXECUTE 'select st_numbands(rast) as numband  from '||nametable||' limit 1'
   		INTO numband2;
		FOR i IN 1..numband2
		LOOP
			EXECUTE 'INSERT INTO farm_rhiresm 
				(SELECT betriebid,
				'||y||' AS year2,
				'||i||' AS month_num,
				(geomval).val  as rhiresm 
				FROM (select st_pixelaspolygons(rast,'||i||') as geomval from '||nametable||') a JOIN 
				b01 on ST_Intersects((geomval).geom,'||geomname||')
				WHERE x is not null)';
		END LOOP;
	END LOOP;
END$$; 
update farm_rhiresm set rhiresm=null where rhiresm<-100;
create index on farm_rhiresm(betriebid);
create index on farm_rhiresm(month_num);
create index on farm_rhiresm(year2);

vacuum analyze farm_rhiresm;

--Interpolation of RH and WS: see interpolation_rh_ws.sql
--Inverse distance interpolation, weight^2
--Relative Humidity
---------------------
--Distance farm-weather station within 50km
CREATE TABLE farm_rh_stations AS (
select a.stn, b.betriebid, st_distance(a.geom, b.geom) as dist
from rh_stations a, b01 b
where st_dwithin(b.geom, a.geom, 50000));
CREATE INDEX frs_bid ON farm_rh_stations USING btree(betriebid);

--Interpolation
CREATE TABLE farm_rh AS (
SELECT 
betriebid, date2, sum(ure200d0*dist^2)/sum(dist^2) as rh_interp
FROM farm_rh_stations join rh_data ON farm_rh_stations.stn=rh_data.stn
GROUP BY betriebid, date2);
create index on farm_rh(betriebid);
create index on farm_rh(date2);
--Wind speed
------------
--Distance farm-weather station within 50km
CREATE TABLE farm_ws_stations AS (
select a.stn, b.betriebid, st_distance(a.geom, b.geom) as dist
from ws_stations a, b01 b
where st_dwithin(b.geom, a.geom, 50000));
CREATE INDEX fws_bid ON farm_ws_stations USING btree(betriebid);
--Interpolation
CREATE TABLE farm_ws AS (
SELECT 
betriebid, date2, sum(fkl010d0*dist^2)/sum(dist^2) as ws_interp
FROM farm_ws_stations join ws_data ON farm_ws_stations.stn=ws_data.stn
GROUP BY betriebid, date2);
create index on farm_ws(betriebid);
create index on farm_ws(date2);
--Get altitude of 2km raster
drop table if exists farm_zrast_2km;
create table farm_zrast_2km (betriebid integer, zrast double precision);
INSERT INTO farm_zrast_2km 
				(SELECT betriebid,
				(geomval).val  as zrast 
				FROM (select st_pixelaspolygons(rast,1) as geomval from dem_2km_21781) a JOIN 
				b01 on ST_Intersects((geomval).geom,geom)
				WHERE x is not null);
--Get altitude of 1km raster			
drop table if exists farm_zrast_1km;
create table farm_zrast_1km (betriebid integer, zrast double precision);
INSERT INTO farm_zrast_1km 
				(SELECT betriebid,
				(geomval).val  as zrast 
				FROM (select st_pixelaspolygons(rast,1) as geomval from dem_1km_21781) a JOIN 
				b01 on ST_Intersects((geomval).geom,geom)
				WHERE x is not null);
create index on farm_zrast_2km(betriebid);
create index on farm_zrast_1km(betriebid);

--thi, csi
/*CREATE TABLE farm_tindex AS (
	SELECT 
	tmd.betriebid,
	tmd.year2,
	tmd.day_num,
	0.8*tmaxd+(rh_interp/100*(tmaxd-14.4))+46.4 as thi,
	(11.7+(3.1*ws_interp^(0.5)))*(40-tabsd)+481+418*(1-exp(-0.04*rhiresd)) as csi
	FROM
	farm_tmaxd tmd
	JOIN farm_tabsd td ON tmd.betriebid=td.betriebid AND tmd.year2=td.year2 AND tmd.day_num=td.day_num
	JOIN farm_rhiresd rd ON rd.betriebid=td.betriebid AND rd.year2=td.year2 AND rd.day_num=td.day_num
	JOIN farm_rh rh ON rh.betriebid=rd.betriebid AND to_date((extract(j from (tmd.year2::char||'-01-01')::date)+tmd.day_num-1)::text,'J')=rh.date2
	JOIN farm_ws ws ON ws.betriebid=rd.betriebid AND to_date((extract(j from (tmd.year2::char||'-01-01')::date)+tmd.day_num-1)::text,'J')=ws.date2
);
create index on farm_tindex(betriebid);
create index on farm_tindex(day_num);
create index on farm_tindex(year2);

vacuum analyze farm_tindex;*/

create table farm_meteo as 
(select 
 	tmd.betriebid,
 	tmd.date2,
	round(tmaxd::numeric,2)::numeric(4,2) as tmaxd,
	round(tabsd::numeric,2)::numeric(4,2) as tabsd,
	round(rh_interp::numeric,2)::numeric(5,2) as rh,
	round(ws_interp::numeric,2)::numeric(4,2) as ws,
	round(rhiresd::numeric,2)::numeric(5,2) as rhiresd
from 
     farm_tmaxd tmd
 LEFT JOIN farm_tabsd td ON td.betriebid=tmd.betriebid AND td.date2=tmd.date2 
 LEFT	 JOIN farm_ws ws ON ws.betriebid=tmd.betriebid AND ws.date2=tmd.date2 
	LEFT JOIN farm_rhiresd rd ON rd.betriebid=tmd.betriebid AND rd.date2=tmd.date2 
	LEFT 	 JOIN farm_rh rh ON rh.betriebid=tmd.betriebid AND rh.date2=tmd.date2 	 	
);
create index on farm_meteo(betriebid);
create index on farm_meteo(date2);


--calculate the avergae temperature humidity index and cold stress index for the 3 days before the record
-- ! corrects temperature with difference in altitude between pixel and recorded altitude (! 1km or 2km resolution for temperature depending on the year). 
--changer le temp gradient?
CREATE TABLE k33_tindex AS (
	SELECT 
	k33f.tier_id, 
	k33f.laktation_nr,
	k33f.probe_nr,
	avg(fz1.zrast) as zrast1,
	avg(fz2.zrast) as zrast2,
	avg(tmaxd) as tmaxd,
	avg(tabsd) as tabsd,
	avg(rh) as rh,
	avg(ws) as ws,
	avg(rhiresd) as rhiresd,
	avg(alp_hohe) as alp_hohe,
	avg(CASE WHEN alp_hohe>10 THEN 0.8*(tmaxd+(CASE WHEN extract(year from dt_probe)>=2014 THEN fz1.zrast ELSE fz2.zrast END-k33f.alp_hohe*100)::double precision/100*0.45)+(rh/100*((tmaxd+(CASE WHEN extract(year from dt_probe)>=2014 THEN fz1.zrast ELSE fz2.zrast END-k33f.alp_hohe*100)::double precision/100*0.45)-14.4))+46.4 ELSE 0.8*tmaxd+(rh/100*(tmaxd-14.4))+46.4 END) as thi,
	avg(CASE WHEN alp_hohe>10 THEN (11.7+(3.1*ws^(0.5)))*(40-(tabsd+(CASE WHEN extract(year from dt_probe)>=2014 THEN fz1.zrast ELSE fz2.zrast END-k33f.alp_hohe*100)::double precision/100*0.65))+481+418*(1-exp(-0.04*rhiresd)) ELSE (11.7+(3.1*ws^(0.5)))*(40-tabsd)+481+418*(1-exp(-0.04*rhiresd)) END) AS csi
	FROM
	k33f
	LEFT JOIN farm_zrast_1km fz1 on k33f.betrieb_id_ort=fz1.betriebid 
	LEFT JOIN farm_zrast_2km fz2 on k33f.betrieb_id_ort=fz2.betriebid 
	LEFT JOIN farm_meteo meteo ON meteo.betriebid=k33f.betrieb_id_ort AND (meteo.date2=k33f.dt_probe or meteo.date2=(k33f.dt_probe-1) or meteo.date2=(k33f.dt_probe-2))
	GROUP BY k33f.tier_id, k33f.laktation_nr, k33f.probe_nr
);

/*select k33f.tier_id, k33f.laktation_nr, (k33f.alp_hohe*100-fz.zrast)::double precision)/100*0.65 alp_hohe
from defizit 
left join k33f on defizit.tier_id=k33f.tier_id and defizit.laktation_nr=k33f.laktation_nr and (defizit.probe_nr5=k33f.probe_nr or defizit.probe_nr6=k33f.probe_nr)
left join farm_zrast fz on k33f.betrieb_id_ort=fz.betriebid 
where k33f.alp_hohe>0 and fz.zrast>0
limit 100*/

-- precipitations over spring and summer
select betriebid, year2, avg(rhiresm) as avg_precip
from farm_rhiresm
where month_num>=4 and month_num<=8
group by betriebid, year2

--fin du script 
---------------------
---------------------
-- la suite n'a pas été utilisée mais est là comme une relique du temps passé =D
--tmin pas utilisé

create table farm_temperature (betriebid integer, day_num integer, temperature double precision);

DO $$DECLARE 
	i integer;
	numband2 integer;
BEGIN
	SELECT numband INTO numband2 from (select st_numbands(rast) as numband  from alt_tmind_total limit 1) q;
	FOR i IN 2..numband2
	LOOP
        EXECUTE 'INSERT INTO farm_temperature 
					(SELECT betriebid,
						'||i||' AS day_num,
						(geomval).val  as temperature 
						FROM (select st_pixelaspolygons(rast,'||i||') as geomval from alt_tmind_total) a JOIN 
						b01 on ST_Intersects((geomval).geom,geom)
						WHERE x is not null)';
    END LOOP;
END$$; 
-- daynum 2= 1.1.2001
-- pour les valeurs nulles
update farm_temperature set temperature=null where temperature<-100;
create index on farm_temperature(betriebid);
create index on farm_temperature(day_num);

vacuum analyze farm_temperature;

-- POUR L'ALTITUDE
CREATE TABLE farm_zrast AS 
					(SELECT betriebid,
						(geomval).val  as zrast 
						FROM (select st_pixelaspolygons(rast,1) as geomval from alt_tmind_total) a JOIN 
						b01 on ST_Intersects((geomval).geom,geom)
						WHERE x is not null);
-- pour les valeurs nulles
update farm_zrast set zrast=null where zrast<-100;
						
create index on farm_zrast(betriebid);
vacuum analyze farm_zrast;

--à injecter dans regression_input
--voir si on fait la moyenne, le min etc...
select k33f.tier_id, k33f.laktation_nr, least(case when defizit.probe_nr3=k33f.probe_nr then ft.temperature else null end,case when defizit.probe_nr4=k33f.probe_nr then ft.temperature else null end) 
from defizit 
left join k33f on defizit.tier_id=k33f.tier_id and defizit.laktation_nr=k33f.laktation_nr and (defizit.probe_nr3=k33f.probe_nr or defizit.probe_nr4=k33f.probe_nr)
left join farm_temperature ft on k33f.betrieb_id_ort=ft.betriebid and ((k33f.dt_probe-to_date('2000-12-30','yyyy-mm-dd'))=ft.day_num or (k33f.dt_probe-to_date('2000-12-30','yyyy-mm-dd')-1)=ft.day_num)



--create a table with the temperature from each record. The temporary view trick enables the use of parallel query
CREATE TEMPORARY VIEW record_temperature_temp AS (
select tier_id, laktation_nr, temperature
from k33 join farm_temperature t on k33.betrieb_id_ort=t.betriebid and (dt_probe-to_date('2000-12-30','yyyy-mm-dd'))=t.day_num);
CREATE TABLE record_temperature (LIKE record_temperature_temp);
INSERT INTO record_temperature SELECT * FROM record_temperature_temp;

-- Autres tests	
----------------
--Pour avoir la température + l'altitude du cente du pixel de temp
	--approche plus rapide
DROP MATERIALIZED VIEW if exists min_temp CASCADE;
CREATE MATERIALIZED VIEW min_temp AS (	
SELECT tier_id, laktation_nr, probe_nr, alp_hohe, temp_today, temp_yesterday, st_value(d.rast, st_centroid(pixelgeom)) as z_rast
FROM
dem100 d
JOIN 
	(SELECT (st_pixelaspolygons(rast)).geom as pixelgeom
	FROM tmind_total
	) tgeom
ON ST_Intersects(d.rast,st_centroid(pixelgeom))
JOIN
	(SELECT tier_id, laktation_nr, probe_nr, alp_hohe, st_value(rast, daynum, farmgeom) as temp_today, st_value(rast, (daynum-1), farmgeom) as temp_yesterday, farmgeom
	FROM tmind_total t
	JOIN
	(SELECT tier_id, laktation_nr, probe_nr, dt_probe-to_date('2000-12-31','yyyy-mm-dd') as daynum, alp_hohe, geom as farmgeom
			FROM k33f join b01 on k33f.betrieb_id_ort=b01.betriebid
			WHERE x is not null and (dt_probe>=to_date('2001-01-02','yyyy-mm-dd') AND dt_probe<=to_date('2013-12-31','yyyy-mm-dd'))
			) b 
	ON ST_Intersects(rast,farmgeom)) a
ON st_intersects(tgeom.pixelgeom, farmgeom));

	--2eme approche bcp plus lente (conservé au cas où)
DROP MATERIALIZED VIEW if exists min_temp CASCADE;
CREATE MATERIALIZED VIEW min_temp AS (
SELECT tier_id, laktation_nr, probe_nr, alp_hohe,
	t.temp_today, t.temp_yesterday,
	ST_Value(d.rast, 1,st_centroid(t.tempgeom)) as z_rast
	FROM dem100 d, 
	(SELECT st_value(rast, daynum, a.geom)  as temp_today, st_value(rast, (daynum-1), a.geom) as temp_yesterday, 
		st_pixelaspolygon(rast, ST_WorldToRasterCoordX(rast, a.geom), ST_WorldToRasterCoordY(rast, a.geom)) as tempgeom, 
		a.geom as farmgeom, tier_id, laktation_nr, probe_nr, alp_hohe 
	FROM tmind_total JOIN 
		(SELECT tier_id, laktation_nr, probe_nr, dt_probe-to_date('2000-12-31','yyyy-mm-dd') as daynum, alp_hohe, geom
		FROM k33f join b01 on k33f.betrieb_id_ort=b01.betriebid
		WHERE x is not null and (dt_probe>=to_date('2001-01-02','yyyy-mm-dd') and dt_probe<=to_date('2013-12-31','yyyy-mm-dd'))
		) a on ST_Intersects(rast,a.geom)
	) t 
	WHERE st_intersects(d.rast, st_centroid(t.tempgeom))
	AND st_intersects(tempgeom, farmgeom));
	
-- Avec un raster ayant l'altitude en bande 1 et les température dans le reste
DROP MATERIALIZED VIEW if exists min_temp CASCADE;
CREATE MATERIALIZED VIEW min_temp AS (
	SELECT tier_id, laktation_nr, probe_nr, dt_probe, st_value(rast, bandnum, a.geom)  as temp_today, st_value(rast, (bandnum-1), a.geom) as temp_yesterday, 
	st_value(rast, 1, a.geom)  as alt_rast,  alp_hohe 
	FROM alt_tmind_total JOIN 
		(SELECT tier_id, laktation_nr, probe_nr, dt_probe, dt_probe-to_date('2000-12-31','yyyy-mm-dd')+1 as bandnum, alp_hohe, geom --dans raster de température, la bande 1=alt, bande 2= temp au 1.1.2001 etc
		FROM k33f join b01 on k33f.betrieb_id_ort=b01.betriebid
		WHERE x is not null and (dt_probe>=to_date('2001-01-02','yyyy-mm-dd') and dt_probe<=to_date('2013-12-31','yyyy-mm-dd'))
		) a on ST_Intersects(rast,a.geom));
		
		
/*
--rh_10mim 
drop table if exists farm_rh;
create table farm_rh (betriebid integer, month_num integer, rh double precision);

DO $$DECLARE 
	i integer;
	numband2 integer;
	nametable varchar;
BEGIN
	nametable='cru_10min_rh_grid';
	FOR i IN 1..12
	LOOP
		EXECUTE 'INSERT INTO farm_rh 
			(SELECT betriebid,
			'||i||' AS month_num,
			month_'||i||'  as rhiresm 
			FROM b01 b JOIN '||nametable||' a 
			ON ST_Intersects(a.geom,b.geom)
			WHERE x is not null)';
	END LOOP;
END$$; 
update farm_rh set rh=null where rh<-100;
create index on farm_rh(betriebid);
create index on farm_rh(month_num);

-- vacuum analyze farm_rh;

--rh_10mim 
drop table if exists farm_ws;
create table farm_ws (betriebid integer, month_num integer, ws double precision);

DO $$DECLARE 
	i integer;
	numband2 integer;
	nametable varchar;
BEGIN
	nametable='cru_10min_wind_grid';
	FOR i IN 1..12
	LOOP
		EXECUTE 'INSERT INTO farm_ws 
			(SELECT betriebid,
			'||i||' AS month_num,
			month_'||i||'  as ws 
			FROM b01 b JOIN '||nametable||' a 
			ON ST_Intersects(a.geom,b.geom)
			WHERE x is not null)';
	END LOOP;
END$$; 
update farm_ws set ws=null where ws<-100;
create index on farm_ws(betriebid);
create index on farm_ws(month_num);

-- vacuum analyze farm_ws;
*/