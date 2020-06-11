-- b01
-------
-- create backup
CREATE TABLE b01_raw as (SELECT * FROM b01) ;
-- enlever les clés dupliquées
DELETE FROM b01
WHERE betriebid in (  
  select betriebid
  from b01
  group by betriebid
  having count(*)>1)
AND betriebid||coalesce(name,'')||coalesce(vorname,'') not in (select name from (select betriebid, max(betriebid||coalesce(name,'')||coalesce(vorname,'')) as name
  from b01
  group by betriebid
  having count(*)>1) a );
-- CREATE PRIMARY KEY
ALTER TABLE b01 
  ADD CONSTRAINT b01pk PRIMARY KEY (betriebid);
-- CREATE INDEX
ALTER TABLE b01 ADD COLUMN geom geometry;
UPDATE b01 SET geom=st_setsrid(st_makepoint(x*1000,y*1000),21781);
CREATE INDEX ind_b01bid ON b01 (betriebid);
CREATE INDEX idx_b01geom ON b01 using gist(geom);
ALTER TABLE b01 ADD COLUMN geom_4326 Geometry('Point');
UPDATE b01 SET geom_4326=st_transform(geom,4326);
CREATE INDEX b01_gm2 ON b01 USING gist(geom_4326);
vacuum analyze b01;


--k01
------
-- create backup
CREATE TABLE k01_raw as (SELECT * FROM k01) ;
alter table k01 add column oid serial ; --create unique column to be able to discard duplicate rows
-- delete duplicates
DELETE FROM k01 -- attention, enlève la moitié des animaux!
WHERE oid IN (SELECT oid
              FROM (SELECT oid,
                             ROW_NUMBER() OVER (partition BY tier_id, geburt_datum, tier_name, mutter_id, vater_id ORDER BY oid) AS rnum
                     FROM k01) t
              WHERE t.rnum > 1);
-- CREATE PK
ALTER TABLE k01 
  ADD CONSTRAINT k01pk2 PRIMARY KEY (tier_id);
-- CREATE INDEX
CREATE INDEX ind_k01bidn ON k01 (tier_id);
-- insert betriebid in b01 where betriebid exists in k01 but not in b01
INSERT INTO b01(betriebid) (SELECT DISTINCT a.betriebid
FROM k01 a LEFT JOIN b01 b ON a.betriebid=b.betriebid
WHERE b.betriebid is null
AND a.betriebid is not null);
-- FOREIGN KEY and index
ALTER TABLE k01 ADD  FOREIGN KEY(betriebid) REFERENCES b01(betriebid);
CREATE INDEX ind_k01bid2n ON k01 (betriebid);
--Change date to date format
alter table k01 alter column geburt_datum type date using to_date(geburt_datum::text,'YYYYMMDD');
alter table k01 alter column abgangdatum type date using to_date(abgangdatum::text,'YYYYMMDD');
alter table k01 alter column zugangdatum type date using to_date(zugangdatum::text,'YYYYMMDD');
vacuum analyze k01;


--k04
--------
-- create backup
CREATE TABLE k04_raw as (SELECT * FROM k04) ;
-- foreign key betriebid
ALTER TABLE k04 ADD FOREIGN KEY(betriebid) REFERENCES b01(betriebid);
-- ALTER TABLE k04 ADD FOREIGN KEY(betrieb_id_ort) REFERENCES b01(betriebid);
-- add missing animals in k01
INSERT INTO k01(tier_id) (SELECT DISTINCT a.tier_id
FROM k04 a LEFT JOIN k01 b ON a.tier_id=b.tier_id
WHERE b.tier_id is null);
--add foreign key tier_id
ALTER TABLE k04 ADD  FOREIGN KEY(tier_id) REFERENCES k01(tier_id);
-- CREATE PK
ALTER TABLE k04 
  ADD CONSTRAINT k04pkn PRIMARY KEY (tier_id, laktation_nr,abschlussart);
-- CREATE INDEX
CREATE INDEX ind_k04bidn ON k04 (betriebid);
CREATE INDEX ind_k04tidn ON k04 (tier_id);
CREATE INDEX ind_k04lakn ON k04 (laktation_nr);
CREATE INDEX ind_k04abn ON k04 (abschlussart);
vacuum analyze k04;
--Change date to date format
alter table k04 alter column kalbedt type date using to_date(kalbedt::text,'YYYYMMDD');

--k07
-------
CREATE TABLE k07_raw as (SELECT * FROM k07) ;
-- foreign key betriebid and tier_id
ALTER TABLE k07 ADD FOREIGN KEY(betriebid) REFERENCES b01(betriebid);
-- add missing animals in k01
INSERT INTO k01(tier_id) (SELECT DISTINCT a.tier_id
FROM k07 a LEFT JOIN k01 b ON a.tier_id=b.tier_id
WHERE b.tier_id is null);
--add foreign key tier_id
ALTER TABLE k07 ADD FOREIGN KEY(tier_id) REFERENCES k01(tier_id);
-- create pk
ALTER TABLE k07 
  ADD CONSTRAINT k07pk PRIMARY KEY (tier_id, datum_beschreibung);
-- CREATE INDEX
CREATE INDEX ind_k07bid ON k07 (betriebid);
CREATE INDEX ind_k07tid ON k07 (tier_id);
CREATE INDEX ind_k07dat ON k07 (datum_beschreibung);
vacuum analyze k07;

--k10
-------
-- pas fait car ne va pas être bcp utilisée
--Change date to date format
update k10 set besamungsdt=20050625 where besamungsdt=2050625
update k10 set besamungsdt=20160812 where besamungsdt=2160812
update k10 set dt_vor_besamung=20050625 where dt_vor_besamung=2050625
update k10 set dt_vor_besamung=20160812 where dt_vor_besamung=2160812 --error
alter table k10 alter column besamungsdt type date using to_date(besamungsdt::text,'YYYYMMDD');
alter table k10 alter column datum_abkalbung type date using to_date(datum_abkalbung::text,'YYYYMMDD');
alter table k10 alter column dt_vor_besamung type date using to_date(dt_vor_besamung::text,'YYYYMMDD');
update k10 set besamungsdt=to_date(20160203::text,'YYYYMMDD') where extract(year from besamungsdt)=3016 --error in the date
CREATE INDEX ind_k10tid ON k10 (tier_id);
vacuum analyze k10;


--k11
--------
-- pas fait car ne va pas être bcp utilisée

--k33
------
CREATE TABLE k33_raw as (SELECT * FROM k33) ;
-- problème lors de l'import de la colonne rasse:
update k33 set tier_rasse=null;
update k33 set tier_rasse=k04.tier_rasse from k04 where k33.tier_id=k04.tier_id;
-- foreign key betriebid and tier_id
ALTER TABLE k33 ADD FOREIGN KEY(betriebid) REFERENCES b01(betriebid);
INSERT INTO b01(betriebid) (SELECT DISTINCT a.betrieb_id_ort
FROM k33 a LEFT JOIN b01 b ON a.betrieb_id_ort=b.betriebid
WHERE b.betriebid is null);
ALTER TABLE k33 ADD FOREIGN KEY(betrieb_id_ort) REFERENCES b01(betriebid);
ALTER TABLE k33 ADD FOREIGN KEY(tier_id) REFERENCES k01(tier_id);
-- PK
ALTER TABLE k33
  ADD CONSTRAINT k33pkn PRIMARY KEY (tier_id, dt_probe, probe_nr,laktation_nr,milch);
vacuum analyze k33;
--Change date to date format
alter table k33 alter column kalbedatum type date using to_date(kalbedatum::text,'YYYYMMDD');
alter table k33 alter column dt_probe type date using to_date(dt_probe::text,'YYYYMMDD');