-- Given that BVO cows were added after BV cows, need to merge both sets of tables

--b01
------
--create geom column
ALTER TABLE b01bvo ADD COLUMN geom geometry;
UPDATE b01bvo SET geom=st_setsrid(st_makepoint(x*1000,y*1000),21781);
-- create backup of b01 BV
CREATE TABLE b01bv AS (SELECT * FROM b01);

-- delete duplicates
DELETE FROM b01bvo
WHERE betriebid in (  
  select betriebid
  from b01bvo
  group by betriebid
  having count(*)>1)
AND betriebid||coalesce(name,'')||coalesce(vorname,'') not in (select name from (select betriebid, max(betriebid||coalesce(name,'')||coalesce(vorname,'')) as name
  from b01bvo
  group by betriebid
  having count(*)>1) a );

-- merge bv and bvo table
INSERT INTO b01 (
	SELECT a.* 
	FROM b01bvo a LEFT JOIN b01bv b
	ON a.betriebid=b.betriebid 
	WHERE b.betriebid is null
	);
	
VACUUM ANALYZE b01;


--k01
------
-- create backup of k01 BV
CREATE TABLE k01bv AS (SELECT * FROM k01);

--Change date to date format
alter table k01bvo alter column geburt_datum type date using to_date(geburt_datum::text,'YYYYMMDD');
alter table k01bvo alter column abgangdatum type date using to_date(abgangdatum::text,'YYYYMMDD');
alter table k01bvo alter column zugangdatum type date using to_date(zugangdatum::text,'YYYYMMDD');

-- delete duplicates
alter table k01bvo add column oid serial ;
DELETE FROM k01bvo
WHERE oid IN (SELECT oid
              FROM (SELECT oid,
                             ROW_NUMBER() OVER (partition BY tier_id, geburt_datum, tier_name, mutter_id, vater_id ORDER BY oid) AS rnum
                     FROM k01bvo) t
              WHERE t.rnum > 1);
			  
-- create betrieb when betriebid not in b01 to comply to foreign key constraint
INSERT INTO b01(betriebid) (SELECT DISTINCT a.betriebid
FROM k01bvo a LEFT JOIN b01 b ON a.betriebid=b.betriebid
WHERE b.betriebid is null
AND a.betriebid is not null);

-- merge bv and bvo table
INSERT INTO k01 (
	SELECT a.* 
	FROM k01bvo a LEFT JOIN k01bv b
	ON a.tier_id=b.tier_id 
	WHERE b.tier_id is null
	);

vacuum analyze k01;

--k04
-----
-- create backup of k04 BV
CREATE TABLE k04bv AS (SELECT * FROM k04);

--Change date to date format
alter table k04bvo alter column kalbedt type date using to_date(kalbedt::text,'YYYYMMDD');

-- create cow when tier_id not in k01 to comply to foreign key constraint
INSERT INTO k01(tier_id) (SELECT DISTINCT a.tier_id
FROM k04bvo a LEFT JOIN k01 b ON a.tier_id=b.tier_id
WHERE b.tier_id is null); --0 animals added

-- merge bv and bvo table
INSERT INTO k04 (
	SELECT a.* 
	FROM k04bvo a LEFT JOIN k04bv b
	ON a.tier_id=b.tier_id and a.laktation_nr=b.laktation_nr and a.abschlussart=b.abschlussart
	WHERE b.tier_id is null and b.laktation_nr is null and b.abschlussart is null
	);
	
vacuum analyze k04;

--k07
-----
-- create backup of k07 BV
CREATE TABLE k07bv AS (SELECT * FROM k07);

-- create cow when tier_id not in k01 to comply to foreign key constraint
INSERT INTO k01(tier_id) (SELECT DISTINCT a.tier_id
FROM k07bvo a LEFT JOIN k01 b ON a.tier_id=b.tier_id
WHERE b.tier_id is null); --0 animals added

-- merge bv and bvo table
INSERT INTO k07 (
	SELECT a.* 
	FROM k07bvo a LEFT JOIN k07bv b
	ON a.tier_id=b.tier_id and a.datum_beschreibung=b.datum_beschreibung
	WHERE b.tier_id is null and b.datum_beschreibung is null
	);

vacuum analyze k07;

--k10
-------
-- create backup of k10 BV
CREATE TABLE k10bv AS (SELECT * FROM k10);

--Change date to date format
alter table k10bvo alter column besamungsdt type date using to_date(besamungsdt::text,'YYYYMMDD');
alter table k10bvo alter column datum_abkalbung type date using to_date(datum_abkalbung::text,'YYYYMMDD');
alter table k10bvo alter column dt_vor_besamung type date using to_date(dt_vor_besamung::text,'YYYYMMDD');

-- merge bv and bvo table
INSERT INTO k10 (
	SELECT a.* 
	FROM k10bvo a LEFT JOIN k10bv b
	ON a.tier_id=b.tier_id and a.besamungsdt=b.besamungsdt and  a.stier_id=b.stier_id
	WHERE b.tier_id is null 
	);


--k11
--------
-- create backup of k11 BV
CREATE TABLE k11bv AS (SELECT * FROM k11);
-- merge bv and bvo table
INSERT INTO k11 (
	SELECT a.* 
	FROM k11bvo a LEFT JOIN k11bv b
	ON a.mutter_id=b.mutter_id and a.kalbedt=b.kalbedt and a.kalb_id=b.kalb_id
	WHERE b.mutter_id is null 
	);



--k33
------
-- create backup of k33 BV
CREATE TABLE k33bv AS (SELECT * FROM k33);
-- create betrieb when betriebid not in b01 to comply to foreign key constraint
INSERT INTO b01(betriebid) (SELECT DISTINCT a.betrieb_id_ort
FROM k33bvo a LEFT JOIN b01 b ON a.betrieb_id_ort=b.betriebid
WHERE b.betriebid is null);
-- create cow when tier_id not in k01 to comply to foreign key constraint
INSERT INTO k01(tier_id) (SELECT DISTINCT a.tier_id
FROM k33bvo a LEFT JOIN k01 b ON a.tier_id=b.tier_id
WHERE b.tier_id is null);

--Change date to date format
alter table k33bvo alter column kalbedatum type date using to_date(kalbedatum::text,'YYYYMMDD');
alter table k33bvo alter column dt_probe type date using to_date(dt_probe::text,'YYYYMMDD');

-- problème lors de l'import de la colonne rasse:
update k33bvo set tier_rasse=null;
update k33bvo set tier_rasse=k04bvo.tier_rasse from k04bvo where k33bvo.tier_id=k04bvo.tier_id;
-- merge bv and bvo table
INSERT INTO k33 (
	SELECT a.* 
	FROM k33bvo a LEFT JOIN k33bv b
	ON a.tier_id=b.tier_id and a.probe_nr=b.probe_nr and a.laktation_nr=b.laktation_nr and a.milch=b.milch
	WHERE b.tier_id is null 
	);

vacuum analyze k33;
