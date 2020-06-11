-- Stats before
select count(*) from (select distinct tier_id FROM k04) s; --num animals
select count(*) from (select tier_id from k04 group by tier_id) a join k07 on a.tier_id=k07.tier_id; --num known pheno
select count(*) from (select tier_id, laktation_nr from k04 group by tier_id, laktation_nr) s ; --num lactations
select count(*) from k33; -- num records
select count(*) from (select betrieb_id_ort from k33 where alp_hohe>0 group by betrieb_id_ort) s ; --num alps
select count(*) from (select betrieb_id_ort from k33 join b01 on k33.betrieb_id_ort=b01.betriebid where alp_hohe>0 and x is not null group by betrieb_id_ort) s ; --num alps
select count(*) from (select betrieb_id_ort from k33 join b01 on k33.betrieb_id_ort=b01.betriebid where alp_hohe>0 and x is not null group by betrieb_id_ort) s -- num alps, known location
select count(*) from (select betrieb_id_ort from k33 join b01 on k33.betrieb_id_ort=b01.betriebid where alp_hohe>0 and postleitzahl is not null group by betrieb_id_ort) s -- num alps, known plz



DROP MATERIALIZED VIEW IF EXISTS  k01f CASCADE;
DROP MATERIALIZED VIEW IF EXISTS k04f CASCADE;
DROP MATERIALIZED VIEW IF EXISTS k33f CASCADE;

--attention à l'ordre: k01, k04, k33
--k01
CREATE MATERIALIZED VIEW k01f AS
	(SELECT k01.* 
	FROM k01 
	LEFT JOIN (SELECT tier_id,kalbedt
		FROM k04 
		WHERE (abschlussart=1 OR abschlussart=2 or abschlussart=8) --to have only one row in k04 
		AND laktation_nr=1 
		) a ON k01.tier_id=a.tier_id -- for age of first calf (see kalbedt-geburt_datum)
	LEFT JOIN (SELECT tier_id, avg(verzogerungszeit) as avg_verzogzeit
		FROM 
			(SELECT tier_id, laktationsnummer, max(besamungsdt)-min(besamungsdt) as verzogerungszeit
			FROM k10 
			WHERE laktationsnummer<=3
			GROUP BY tier_id, laktationsnummer) sub
		GROUP by tier_id) b --removes cows whose avg delay 1-last insemination during the 3 first lactations > 100 (if present)
	ON k01.tier_id=b.tier_id
	WHERE ((kalbedt-geburt_datum)>(2*365) OR a.tier_id IS NULL)--first calf >2years 
	AND ((kalbedt-geburt_datum)<(4*365) OR a.tier_id IS NULL) --first calf  <4 years
	AND (k01.tier_rasse like 'BV' OR k01.tier_rasse like 'OB') --beed OB or BV
	AND (vater_rasse like 'BV' OR vater_rasse like 'OB' OR vater_rasse like 'BS') --breed parents
	AND (mutter_rasse like 'BV' OR mutter_rasse like 'OB' OR mutter_rasse like 'BS')
	AND (avg_verzogzeit<100 OR avg_verzogzeit is null)--removes cows whose avg delay 1-last insemination during the 3 first lactations > 100
	);


--k04
CREATE MATERIALIZED VIEW k04f AS
	(SELECT k04.* 
	FROM k04 
	JOIN k01f ON k01f.tier_id=k04.tier_id --discards animals removed from k01
	LEFT JOIN (SELECT tier_id, laktation_nr
		FROM k33
		WHERE (dt_probe-kalbedatum)<0 --discards lact for which >=1 records are taken before calving
		OR ((dt_probe-kalbedatum)>42 AND probe_nr=1) --discards lact where first record after 42th day
		GROUP BY tier_id, laktation_nr) b --! + b.tier_id is null
	ON k04.tier_id=b.tier_id AND k04.laktation_nr=b.laktation_nr
	LEFT JOIN k04 c ON k04.tier_id=c.tier_id AND k04.laktation_nr=(c.laktation_nr+1) and k04.abschlussart=c.abschlussart --for the interval between calving
	/*JOIN (select tier_id, laktation_nr, sum(case when alp_hohe>0 then 1 else 0 end) as num_probe_alp
		FROM k33
		GROUP BY tier_id, laktation_nr) d 
	ON k04.tier_id=d.tier_id AND k04.laktation_nr=d.laktation_nr --for at least one record in the alp*/ -- already in subquery e
	JOIN (SELECT tier_id, laktation_nr, max(alp_hohe) as max_alt
		FROM k33
		WHERE alp_hohe>=11
		GROUP BY tier_id, laktation_nr) e --calc maximum alp altitude
	ON k04.tier_id=e.tier_id AND k04.laktation_nr=e.laktation_nr	
	WHERE b.tier_id IS NULL --see subquery b
	AND k04.tage_milch>=270 --longer than 270 days
	AND k04.kalbedt<to_date('01.04.2015','dd.mm.yyyy') --calving before March 2015
	AND k04.kalbedt>to_date('31.07.2000','dd.mm.yyyy') --calving after August 2000
	AND k04.laktation_nr<=10 --10th or less lactation
	AND ((k04.kalbedt-c.kalbedt)>290 or c.kalbedt is null) -- interval between calving > 290
	--AND num_probe_alp>=1 --at least one record in the alp
	AND (extract(month from k04.kalbedt)<=4 or extract(month from k04.kalbedt)>=8) --calving month August-April
	AND (max_alt>=11 AND max_alt<=26) --Altitude between 1100m and 2600m
	);


	
--k33
CREATE MATERIALIZED VIEW k33f AS (
	SELECT k33.*, alp_probe 
	FROM k33 
	JOIN k04f ON k33.tier_id=k04f.tier_id AND k33.laktation_nr=k04f.laktation_nr --discard lactations not in k04f
	JOIN (SELECT tier_id, laktation_nr 
		FROM k33 
		GROUP BY tier_id, laktation_nr 
		HAVING min(dt_probe-kalbedatum)>=5) a --for lactation whose first record is after the 5th day (what we want)
	ON k33.tier_id=a.tier_id AND k33.laktation_nr=a.laktation_nr
	/* *** */
	LEFT JOIN (SELECT tier_id, laktation_nr, min(probe_nr) as min_alp 
		FROM k33 
		WHERE alp_hohe>=11 
		GROUP BY  tier_id, laktation_nr) min_alp 
	ON  min_alp.tier_id=k33.tier_id AND min_alp.laktation_nr=k33.laktation_nr
	LEFT JOIN (SELECT tier_id, laktation_nr, probe_nr, rank() over (PARTITION BY k33.tier_id, k33.laktation_nr ORDER BY k33.probe_nr) as alp_probe 
		FROM k33 
		WHERE alp_hohe>=11) alp
	ON alp.tier_id=k33.tier_id and alp.laktation_nr=k33.laktation_nr and alp.probe_nr=k33.probe_nr
	LEFT JOIN (SELECT b.tier_id, b.laktation_nr, min(b.probe_nr) as max_probe
		FROM
			(SELECT tier_id, laktation_nr, probe_nr, rank() over (PARTITION BY tier_id, laktation_nr ORDER BY probe_nr) as alp_probe
			FROM k33 
			WHERE alp_hohe>=11) b 
		JOIN 
		k33 on b.tier_id=k33.tier_id AND b.laktation_nr=k33.laktation_nr AND b.probe_nr=(k33.probe_nr+1)
		AND (k33.alp_hohe is null OR k33.alp_hohe=0) and alp_probe>1
		GROUP BY b.tier_id, b.laktation_nr) ai 
	ON ai.tier_id=k33.tier_id AND ai.laktation_nr=k33.laktation_nr	
	WHERE (k33.probe_nr<max_probe OR max_probe is null)
	/* *** */-- This block truncates the records before the second alp-stay
	AND (abschlussart=7 OR abschlussart=2) --to have only one row per lactation in k04 
	AND (dt_probe-kalbedatum)<=500 --to delete records taken after the 500th day in milk
	
	UNION ALL --should be faster than union and should not change the result
	
	SELECT  k33.satzart, k33.version, k33.betriebid,  k33.betriebidtvd,  k33.tier_id, k33.tier_rasse,  k33.tier_name,  k33.betrieb_id_ort,  k33.betriebtvd_id_ort,  k33.kalbedatum, k33.laktation_nr, 
	k33.probe_nr-1 as probe_nr, --when the first record is taken before the 5th day, shift probe_nr
	k33.dt_probe, k33.milch ,  k33.fett , k33.eiweiss , k33.laktose , k33.persitenz,  k33.zellzahl,  k33.milchharnstoff,  k33.bemerkung,  k33.alp_hohe,  k33.citrat,  k33.melkmethode,  k33.prufmethode,  k33.aceton,  k33.milch_morgen , k33.milch_abend , k33.fett_p , k33.eiweis_p , k33.code_w,  k33.code_labor,  k33.melkzt_morgen,  k33.melkzt_abend,  k33.anmeldung_mbk,  k33.anmeldung_lbe,  k33.kasein_p , k33.laufnummer,  k33.wagetyp,  k33.datenherkunft,
	alp_probe 
	FROM k33 
	JOIN k04f ON k33.tier_id=k04f.tier_id AND k33.laktation_nr=k04f.laktation_nr 
	JOIN (SELECT tier_id, laktation_nr 
		FROM k33 
		GROUP BY tier_id, laktation_nr 
		HAVING min(dt_probe-kalbedatum)<5) a
	ON k33.tier_id=a.tier_id AND k33.laktation_nr=a.laktation_nr
	/* *** */
	LEFT JOIN (SELECT tier_id, laktation_nr, min(probe_nr) as min_alp 
		FROM k33 
		WHERE alp_hohe>=11 
		GROUP BY  tier_id, laktation_nr) min_alp 
	ON  min_alp.tier_id=k33.tier_id AND min_alp.laktation_nr=k33.laktation_nr
	LEFT JOIN (SELECT tier_id, laktation_nr, probe_nr, rank() over (PARTITION BY k33.tier_id, k33.laktation_nr ORDER BY k33.probe_nr) as alp_probe 
		FROM k33 
		WHERE alp_hohe>=11) alp
	ON alp.tier_id=k33.tier_id and alp.laktation_nr=k33.laktation_nr and alp.probe_nr=k33.probe_nr
	LEFT JOIN (SELECT b.tier_id, b.laktation_nr, min(b.probe_nr) as max_probe
		FROM
			(SELECT tier_id, laktation_nr, probe_nr, rank() over (PARTITION BY tier_id, laktation_nr ORDER BY probe_nr) as alp_probe
			FROM k33 
			WHERE alp_hohe>=11) b 
		JOIN 
		k33 on b.tier_id=k33.tier_id AND b.laktation_nr=k33.laktation_nr AND b.probe_nr=(k33.probe_nr+1)
		AND (k33.alp_hohe is null OR k33.alp_hohe=0) and alp_probe>1
		GROUP BY b.tier_id, b.laktation_nr) ai 
	ON ai.tier_id=k33.tier_id AND ai.laktation_nr=k33.laktation_nr	
	WHERE (k33.probe_nr<max_probe OR max_probe is null)
	/* *** */-- This block truncates the records before the second alp-stay
	AND (dt_probe-kalbedatum)>=5 --only records taken after the 5th day
	AND (abschlussart=7 OR abschlussart=2) --to have only one row per lactation in k04 
	AND (dt_probe-kalbedatum)<=500); --to delete records taken after the 500th day in milk

-- Create indexes
CREATE INDEX ind_k01fbid ON k01f (betriebid);
CREATE INDEX ind_k01ftid ON k01f (tier_id);
vacuum analyze k01f;

CREATE INDEX ind_k04ftid ON k04f (tier_id);
CREATE INDEX ind_k04fln ON k04f (laktation_nr);
CREATE INDEX ind_k04faa ON k04f (abschlussart);
CREATE INDEX ind_k04fkd ON k04f (kalbedt);
vacuum analyze k04f;

CREATE INDEX ind_k33ftid ON k33f (tier_id);
CREATE INDEX ind_k33fln ON k33f (laktation_nr);
CREATE INDEX ind_k33fpn ON k33f (probe_nr);
CREATE INDEX ind_k33fbid ON k33f (betrieb_id_ort);
CREATE INDEX ind_k33fkdt ON k33f (kalbedatum);
CREATE INDEX ind_k33fpdt ON k33f (dt_probe);
CREATE INDEX ind_k33fah ON k33f (alp_hohe);
CREATE INDEX ind_k33fap ON k33f (alp_probe);
vacuum analyze k33f;

-- Stats after
select count(*) from (select distinct tier_id FROM k04f) s; --num animals
select count(*) from (select tier_id from k04f group by tier_id) a join (select * from k07 where laktation_nr=1) k07 on a.tier_id=k07.tier_id; --num known pheno
select count(*) from (select tier_id, laktation_nr from k04f group by tier_id, laktation_nr) s ; --num lactations
select count(*) from k33f; -- num records
select count(*) from (select betrieb_id_ort from k33f join b01 on k33f.betrieb_id_ort=b01.betriebid where alp_hohe>0 group by betrieb_id_ort) s ; --num alps
select count(*) from (select betrieb_id_ort from k33f join b01 on k33f.betrieb_id_ort=b01.betriebid where alp_hohe>0 and x is not null group by betrieb_id_ort) s -- num alps, known location
select count(*) from (select betrieb_id_ort from k33f join b01 on k33f.betrieb_id_ort=b01.betriebid where alp_hohe>0 and postleitzahl is not null group by betrieb_id_ort) s -- num alps, known plz

select count(*)
from 
(select tier_id, laktation_nr
from k33f
group by tier_id, laktation_nr
having max(alp_probe)>=3 and max(case when alp_probe=1 then probe_nr-1 else 0 end)>=4) s --num of lactation with 3 records in the alp and 4 records before



/*
--filter
--k04_full
CREATE TABLE k04_full AS (
SELECT * FROM k04 WHERE abschlussart=3
UNION
SELECT k04.* FROM k04 
JOIN 
(SELECT  tier_id, laktation_nr
FROM k04
GROUP BY tier_id, laktation_nr
HAVING COUNT(*)=1) sub ON k04.tier_id=sub.tier_id AND k04.laktation_nr=sub.laktation_nr
WHERE abschlussart=2
);
--k04 standardized
CREATE TABLE k04_standardized AS (
SELECT * FROM k04 WHERE abschlussart=2);

-- lactation <270days => ok (abschlussart 2 and 3)

-- lactation > x
DELETE FROM k04_full WHERE tage_milch>450;
DELETE FROM k33 

-- Year 2016
DELETE FROM k04_full WHERE kalbedt>to_date('20150901', 'YYYYMMDD')

-- interval calving
DELETE FROM k04_full a USING k04_full b
WHERE a.tier_id=b.tier_id 
AND b.kalbedt<a.kalbedt
AND a.laktation_nr=b.laktation_nr+1
AND (a.kalbedt  - b.kalbedt)<XXXX

--first last insemination
DELETE FROM k04_full
USING
(SELECT tier_id, laktationsnummer, MAX(besamungsdt)-MIN(besamungsdt) AS verzogerungszeit
FROM k10 
GROUP BY tier_id, laktationsnummer) sub
WHERE k04_full.tier_id=sub.tier_id
AND k04_full.laktation_nr=sub.laktation_nr -- do we remove the cow or only the lactation
AND verzogerungszeit>XXXX
*/