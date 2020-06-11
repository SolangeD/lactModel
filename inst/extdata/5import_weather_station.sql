--Coordinates and data are in separate files

--Relative humidity 
CREATE TABLE rh_data (stn varchar, date2 integer, ure200d0 double precision);
COPY rh_data FROM 'D:/ClimGen-CH/Data/Meteo/rh/import/rh_data.csv' WITH csv delimiter ';' header;
ALTER TABLE rh_data ALTER COLUMN date2 TYPE date USING to_date(date2::text,'YYYYMMDD');
CREATE INDEX ON rh_data USING btree(stn);

CREATE TABLE rh_stations (stn varchar, name2 varchar, parameter varchar, source varchar, x integer, y integer, altitude integer);
COPY rh_stations FROM 'D:/ClimGen-CH/Data/Meteo/rh/import/rh_stations.csv' WITH csv delimiter ';' header;
ALTER TABLE rh_stations ADD COLUMN geom Geometry('Point',21781);
UPDATE rh_stations SET geom=st_setsrid(st_makepoint(x,y),21781);
CREATE INDEX ON rh_stations USING btree(stn);
CREATE INDEX ON rh_stations USING gist(geom);

--Wind speed
CREATE TABLE ws_data (stn varchar, date2 integer, fkl010d0 double precision);
COPY ws_data FROM 'D:/ClimGen-CH/Data/Meteo/WS/import/ws_data.csv' WITH csv delimiter ';' header;
ALTER TABLE ws_data ALTER COLUMN date2 TYPE date USING to_date(date2::text,'YYYYMMDD');
CREATE INDEX ON ws_data USING btree(stn);

CREATE TABLE ws_stations (stn varchar, name2 varchar, parameter varchar, source varchar, x integer, y integer, altitude integer);
COPY ws_stations FROM 'D:/ClimGen-CH/Data/Meteo/ws/import/ws_stations.csv' WITH csv delimiter ';' header;
ALTER TABLE ws_stations ADD COLUMN geom Geometry('Point',21781);
UPDATE ws_stations SET geom=st_setsrid(st_makepoint(x,y),21781);
CREATE INDEX ON ws_stations USING btree(stn);
CREATE INDEX ON ws_stations USING gist(geom);