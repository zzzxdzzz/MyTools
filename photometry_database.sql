BEGIN TRANSACTION;

-- Load spatialite library; initialize and load metadata; add targer info to spatial ref sys
.load 'libspatialite.so.5'
select InitSpatialMetaData();
.read set_target_spat_ref_sys.sql


-- Image data
CREATE TABLE Image (
  -- Create unique image identifier - one binary, other same source as point ids
  imageid INTEGER PRIMARY KEY,
  sourceproductid TEXT NOT NULL,

  -- File information
  file text not NULL,
  lines integer,
  samples integer,

  -- General information
  instrumentid text,
  targetname text,
  missionphasename text,
  starttime text,
  etstarttime real,
  exposuretime real,
  yeardoy integer,
 
  -- Spacecraft position in body fixed coordinates
  scposstatus text,
  scposx real,
  scposy real,
  scposz real,

  -- Spacecraft orientation in NAIF quaternion form
  scpntstatus text,
  q0 real,
  q1 real,
  q2 real,
  q3 real,

  UNIQUE (imageid, sourceproductid)
);

 -- Pixel photometry data
CREATE TABLE Pixels (
  -- Link source back to image
  imageid integer references Image( imageid ) ON DELETE CASCADE ON UPDATE CASCADE,

  line real,
  sample real,

  -- Body-fixed coordinate of surface intercept
  bfx real,
  bfy real,
  bfz real,

   -- emission and incidence is from the shape model
  phase  real,
  emisson real,
  incidence real,
  resolution real,
);

COMMIT;

