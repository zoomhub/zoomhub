INSERT INTO
  content (
    id,
    hash_id,
    type_id,
    url,
    state,
    initialized_at,
    active_at,
    completed_at,
    title,
    attribution_text,
    attribution_link,
    mime,
    size,
    error,
    progress,
    abuse_level_id,
    num_abuse_reports,
    num_views,
    version
  )
VALUES
  (
    1,
    '100',
    1,
    'http://e.i.uol.com.br/outros/0907/090731cielao1.jpg',
    'initialized',
    '2009-09-09 18:47:38.058-07',
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    'image/jpeg',
    33998,
    NULL,
    1,
    0,
    0,
    0,
    3
  );

INSERT INTO
  content (
    id,
    hash_id,
    type_id,
    url,
    state,
    initialized_at,
    active_at,
    completed_at,
    title,
    attribution_text,
    attribution_link,
    mime,
    size,
    error,
    progress,
    abuse_level_id,
    num_abuse_reports,
    num_views,
    version
  )
VALUES
  (
    2,
    'hdfm',
    1,
    'http://wdh.blob.core.windows.net/deepzoom-sources/hampi/HampiCB/02-Lakshmi,%20The%20Temple%20Elephant/09-Lakshmi%20and%20little%20girl.jpg',
    'completed:success',
    '2011-01-17 20:56:09.782-08',
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    'image/jpeg',
    15533083,
    NULL,
    1,
    0,
    0,
    0,
    4
  );

INSERT INTO
  content (
    id,
    hash_id,
    type_id,
    url,
    state,
    initialized_at,
    active_at,
    completed_at,
    title,
    attribution_text,
    attribution_link,
    mime,
    size,
    error,
    progress,
    abuse_level_id,
    num_abuse_reports,
    num_views,
    version
  )
VALUES
  (
    3,
    '100U',
    1,
    'http://www.archdrawing.ireland.anglican.org/archive/files/a0630962f3164841684eea66b6552445.JPG',
    'initialized',
    '2013-08-17 16:30:39.464-07',
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    'image/jpeg',
    202199,
    NULL,
    1,
    0,
    0,
    0,
    4
  );

INSERT INTO
  content (
    id,
    hash_id,
    type_id,
    url,
    state,
    initialized_at,
    active_at,
    completed_at,
    title,
    attribution_text,
    attribution_link,
    mime,
    size,
    error,
    progress,
    abuse_level_id,
    num_abuse_reports,
    num_views,
    version
  )
VALUES
  (
    4,
    '4rcn',
    1,
    'http://media.stenaline.com/media_SE/lalandia-map-zoomit/lalandia-map.jpg',
    'completed:success',
    '2014-04-11 13:41:21.717-07',
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    'image/jpeg',
    9115770,
    NULL,
    1,
    0,
    0,
    0,
    4
  );

INSERT INTO
  content (
    id,
    hash_id,
    type_id,
    url,
    state,
    initialized_at,
    active_at,
    completed_at,
    title,
    attribution_text,
    attribution_link,
    mime,
    size,
    error,
    progress,
    abuse_level_id,
    num_abuse_reports,
    num_views,
    version
  )
VALUES
  (
    5,
    'h',
    1,
    'http://upload.wikimedia.org/wikipedia/commons/3/36/SeattleI5Skyline.jpg#zoomhub=h',
    'completed:success',
    '2016-04-03 22:54:55-07',
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    'image/jpeg',
    3232686,
    NULL,
    1,
    2,
    37,
    0,
    3
  );

INSERT INTO
  content (
    id,
    hash_id,
    type_id,
    url,
    state,
    initialized_at,
    active_at,
    completed_at,
    title,
    attribution_text,
    attribution_link,
    mime,
    size,
    error,
    progress,
    abuse_level_id,
    num_abuse_reports,
    num_views,
    version
  )
VALUES
  (
    6,
    'L8wh',
    1,
    'http://eoimages.gsfc.nasa.gov/images/imagerecords/79000/79765/dnb_land_ocean_ice.2012.54000x27000_geo.tif',
    'completed:success',
    '2020-11-29 01:17:27.402332-08',
    NULL,
    '2020-11-29 01:55:55.096107-08',
    NULL,
    NULL,
    NULL,
    'image/tiff',
    402827836,
    NULL,
    1,
    0,
    0,
    29,
    4
  );

INSERT INTO
  image (
    content_id,
    created_at,
    width,
    height,
    tile_size,
    tile_overlap,
    tile_format
  )
VALUES
  (
    2,
    '2011-01-17 20:55:08.337606-08',
    4032,
    6048,
    254,
    1,
    'jpg'
  );

INSERT INTO
  image (
    content_id,
    created_at,
    width,
    height,
    tile_size,
    tile_overlap,
    tile_format
  )
VALUES
  (
    4,
    '2014-04-11 13:40:50.718786-07',
    5058,
    3750,
    254,
    1,
    'jpg'
  );

INSERT INTO
  image (
    content_id,
    created_at,
    width,
    height,
    tile_size,
    tile_overlap,
    tile_format
  )
VALUES
  (
    5,
    '2009-07-30 21:20:13.596581-07',
    4013,
    2405,
    254,
    1,
    'jpg'
  );

INSERT INTO
  image (
    content_id,
    created_at,
    width,
    height,
    tile_size,
    tile_overlap,
    tile_format
  )
VALUES
  (
    6,
    '2020-11-29 01:55:55.096107-08',
    54000,
    27000,
    254,
    1,
    'jpg'
  );
