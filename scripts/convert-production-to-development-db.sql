DELETE
FROM content
WHERE id NOT IN
    (SELECT id
     FROM content
     WHERE hashId IN ('4rcn',
                      '100',
                      'h',
                      '100U',
                      'hdfm'));


DELETE
FROM flickr
WHERE contentId NOT IN
    (SELECT id
     FROM content
     WHERE hashId IN ('4rcn',
                      '100',
                      'h',
                      '100U',
                      'hdfm'));


DELETE
FROM image
WHERE contentId NOT IN
    (SELECT id
     FROM content
     WHERE hashId IN ('4rcn',
                      '100',
                      'h',
                      '100U',
                      'hdfm'));

 VACUUM;
