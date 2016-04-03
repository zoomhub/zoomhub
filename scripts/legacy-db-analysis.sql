--SELECT COUNT(*), Error FROM ContentInfo1 WHERE (Error LIKE "video/%" OR Error LIKE "application/%" OR Error LIKE "image/%" OR Error LIKE "text/%") AND Mime == "" GROUP BY Error ORDER BY COUNT(*) DESC;
--SELECT COUNT(*), Error FROM ContentInfo1 WHERE Error LIKE "%/%" AND Mime == "" GROUP BY Error ORDER BY COUNT(*) DESC;
--SELECT COUNT(*), Error FROM ContentInfo1 GROUP BY Error ORDER BY COUNT(*) DESC;x``
--UPDATE ContentInfo1 SET Error = "", Mime = Error WHERE (Error LIKE "video/%" OR Error LIKE "application/%") AND Mime == "";
--SELECT [#Attributes PartitionKey] AS Att, RowKey FROM ContentInfoGroup1 WHERE Att != RowKey;
--SELECT [#Attributes PartitionKey] AS Att, RowKey FROM ContentInfoGroup2 WHERE Att != RowKey;
--SELECT COUNT(*), Version FROM ContentInfo1 GROUP BY Version ORDER BY COUNT(*) DESC;
--SELECT Id, RowKey FROM ContentInfoGroup1 WHERE Id <> RowKey;
--SELECT COUNT(*) FROM FlickrPhotoInfo WHERE OwnerNsid = '72389028@N00';
--SELECT COUNT(*),AbuseLevel FROM ContentInfoGroup1 GROUP BY AbuseLevel ORDER BY COUNT(*) DESC;
--SELECT AbuseLevel, Url FROM ContentInfoGroup1 WHERE AbuseLevel >= 0 ORDER BY AbuseLevel DESC;
--SELECT COUNT(*), NumAbuseReports, Url FROM ContentInfoGroup1 GROUP BY NumAbuseReports ORDER BY COUNT(*) DESC;
--SELECT COUNT(*), Stage FROM ContentInfoGroup1 GROUP BY Stage ORDER BY COUNT(*) DESC;
--SELECT COUNT(*), Version FROM ContentInfoGroup1 GROUP BY Version ORDER BY COUNT(*) DESC;

--SELECT COUNT(*), TileFormat FROM ImageInfo GROUP BY TileFormat;
--SELECT COUNT(*), FarmId FROM FlickrPhotoInfo GROUP BY FarmId;
--SELECT COUNT(*), IsPublic FROM FlickrPhotoInfo GROUP BY IsPublic;
--SELECT COUNT(*), License FROM FlickrPhotoInfo GROUP BY License;
--SELECT COUNT(*), OriginalExtension FROM FlickrPhotoInfo GROUP BY OriginalExtension;
--SELECT COUNT(*), OriginalSecret FROM FlickrPhotoInfo GROUP BY OriginalSecret;
--SELECT COUNT(*), OwnerNsid FROM FlickrPhotoInfo GROUP BY OwnerNsid;
--SELECT COUNT(*), OwnerRealName FROM FlickrPhotoInfo GROUP BY OwnerRealName;
--SELECT COUNT(*), OwnerUserName FROM FlickrPhotoInfo GROUP BY OwnerUserName;
--SELECT COUNT(*), Size FROM FlickrPhotoInfo GROUP BY Size;

SELECT COUNT(*), Type FROM ContentInfoGroup2 GROUP BY Type ORDER BY Type ASC;
SELECT * FROM ContentInfoGroup2 WHERE Type = 0;


SELECT
  COUNT(*),
  Id,
  Url AS OriginalURL,
  MIN(Timestamp) AS FirstCreated
FROM
  ContentInfoGroup1
GROUP BY
  Url
HAVING
  COUNT(*) > 1
ORDER BY
  COUNT(*) DESC;
/*
UPDATE
  ContentInfoGroup1
SET
  Url =(Url || '#' || Id)
WHERE
  Timestamp NOT IN (
    SELECT
      MIN(Timestamp)
    FROM
      ContentInfoGroup1
    GROUP BY
      Url
    HAVING
      COUNT(*) > 1
  )
  AND Url IN (
    SELECT
      Url
    FROM
      ContentInfoGroup1
    GROUP BY
      Url
    HAVING
      COUNT(*) > 1
  );
*/

--SELECT Id, Url, Timestamp FROM ContentInfoGroup1 WHERE Url='http://upload.wikimedia.org/wikipedia/commons/3/36/SeattleI5Skyline.jpg' ORDER BY Timestamp ASC LIMIT 1;
--SELECT Id, Url, Timestamp FROM ContentInfoGroup1 WHERE Url='http://www.americares.org/' ORDER BY Timestamp ASC LIMIT 1;
