-- Drop legacy tables:
BEGIN TRANSACTION;
DROP TABLE ContentInfoGroup1;
DROP TABLE ContentInfoGroup2;
DROP TABLE FlickrPhotoInfo;
DROP TABLE ImageInfo;
END TRANSACTION;
