-- Import `FlickrPhotoInfo` table:
BEGIN TRANSACTION;
INSERT INTO flickr (
    contentId
  , farmId
  , serverId
  , photoId
  , secret
  , sizeId
  , isPublic
  , licenseId
  , originalExtension
  , originalSecret
  , ownerNSID
  , ownerRealName
  , ownerUsername
  , photoPageURL
  )
  SELECT
    content.id
  , FlickrPhotoInfo.FarmId
  , FlickrPhotoInfo.ServerId
  , FlickrPhotoInfo.PhotoId
  , FlickrPhotoInfo.Secret
  , FlickrPhotoInfo.Size
  , FlickrPhotoInfo.IsPublic
  , FlickrPhotoInfo.License
  , FlickrPhotoInfo.OriginalExtension
  , FlickrPhotoInfo.OriginalSecret
  , FlickrPhotoInfo.OwnerNsid
  , FlickrPhotoInfo.OwnerRealName
  , FlickrPhotoInfo.OwnerUserName
  , FlickrPhotoInfo.PhotoPageUrl
  FROM content JOIN FlickrPhotoInfo ON content.hashId=FlickrPhotoInfo.Id;
END TRANSACTION;
