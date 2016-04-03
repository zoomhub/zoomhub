-- Replace empty strings with NULL:
BEGIN TRANSACTION;
UPDATE content SET activeAt = NULL WHERE activeAt = '';
UPDATE content SET attributionLink = NULL WHERE attributionLink = '';
UPDATE content SET attributionText = NULL WHERE attributionText = '';
UPDATE content SET completedAt = NULL WHERE completedAt = '';
UPDATE content SET error = NULL WHERE error = '';
UPDATE content SET mime = NULL WHERE mime = '';
UPDATE content SET size = NULL WHERE size = '';
UPDATE content SET title = NULL WHERE title = '';

UPDATE flickr SET originalExtension = NULL WHERE originalExtension = '';
UPDATE flickr SET originalSecret = NULL WHERE originalSecret = '';
UPDATE flickr SET ownerRealName = NULL WHERE ownerRealName = '';
UPDATE flickr SET photoPageURL = NULL WHERE photoPageURL = '';
END TRANSACTION;

-- Normalize booleans:
BEGIN TRANSACTION;
UPDATE flickr SET isPublic = 1 WHERE isPublic = 'true';
UPDATE flickr SET isPublic = 0 WHERE isPublic = 'false';
END TRANSACTION;
