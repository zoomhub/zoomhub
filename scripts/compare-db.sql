ATTACH 'content-production.sqlite3' AS DB1;
ATTACH 'zoomhub.sqlite3' AS DB2;
SELECT * FROM DB1.content WHERE DB1.content.hashId NOT IN (SELECT DB2.content.hashId FROM DB2.content);
