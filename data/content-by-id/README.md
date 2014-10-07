This is a directory for local data.

It contains our content info as flat JSON files, keyed by ID.
(Note: uppercase letters get an underscore prefix, to work around
case-insensitive filesystems.)

Example data follows.

`100.json`:

```json
{
    "id": "100",
    "url": "http://e.i.uol.com.br/outros/0907/090731cielao1.jpg",
    "ready": true,
    "failed": false,
    "progress": 1,
    "mime": "image/jpeg",
    "size": 33998,
    "dzi": {
        "width": 208,
        "height": 208,
        "tileSize": 254,
        "tileOverlap": 1,
        "tileFormat": "jpg"
    }
}
```

`100_U.json`:

```json
{
    "id": "100U",
    "url": "http://www.archdrawing.ireland.anglican.org/archive/files/a0630962f3164841684eea66b6552445.JPG",
    "ready": true,
    "failed": false,
    "progress": 1,
    "mime": "image/jpeg",
    "size": 202199,
    "dzi": {
        "width": 1824,
        "height": 1368,
        "tileSize": 254,
        "tileOverlap": 1,
        "tileFormat": "jpg"
    }
}
```
