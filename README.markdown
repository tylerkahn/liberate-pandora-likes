Liberate Pandora Likes
======================

Usage
----------
Compile with `ghc --make Main`

This depends on quite a few libraries so hopefully it's not too painless to get those.

Run with `./Main <station_id>`

Results are printed to console in JSON format, sorted in descending order by date liked.

Example
-------------
    > ./Main 529228236775615423
  
    [{"name":"4 AM (Adam K & Soha Dub)","artist":"Kaskade","date":"08-14-2011"},
    {"name":"One Night In Tokyo (Dj Shah's Savannah Remix)","artist":"Purple Mood","date":"08-05-2011"},
    {"name":"Animus Vox","artist":"The Glitch Mob","date":"08-04-2011"},
    {"name":"Pitfield Girl","artist":"Phelps","date":"08-04-2011"}]

Todo
---------

- Sorting by ascending order is supported in the library, add a command line flag for it
- Likewise sorting by artist is supported, add a flag for that
- Maybe add ability to customize output format
- Also maybe throw this onto a server using Snap framework or something