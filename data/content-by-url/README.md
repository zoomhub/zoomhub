This directory holds files that index content by URL. The content URL is hashed
using SHA-256 and then used to create index files which link to content file.

For development, we use symbolic links (`ln -s target link_name`) as they can be
checked into Git. In production we use hardlinks (`ln target link_name`) to save
inodes (see: http://stackoverflow.com/a/33639131).
