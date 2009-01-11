#!/usr/bin/awk -f

# Replace all bad characters (punctuation, space, etc) by underscores

# input: 
#   One file per line, for example the output of find. Important:
#   subdirectories must come before their files, this is the default behavior
#   of find
#
#   Caveat: don't include the trailing slash for find
#   DO
#   $ find pallo | sanitize.sh
#   DON'T
#   $ find pallo/ | sanitize.sh

# output:
#   Suitable for piping to sh.
#
# If you want to extend the list of bad characters, make sure you keep the two
# regexes in sync!

function sanitize(name)
{
    gsub(/[][)(><:, ']/, "_", name);
    return name;
}

BEGIN {
    # we want to juggle with filenames, set the field separators accordingly
    FS="/";
    OFS="/";
}

# only process filenames containing bad characters
$NF~/[][)(><:, ']/ {
    # Assume that all directories are already sanitized.
    #
    # The last field is the filename, all other fields are directories. Save
    # the filename to a variable and then set it to "". $0 then consists only
    # of the directory entries
    filename=$NF
    $NF="";

    # print the directories sanitized and the filename unsanitized, everything
    # in quotes
    sanitized_dirs=sanitize($0);
    # Problem: mv -i has to read from stdin which doesn't work. At least it
    # doesn't overwrite existing files.
    printf "mv -i \"" sanitized_dirs
    printf filename "\" ";

    # print everything sanitized
    printf sanitized_dirs
    print sanitize(filename);
}
