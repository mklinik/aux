#!/usr/bin/awk -f

# Replace all bad characters (punctuation, space, quotes, etc) by underscores

# input: 
#   One file per line, for example the output of find. Important:
#   subdirectories must come before their files, this is the default behavior
#   of find
#
#   Caveat: don't include the trailing slash for find
#   DO
#   $ find hello | sanitize.sh
#   DON'T
#   $ find hello/ | sanitize.sh
#
# output:
#   Suitable for piping to sh.
#
# If you want to extend the list of bad characters, make sure you keep the
# regexes in sync!
#
# Single and double quotes are included twice in the regexes
# for not confusing the vim syntax highlighting :(

# replace all bad characters by underscores
function sanitize(name)
{
    gsub(/[][)(><:, ''""]/, "_", name);
    return name;
}

# escape all double quotes
function escape(name)
{
    gsub(/[""]/, "\\\"", name);
    return name;
}

BEGIN {
    # we want to juggle with filenames, set the field separators accordingly
    FS="/";
    OFS="/";
}

# only process filenames containing bad characters
$NF~/[][)(><:, ''""]/ {
    # Assume that all directories are already sanitized.
    #
    # The last field is the filename, all other fields are directories. Save
    # the filename to a variable and set it to "". $0 then consists only of the
    # directory entries
    filename=$NF
    $NF="";

    # print the directories sanitized and the filename unsanitized but escaped,
    # everything in quotes -- the original filename
    sanitized_dirs=sanitize($0);
    printf "mv -i \"" sanitized_dirs
    # we're using double quotes to specify the original filename: escape all
    # double quotes in the original filename
    printf escape(filename) "\" ";

    # print everything sanitized -- the target filename
    printf sanitized_dirs
    printf sanitize(filename) " </dev/tty\n";
}
