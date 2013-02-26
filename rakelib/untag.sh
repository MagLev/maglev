#!/bin/bash

top="`cd $(dirname "$0"); cd ..; pwd`"

### Determine how to run timeout
if [ -z "$(which timeout 2>/dev/null)" ]; then
    if [ -n "$(which gtimeout 2>/dev/null)" ]; then
        TIMEOUT="gtimeout -s 9"
    else
        ################################################################################
        # Executes command with a timeout. From
        # http://unix.stackexchange.com/questions/43340/how-to-introduce-timeout-for-shell-scripting
        #
        # Params:
        #   $1 timeout in seconds
        #   rest - command
        # Returns 1 if timed out 0 otherwise
        timeout_func() {
            time=$1
            # start the command in a subshell to avoid problem with pipes
            # (spawn accepts one command)
            command_and_args="${@:2}"
            command="/bin/bash -c \"$command_and_args\""
            expect -c "set echo \"-noecho\"; set timeout $time; spawn -noecho $command; expect timeout { exit 1 } eof { exit 0 }"
            if [ $? = 1 ] ; then
                echo "Killed after ${time} seconds"
            fi
        }
        TIMEOUT="timeout_func"
    fi
else
    TIMEOUT="timeout -s 9"
fi

### Make sure we exit when pressing Ctrl+C
function control_c() {
    exit 1
}
trap control_c SIGINT

function untag_spec() {
    $TIMEOUT 15 spec/mspec/bin/mspec tag -t m --del fails -g fails -e "$1" "$2"
}

### Actually run
echo "Untagging tagged specs"
sleep 1
for i in `find spec/tags/ -name "*_tags.txt"`; do
    FILE="$i"
    SPECNAME="${FILE%_tags.txt}"_spec.rb
    SPECPATH="`echo "$SPECNAME" | sed 's#spec/tags/rubyspec/tags/#spec/rubyspec/#'`"
    SPECPATH="`echo "$SPECNAME" | sed 's#spec/tags/#spec/rubyspec/#'`"

    while read LINE
    do
	SPEC="${LINE##fails:*#}"
	echo "$SPEC"
	sleep 0.5
	untag_spec "$SPEC" "$SPECPATH" &
    done < "$FILE"
done

export FAILING_FILES="$(pwd)/failing_files.txt"

function tag_file() {
    FILE="$1"
    echo $FILE
    outfile="$$"
    $TIMEOUT 15 spec/mspec/bin/mspec tag -t m --add fails -G fails "$FILE" | tee "$outfile".txt
    grep "1 file, 0 examples, 0 expectations, 0 failures, 1 error" "$outfile".txt
    if [ $? -eq 0 ]; then
        # Specfile had an error during load
        echo "$FILE " > "$FAILING_FILES"
    fi
    rm -f "$outfile".txt
}

echo "Tagging failing specs, also see $FAILING_FILES afterwards"
sleep 1
# for i in `find spec/rubyspec/core spec/rubyspec/command_line spec/rubyspec/language spec/rubyspec/library -name "*_spec.rb"`; do
for i in `find spec/rubyspec/ -name "*_spec.rb"`; do
    sleep 0.5
    tag_file "$i" &
done

if [ -n "$FAILING_FILES" ]; then
    echo "These files failed to even load, make sure they are in ${top}/topaz.mspec"
    sleep 2
    echo $FAILING_FILES
fi
