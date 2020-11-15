#/bin/bash

# test file format

# EXPECTED OUTPUT
# <BLANK LINE>
# Command 1
# Command 2
# Command 3
# ...
# Command N

stack build

RUN="stack exec rea-toy-robot-exe"

function test {
    EXPECTED=`head -n1 $1`

    # read from the 3rd line onwards
    COMMANDS="tail -n +3 $1"

    echo "> testing with $1"

    if $COMMANDS | $RUN | grep -q $EXPECTED; then
        echo "   Passed"
    else
        echo "   Failed"
        echo "   expected: $EXPECTED"
        echo "     actual: `$COMMANDS | $RUN`"
    fi
    echo ""
}

for file in ./test/e2e/*
do
    test "$file"
done
