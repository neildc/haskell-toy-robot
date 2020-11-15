#/bin/bash

stack build

RUN="stack exec rea-toy-robot-exe"

function test {
    echo "> testing with $1"

    if cat $1 | $RUN | grep -q $2; then
        echo "   Passed"
    else
        echo "   Failed"
        echo "   expected: $2"
        echo "     actual: `cat $1 | $RUN`"
    fi
    echo ""
}

test test/exampleA '0,1,NORTH'
test test/exampleB '0,0,WEST'
test test/exampleC '3,3,NORTH'
test test/firstCommandMustBeValidPlace '2,3,NORTH'
