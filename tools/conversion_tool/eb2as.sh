#!/bin/sh

curr=0
file=0
count=0

CLEAN=0
TEST=0

if [ "$1" != "--dev" ]
then
    $(dirname $0)/cleanup.sh ../
    CLEAN=1
else
    shift
fi

if [ "$1" = "--test" ]
then
    TEST=1
    shift
fi

if [ "$1" = "--xlc" ]
then
    COMPILER="xlclang"
    shift
else
    COMPILER="njsc"
fi

#find and remove any potential propositional arguments that could
#prevent the pre-processing step from producing a .u header file
find=$(echo $@ | sed -E 's/.+ (-c) .+/\1/')
if [ "$find" = "-c" ]
then
    first=$(echo $@ | sed -E 's/(.+ )-c( .+)/\1\2/')
    deleted[0]=$(echo $@ | sed -E 's/.+ (-c) .+/\1/')
    new=$(echo $first | sed -E 's/(.+ )-MF [A-Z0-9a-z/\._-]+( .+)/\1\2/')
    deleted[1]=$(echo $first | sed -E 's/.+ (-MF [A-Z0-9a-z/\._-]+) .+/\1/')
else
    new=$@
fi

#iterate over each .c, .cc, .cpp file that's been given and call the convereter
CFLAG=0
count=0
compiled=0
for var in $new
do
    fileend=$(echo $var | sed -E 's/.+(\.[a-zA-Z0-9]+)/\1/')
    if [ $fileend = ".cc" ] || [ $fileend = ".cpp" ] || [ $fileend = ".c" ] || [ $fileend = ".cxx" ]
    then
        if [ $compiled = 0 ]
        then
            if [ $fileend = ".c" ]
            then
                compiled=1
                CFLAG=1
                $COMPILER -E $new > /dev/null
            else
                compiled=1
                $COMPILER++ -E $new > /dev/null
            fi
        fi
        HEADER=$(echo $var | sed -E 's/.*\/([A-Za-z0-9_\-\.]+)\.[a-zA-Z0-9]+/\1.u/')
        TEMP=$(echo $var | sed -E 's/(.+)\.([A-Z0-9a-z]+)/\1_temp.\2/')
        if [ $TEST -eq 1 ]
        then
            python $(dirname $0)/ebcdic2ascii.py -H $HEADER $var $TEMP
        else
            INCLUDE=$(node -p process.execPath)
            python $(dirname $0)/ebcdic2ascii.py -H $HEADER $var $TEMP -I $INCLUDE
        fi
        COMPILE[count]=$TEMP
        count=$((count+1))
    else
        COMPILE[count]=$var
        count=$((count+1))
    fi
done

# compile using the temp file that has been converted into ascii
if [ $CFLAG = 1 ]
then
    $COMPILER ${COMPILE[*]} ${deleted[*]}
else
    $COMPILER++ ${COMPILE[*]} ${deleted[*]}
fi

# get rid of all files created
if [ $CLEAN -eq 1 ]
then
    $(dirname $0)/cleanup.sh ../
fi
