# This needs to be run with bash

set -e

TEST_FILES=*.test
TIMEOUT=200

passed=0
tot=0

temp_result=$(mktemp)

for test_file in $TEST_FILES; do
    t="${test_file%.*}"
    echo "Testing $t ..."
    make MODULE_NAME=$t clean
    tot=$(expr $tot + 1)
    if make MODULE_NAME=$t; then
        if [ "z$BUILD_ONLY" = "z" ]; then
            echo "Build success. Now to run test ..."
            make MODULE_NAME=$t run TIMEOUT=$TIMEOUT \
                | head -n -3 \
                | (grep -v 'Verilog $finish' || true) > $temp_result
            if diff $temp_result $test_file > $t.error; then
                # results are identical
                echo "Passed: $t"
                passed=$(expr $passed + 1)
            else
                echo "Failed: $t"
            fi
        else
            echo "Build success. Test skipped"
            passed=$(expr $passed + 1)
        fi
    else
        echo "Failed (build): $t"
    fi
done

echo "Testing stats = $passed/$tot"
