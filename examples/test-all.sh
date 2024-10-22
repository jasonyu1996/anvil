# This needs to be run with bash

set -e

TESTS=(
    RCATop
    concurrent
    subreg
    subreg_var
    extern_proc
    extern_proc2
    assign_repeat
    counter2
    import_example
    multi_send_recv
    latency
)

passed=0
tot=0

for t in ${TESTS[@]}; do
    echo "Testing $t ..."
    make MODULE_NAME=$t clean
    tot=$(expr $tot + 1)
    if make MODULE_NAME=$t; then
        echo "Passed: $t"
        passed=$(expr $passed + 1)
    else
        echo "Failed: $t"
    fi
done

echo "Testing stats = $passed/$tot"
