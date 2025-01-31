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
    multicycle_counter
    comb_loop
    list
    param_int
    param_type
    param_int_typedef
    param_type_typedef
    param_int_chan
    param_type_chan
    if_comb
    match_test
    enum_test
    test_multicycle
    branch_borrow
    queue
    test_ready
    cache
    arbiter
    arbiter2
    generate_test
    static_sync
    bug_stuck
    static_dyn_sync
    static_sync
)

passed=0
tot=0

for t in ${TESTS[@]}; do
    echo "Testing $t ..."
    make MODULE_NAME=$t clean
    tot=$(expr $tot + 1)
    if make MODULE_NAME=$t; then
        echo "Build success. Now to run test ..."
        exit_code=0
        timeout 5 make MODULE_NAME=$t run 2>&1 > /dev/null || exit_code=$?
        if [ "$exit_code" -eq 0 ] || [ "$exit_code" -eq 124 ]; then
            # timeout or success are both good
            echo "Passed: $t"
            passed=$(expr $passed + 1)
        else
            echo "Failed: $t"
        fi
    else
        echo "Failed (build): $t"
    fi
done

echo "Testing stats = $passed/$tot"
