# This needs to be run with bash

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
    match_test_set
    enum_test
    test_multicycle
    branch_borrow
    queue
    test_ready
    cache
    arbiter
    arbiter2
    generate_test
    bug_stuck
    static_dyn_sync
    static_sync
    simple_usereg_pipeline
    AluExample
    general_recurse_assign
    general_recurse_simple
    test_reg_borrow
    try_recv_sync_pat
)

TIMEOUT=200

for t in ${TESTS[@]}; do
    make MODULE_NAME=$t clean
    make MODULE_NAME=$t
    make run MODULE_NAME=$t TIMEOUT=$TIMEOUT | head -n -3 | grep -v 'Verilog $finish' > $t.test
    make MODULE_NAME=$t clean
done
