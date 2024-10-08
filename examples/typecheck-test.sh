set -e

should_pass_p=0
should_pass_t=0

should_fail_p=0
should_fail_t=0

for t in should_pass/*.anvil; do
    should_pass_t=$(expr $should_pass_t + 1)
    if dune exec anvil -- -verbose $t; then
        should_pass_p=$(expr $should_pass_p + 1)
        echo "####### Test Passed: $t"
    else
        echo "####### Test Failed: $t"
    fi
done

for t in should_fail/*.anvil; do
    should_fail_t=$(expr $should_fail_t + 1)
    if ! dune exec anvil -- -verbose $t; then
        should_fail_p=$(expr $should_fail_p + 1)
        echo "####### Test Passed: $t"
    else
        echo "####### Test Failed: $t"
    fi
done

echo "######### should_pass = $should_pass_p/$should_pass_t"
echo "######### should_fail = $should_fail_p/$should_fail_t"
