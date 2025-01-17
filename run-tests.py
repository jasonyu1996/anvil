import subprocess, pathlib, sys


TEST_FOLDER = pathlib.Path("./examples")

# type-check tests
p = subprocess.run(["sh", "typecheck-test.sh"], cwd=TEST_FOLDER)
typecheck_failed = p.returncode != 0

# simulation tests
p = subprocess.run(["bash", "test-all.sh"], cwd=TEST_FOLDER)
simulation_failed = p.returncode != 0


def to_verdict(failed):
    return "Failed" if failed else "Passed"

print(f"Typechecking tests: {to_verdict(typecheck_failed)}", file=sys.stderr)
print(f"Simulation tests  : {to_verdict(simulation_failed)}", file=sys.stderr)

