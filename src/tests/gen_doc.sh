#!/bin/bash
pytest_report="./pytest_report.log"
test_names=""
test_desc=""
test_results=""
total=0

for file in "$@"; do
    # Extract function names, remove 'test_' prefix, and print
    test_names=$(grep -E '^\s*def\s+test_' "$file" | \
        sed -E 's/^\s*def\s+test_([a-zA-Z0-9_]+)\s*\(.*/\1/')
    # Extract docstrings
    test_desc=$(grep -E '^\s*def\s+test_' "$file" -A 2 | \
        grep -E '^\s*"""' | sed -E 's/^\s*"""(.*)""".*/\1/')
    # Count number of tests in the file
    total=$(echo "$test_names" | wc -l)

    classname=$(dirname "$file" | sed -E 's|/|.|g').$(basename "$file" .py)
    first_step=$(grep -E "<testcase classname=\"tests.$classname\" name=\"test_[a-zA-Z0-9_]+\"" "$pytest_report")
    failed_test_names=$(echo "$first_step" | sed -E 's/.*name="(test_[a-zA-Z0-9_]+)".*/\1/')
    for name in $test_names; do
        if echo "$failed_test_names" | sed -E 's/^test_//' | grep -q "^$name$"; then
            test_results+=("Failed")
        else
            test_results+=("Passed")
        fi
    done
done

# directory_name.file_name (no extension, no test_ prefix)
filename=$(dirname "$@").$(basename "$@" .py | sed -E 's/^test_//')
# echo "## Tests in $@ (total: $total)"
echo "# $filename (total: $total)"
echo "| name | description | result |"
echo "|------|-------------|--------|"


for i in $(seq 1 $total); do
    name=$(echo "$test_names" | sed -n "${i}p")
    desc=$(echo "$test_desc" | sed -n "${i}p")
    actual_result=$(echo "${test_results[i]}" | tr '[:upper:]' '[:lower:]')
    if [[ "$actual_result" == "passed" ]]; then
        actual_result="✅ Passed"
    elif [[ "$actual_result" == "failed" ]]; then
        actual_result="❌ Failed"
    else
        actual_result="❓ $actual_result"
    fi
    echo "| $name | $desc | $actual_result |"
done
