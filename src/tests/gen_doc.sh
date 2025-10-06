#!/bin/bash
pytest_report="./pytest_report.log"
test_names=""
test_desc=""
test_results=""
total=0
file="$1"
# for file in "$@"; do

# Count number of tests in the file
total=0
passed=0

classname=$(dirname "$file" | sed -E 's|/|.|g').$(basename "$file" .py)
test_cases=$(grep -oE "<testcase classname=\"tests.$classname\"[^>]+>" "$pytest_report")
# Successfull tests never have a child element, failed ones do
test_names=$(echo "$test_cases" | grep -oE 'name="test_[a-zA-Z0-9_]+"' | sed -E 's/.*name="(test_[a-zA-Z0-9_]+)".*/\1/' | sed -E 's/^test_//')
failed_test_names=$(echo "$test_cases" | grep -oE "<[^/>]+>" | sed -E 's/.*name="(test_[a-zA-Z0-9_]+)".*/\1/' | sed -E 's/^test_//')
for name in $test_names; do
    if echo "$failed_test_names" | grep -qw "$name"; then
        test_results+=("Failed")
    else
        test_results+=("Passed")
        passed=$((passed + 1))
    fi
    total=$((total + 1))
done

# Extract docstrings
test_desc=""
for name in $test_names; do
    desc=$(grep -E "^\s*def\s+test_$name\s*\(.*\):" -A 2 "$file" | \
        grep -E '^\s*"""' | sed -E 's/^\s*"""(.*)""".*/\1/')
    test_desc+="$desc"$'\n'
done
# done

# directory_name.file_name (no extension, no test_ prefix)
filename=$(dirname "$@").$(basename "$@" .py | sed -E 's/^test_//')
# echo "## Tests in $@ (total: $total)"
echo "# $filename ($suite_time s)"
echo "> [!INFO] failed: $suite_failures, errors: $suite_errors, skipped: $suite_skipped"
echo ""
echo "| name | description | result ($passed/$total) |"
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
