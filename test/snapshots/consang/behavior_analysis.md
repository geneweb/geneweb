# Consang Behavioral Analysis Report

## Test Categories

### Help Behavior

Executed 3 test cases.

| Test | Exit Code | STDOUT | STDERR | Status |
|------|-----------|--------|--------|---------|
| -help | 0 | 346 | 0 | ✅ |
| --help | 0 | 346 | 0 | ✅ |
| -h | 2 | 0 | 392 | ⚠️ |

### Argument Validation

Executed 5 test cases.

| Test | Exit Code | STDOUT | STDERR | Status |
|------|-----------|--------|--------|---------|
| no_args | 2 | 0 | 46 | ⚠️ |
| invalid_db | 2 | 0 | 87 | ⚠️ |
| invalid_flag | 2 | 0 | 403 | ⚠️ |
| missing_db_after_flag | 2 | 0 | 46 | ⚠️ |
| multiple_flags | 2 | 0 | 89 | ⚠️ |

### Database Operations

Executed 3 test cases.

| Test | Exit Code | STDOUT | STDERR | Status |
|------|-----------|--------|--------|---------|
| empty_db_name | 2 | 0 | 46 | ⚠️ |
| nonexistent_db | 2 | 0 | 93 | ⚠️ |
| existing_test_db | 0 | 0 | 17 | ✅ |

### Flag Combinations

Executed 8 test cases.

| Test | Exit Code | STDOUT | STDERR | Status |
|------|-----------|--------|--------|---------|
| quiet_mode | 2 | 0 | 89 | ⚠️ |
| very_quiet_mode | 2 | 0 | 89 | ⚠️ |
| fast_mode | 2 | 0 | 89 | ⚠️ |
| scratch_mode | 2 | 0 | 89 | ⚠️ |
| memory_save_mode | 2 | 0 | 89 | ⚠️ |
| no_lock_mode | 2 | 0 | 89 | ⚠️ |
| quiet_fast | 2 | 0 | 89 | ⚠️ |
| very_quiet_mem | 2 | 0 | 89 | ⚠️ |

## Behavioral Patterns

### Help Exits Zero
Found 2 matching cases:
- -help
- --help

### Invalid Args Non Zero
Found 4 matching cases:
- invalid_db
- invalid_flag
- missing_db_after_flag
- nonexistent_db

### Missing Db Behavior
Found 2 matching cases:
- ('empty_db_name', 2, True)
- ('nonexistent_db', 2, True)

### Flag Compatibility
No matching cases found.

