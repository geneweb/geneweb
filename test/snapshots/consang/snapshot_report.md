# Consang Reference Snapshots Report

Generated snapshots for 27 test cases.

## Test Cases Summary

| Test Name | Exit Code | STDOUT | STDERR | Description |
|-----------|-----------|--------|--------|--------------|
| help_display | 0 | 346 chars | 0 chars | Test help output consistency |
| help_display_long | 0 | 346 chars | 0 chars | Test long help option |
| help_display_short_invalid | 2 | 0 chars | 392 chars | Test short help option (should fail as -h is not r... |
| no_arguments_error | 2 | 0 chars | 46 chars | Test behavior with no arguments - should show miss... |
| nonexistent_database | 2 | 0 chars | 87 chars | Test with non-existent database file |
| quiet_mode_nonexistent | 2 | 0 chars | 87 chars | Test quiet mode with non-existent database |
| very_quiet_mode_nonexistent | 2 | 0 chars | 87 chars | Test very quiet mode with non-existent database |
| fast_mode_nonexistent | 2 | 0 chars | 87 chars | Test fast mode with non-existent database |
| from_scratch_nonexistent | 2 | 0 chars | 87 chars | Test scratch mode with non-existent database |
| save_memory_nonexistent | 2 | 0 chars | 87 chars | Test memory saving mode with non-existent database |
| no_lock_nonexistent | 2 | 0 chars | 87 chars | Test no lock mode with non-existent database |
| empty_database_name | 2 | 0 chars | 46 chars | Test with empty string as database name |
| combined_options_nonexistent | 2 | 0 chars | 87 chars | Test combination of options with non-existent data... |
| invalid_flag_with_db | 2 | 0 chars | 398 chars | Test invalid flag with database name |
| multiple_flags_nonexistent | 2 | 0 chars | 87 chars | Test multiple compatible flags with non-existent d... |
| test_database_normal_mode | 0 | 0 chars | 648 chars | Test consanguinity calculation with normal verbose... |
| test_database_quiet_mode | 0 | 0 chars | 17 chars | Test consanguinity calculation in quiet mode |
| test_database_very_quiet_mode | 0 | 0 chars | 0 chars | Test consanguinity calculation in very quiet mode |
| test_database_fast_mode | 0 | 0 chars | 17 chars | Test consanguinity calculation in fast mode |
| test_database_memory_save_mode | 0 | 0 chars | 17 chars | Test consanguinity calculation in memory saving mo... |
| test_database_no_lock_mode | 0 | 0 chars | 17 chars | Test consanguinity calculation without database lo... |
| test_database_from_scratch | 0 | 0 chars | 648 chars | Test consanguinity calculation from scratch (recal... |
| test_database_fast_quiet_combined | 0 | 0 chars | 17 chars | Test consanguinity calculation with fast and quiet... |
| test_database_mem_very_quiet_combined | 0 | 0 chars | 0 chars | Test consanguinity calculation with memory save an... |
| test_database_scratch_fast_combined | 0 | 0 chars | 648 chars | Test consanguinity calculation from scratch with f... |
| test_database_all_performance_flags | 0 | 0 chars | 17 chars | Test consanguinity calculation with all performanc... |
| test_database_complete_workflow | 0 | 0 chars | 502 chars | Test complete workflow: from scratch, fast, and qu... |

## Detailed Outputs

### help_display

**Command:** `distribution/gw/consang -help`

**Description:** Test help output consistency

**Exit Code:** 0

**STDOUT:**
```
usage: distribution/gw/consang [options] <file_name>
  -fast     faster, but use more memory
  -mem      Save memory, but slower when rewritting database
  -nolock   do not lock database.
  -q        quiet mode
  -qq       very quiet mode
  -scratch  from scratch
  -help     Display this list of options
  --help    Display this list of options

```

**STDERR:** (empty)

---

### help_display_long

**Command:** `distribution/gw/consang --help`

**Description:** Test long help option

**Exit Code:** 0

**STDOUT:**
```
usage: distribution/gw/consang [options] <file_name>
  -fast     faster, but use more memory
  -mem      Save memory, but slower when rewritting database
  -nolock   do not lock database.
  -q        quiet mode
  -qq       very quiet mode
  -scratch  from scratch
  -help     Display this list of options
  --help    Display this list of options

```

**STDERR:** (empty)

---

### help_display_short_invalid

**Command:** `distribution/gw/consang -h`

**Description:** Test short help option (should fail as -h is not recognized)

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
distribution/gw/consang: unknown option '-h'.
usage: distribution/gw/consang [options] <file_name>
  -fast     faster, but use more memory
  -mem      Save memory, but slower when rewritting database
  -nolock   do not lock database.
  -q        quiet mode
  -qq       very quiet mode
  -scratch  from scratch
  -help     Display this list of options
  --help    Display this list of options

```

---

### no_arguments_error

**Command:** `distribution/gw/consang`

**Description:** Test behavior with no arguments - should show missing filename error

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
Missing file name
Use option -help for usage@.
```

---

### nonexistent_database

**Command:** `distribution/gw/consang nonexistent_db`

**Description:** Test with non-existent database file

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
Fatal error: exception Sys_error("nonexistent_db.gwb/base: No such file or directory")

```

---

### quiet_mode_nonexistent

**Command:** `distribution/gw/consang -q nonexistent_db`

**Description:** Test quiet mode with non-existent database

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
Fatal error: exception Sys_error("nonexistent_db.gwb/base: No such file or directory")

```

---

### very_quiet_mode_nonexistent

**Command:** `distribution/gw/consang -qq nonexistent_db`

**Description:** Test very quiet mode with non-existent database

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
Fatal error: exception Sys_error("nonexistent_db.gwb/base: No such file or directory")

```

---

### fast_mode_nonexistent

**Command:** `distribution/gw/consang -fast nonexistent_db`

**Description:** Test fast mode with non-existent database

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
Fatal error: exception Sys_error("nonexistent_db.gwb/base: No such file or directory")

```

---

### from_scratch_nonexistent

**Command:** `distribution/gw/consang -scratch nonexistent_db`

**Description:** Test scratch mode with non-existent database

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
Fatal error: exception Sys_error("nonexistent_db.gwb/base: No such file or directory")

```

---

### save_memory_nonexistent

**Command:** `distribution/gw/consang -mem nonexistent_db`

**Description:** Test memory saving mode with non-existent database

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
Fatal error: exception Sys_error("nonexistent_db.gwb/base: No such file or directory")

```

---

### no_lock_nonexistent

**Command:** `distribution/gw/consang -nolock nonexistent_db`

**Description:** Test no lock mode with non-existent database

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
Fatal error: exception Sys_error("nonexistent_db.gwb/base: No such file or directory")

```

---

### empty_database_name

**Command:** `distribution/gw/consang `

**Description:** Test with empty string as database name

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
Missing file name
Use option -help for usage@.
```

---

### combined_options_nonexistent

**Command:** `distribution/gw/consang -q -fast nonexistent_db`

**Description:** Test combination of options with non-existent database

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
Fatal error: exception Sys_error("nonexistent_db.gwb/base: No such file or directory")

```

---

### invalid_flag_with_db

**Command:** `distribution/gw/consang -invalid test_db`

**Description:** Test invalid flag with database name

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
distribution/gw/consang: unknown option '-invalid'.
usage: distribution/gw/consang [options] <file_name>
  -fast     faster, but use more memory
  -mem      Save memory, but slower when rewritting database
  -nolock   do not lock database.
  -q        quiet mode
  -qq       very quiet mode
  -scratch  from scratch
  -help     Display this list of options
  --help    Display this list of options

```

---

### multiple_flags_nonexistent

**Command:** `distribution/gw/consang -qq -mem -fast nonexistent_db`

**Description:** Test multiple compatible flags with non-existent database

**Exit Code:** 2

**STDOUT:** (empty)

**STDERR:**
```
Fatal error: exception Sys_error("nonexistent_db.gwb/base: No such file or directory")

```

---

### test_database_normal_mode

**Command:** `distribution/gw/consang test-consang-db`

**Description:** Test consanguinity calculation with normal verbose output

**Exit Code:** 0

**STDOUT:** (empty)

**STDERR:**
```
To do: 12 persons
Computing consanguinity...     12     11     10
Max consanguinity 0 for Jean.0 MARTIN...       9      8      7      6      5      4      3      2
Max consanguinity 0.25 for Thomas.0 MARTIN...       1 done   
*** saving persons array
*** saving ascends array
*** saving unions array
*** saving families array
*** saving couples array
*** saving descends array
*** saving strings array
*** create nam
... (truncated)
```

---

### test_database_quiet_mode

**Command:** `distribution/gw/consang -q test-consang-db`

**Description:** Test consanguinity calculation in quiet mode

**Exit Code:** 0

**STDOUT:** (empty)

**STDERR:**
```
To do: 0 persons

```

---

### test_database_very_quiet_mode

**Command:** `distribution/gw/consang -qq test-consang-db`

**Description:** Test consanguinity calculation in very quiet mode

**Exit Code:** 0

**STDOUT:** (empty)

**STDERR:** (empty)

---

### test_database_fast_mode

**Command:** `distribution/gw/consang -fast test-consang-db`

**Description:** Test consanguinity calculation in fast mode

**Exit Code:** 0

**STDOUT:** (empty)

**STDERR:**
```
To do: 0 persons

```

---

### test_database_memory_save_mode

**Command:** `distribution/gw/consang -mem test-consang-db`

**Description:** Test consanguinity calculation in memory saving mode

**Exit Code:** 0

**STDOUT:** (empty)

**STDERR:**
```
To do: 0 persons

```

---

### test_database_no_lock_mode

**Command:** `distribution/gw/consang -nolock test-consang-db`

**Description:** Test consanguinity calculation without database locking

**Exit Code:** 0

**STDOUT:** (empty)

**STDERR:**
```
To do: 0 persons

```

---

### test_database_from_scratch

**Command:** `distribution/gw/consang -scratch test-consang-db`

**Description:** Test consanguinity calculation from scratch (recalculate all)

**Exit Code:** 0

**STDOUT:** (empty)

**STDERR:**
```
To do: 12 persons
Computing consanguinity...     12     11     10
Max consanguinity 0 for Jean.0 MARTIN...       9      8      7      6      5      4      3      2
Max consanguinity 0.25 for Thomas.0 MARTIN...       1 done   
*** saving persons array
*** saving ascends array
*** saving unions array
*** saving families array
*** saving couples array
*** saving descends array
*** saving strings array
*** create nam
... (truncated)
```

---

### test_database_fast_quiet_combined

**Command:** `distribution/gw/consang -fast -q test-consang-db`

**Description:** Test consanguinity calculation with fast and quiet modes combined

**Exit Code:** 0

**STDOUT:** (empty)

**STDERR:**
```
To do: 0 persons

```

---

### test_database_mem_very_quiet_combined

**Command:** `distribution/gw/consang -mem -qq test-consang-db`

**Description:** Test consanguinity calculation with memory save and very quiet modes

**Exit Code:** 0

**STDOUT:** (empty)

**STDERR:** (empty)

---

### test_database_scratch_fast_combined

**Command:** `distribution/gw/consang -scratch -fast test-consang-db`

**Description:** Test consanguinity calculation from scratch with fast mode

**Exit Code:** 0

**STDOUT:** (empty)

**STDERR:**
```
To do: 12 persons
Computing consanguinity...     12     11     10
Max consanguinity 0 for Jean.0 MARTIN...       9      8      7      6      5      4      3      2
Max consanguinity 0.25 for Thomas.0 MARTIN...       1 done   
*** saving persons array
*** saving ascends array
*** saving unions array
*** saving families array
*** saving couples array
*** saving descends array
*** saving strings array
*** create nam
... (truncated)
```

---

### test_database_all_performance_flags

**Command:** `distribution/gw/consang -fast -mem -nolock test-consang-db`

**Description:** Test consanguinity calculation with all performance flags

**Exit Code:** 0

**STDOUT:** (empty)

**STDERR:**
```
To do: 0 persons

```

---

### test_database_complete_workflow

**Command:** `distribution/gw/consang -scratch -fast -q test-consang-db`

**Description:** Test complete workflow: from scratch, fast, and quiet

**Exit Code:** 0

**STDOUT:** (empty)

**STDERR:**
```
To do: 12 persons
............................................................
#####|#####|#####|#####|#####|#####|#####|#####|#####|#####|#####|##### 
*** saving persons array
*** saving ascends array
*** saving unions array
*** saving families array
*** saving couples array
*** saving descends array
*** saving strings array
*** create name index
*** create strings of sname
*** create strings of fname
*** create string index
*** create surname index
*** create first name index
*** o
... (truncated)
```

---

