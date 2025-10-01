# Consang Business Logic Analysis Report

This report analyzes the consanguinity calculation functionality.

## Performance Analysis

### Consanguinity Calculation

| Test | Exit Code | Execution Time | STDERR Lines | Status |
|------|-----------|----------------|--------------|--------|
| normal_mode | 0 | 0.010s | 1 | ✅ |
| quiet_mode | 0 | 0.009s | 1 | ✅ |
| very_quiet_mode | 0 | 0.010s | 0 | ✅ |
| fast_mode | 0 | 0.010s | 1 | ✅ |
| memory_save_mode | 0 | 0.010s | 1 | ✅ |
| no_lock_mode | 0 | 0.010s | 1 | ✅ |

### Scratch Mode Operations

| Test | Exit Code | Execution Time | STDERR Lines | Status |
|------|-----------|----------------|--------------|--------|
| scratch_normal | 0 | 0.016s | 18 | ✅ |
| scratch_quiet | 0 | 0.015s | 17 | ✅ |
| scratch_fast | 0 | 0.016s | 18 | ✅ |
| scratch_memory_save | 0 | 0.017s | 22 | ✅ |

### Performance Combinations

| Test | Exit Code | Execution Time | STDERR Lines | Status |
|------|-----------|----------------|--------------|--------|
| fast_quiet | 0 | 0.010s | 1 | ✅ |
| memory_very_quiet | 0 | 0.010s | 0 | ✅ |
| fast_memory | 0 | 0.010s | 1 | ✅ |
| fast_no_lock | 0 | 0.010s | 1 | ✅ |
| all_performance_flags | 0 | 0.009s | 1 | ✅ |
| complete_workflow | 0 | 0.015s | 17 | ✅ |

## Mode Comparisons

- ✓ scratch_quiet: 22.5% less verbose

## Key Findings

- **Total business logic tests**: 16
- **Successful executions**: 16
- **Success rate**: 100.0%
- **Average execution time**: 0.012s
- **Fastest execution**: 0.009s
- **Slowest execution**: 0.017s
