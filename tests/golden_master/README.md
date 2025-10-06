# Golden Master Tests - Generic Framework

Generic Golden Master testing framework to **compare any two executables** with pytest, regardless of programming language.

## 🎯 Principle

Compares two executables (reference vs candidate) by:
1. Running the **reference version** to create golden masters
2. Running the **candidate version** with the same parameters  
3. Comparing **all outputs**: stdout, stderr, files, return codes

## ⚙️ Complete Configuration (JSON)

Everything is configurable in `test_config.json`:

```json
{
  "test_config": {
    "executables": {
      "reference": {
        "path": "path/to/old/binary",
        "type": "binary",
        "description": "Reference version"
      },
      "candidate": {
        "path": "path/to/new/script.py", 
        "type": "script",
        "interpreter": "python3",
        "description": "Candidate version"
      }
    },
    "environment": {
      "timeout_seconds": 30,
      "encoding": "utf-8",
      "env_vars": {"VAR": "value"}
    },
    "comparison": {
      "ignore_patterns": ["^# Generated.*"],
      "normalize_whitespace": true,
      "normalize_line_endings": true
    },
    "output_capture": {
      "stdout": true,
      "stderr": true, 
      "files": true,
      "return_code": true
    }
  },
  "test_cases": [
    {
      "name": "test_basic",
      "description": "Basic test",
      "input_files": ["input.txt"],
      "args": ["-option", "value"],
      "expected_output_files": ["*.out"],
      "working_dir": "test_basic"
    }
  ]
}
```

## 🚀 Usage

### Installation
```bash
make install       # Install pytest
make check-config  # Check configuration
make check-executables  # Check that binaries exist
```

### Creating Golden Masters
```bash
make update-golden  # Create all golden masters with reference
# or
make update-single TEST=test_name  # Single test
```

### Running Tests
```bash
make test          # All tests
make test-single TEST=test_name  # Specific test
make test-verbose  # Detailed mode
```

### With pytest directly
```bash
pytest -v golden_master.py::TestGenericGoldenMaster
pytest --update-golden golden_master.py  # Update
pytest -k "test_name" golden_master.py   # Specific test
```

### Direct CLI (without pytest)
```bash
python3 golden_master.py                 # All tests
python3 golden_master.py --update-golden # Update
python3 golden_master.py --test name     # Specific test
```

## 📋 Supported Executable Types

### Native binary
```json
"reference": {
  "path": "bin/my_program",
  "type": "binary"
}
```

### Script with interpreter
```json
"candidate": {
  "path": "src/my_script.py",
  "type": "script", 
  "interpreter": "python3"
}
```

## 🧪 Test Configuration

### Simple test
```json
{
  "name": "basic_test",
  "input_files": ["input.dat"],
  "args": ["-v", "--output", "result.txt"],
  "expected_output_files": ["result.txt"],
  "working_dir": "basic"
}
```

### Test with setup
```json
{
  "name": "with_setup",
  "input_files": ["data.in"],
  "args": ["-f"],
  "expected_output_files": ["*.out"],
  "setup_commands": [
    "touch existing_file.txt",
    "mkdir -p subdir"
  ]
}
```

### Test with full capture
```json
{
  "name": "full_capture",
  "args": ["--verbose"],
  "expected_output_files": ["*.log"],
  "capture_stdout": true,
  "capture_stderr": true
}
```

## 📊 Result Comparison

The framework automatically compares:
- **Return codes** (exit codes)
- **Stdout** (standard output)
- **Stderr** (errors)
- **Output files** (according to patterns)

### Configurable normalization:
- Pattern removal (timestamps, etc.)
- Whitespace/line ending normalization
- Case handling

## 🛠️ Useful Commands

```bash
make list-tests     # List all tests
make show-config    # Display configuration
make stats          # Statistics
make report         # HTML report
make clean          # Clean temporaries
make clean-golden   # Remove golden masters
```

## 🎯 Concrete Example: ged2gwb

Configuration to compare OCaml vs Python:

```json
{
  "executables": {
    "reference": {
      "path": "distribution/gw/ged2gwb",
      "type": "binary"
    },
    "candidate": {
      "path": "python/ged2gwb/ged2gwb_cli.py",
      "type": "script",
      "interpreter": "python3"
    }
  },
  "test_cases": [
    {
      "name": "convert_basic",
      "input_files": ["sample.ged"],
      "args": [],
      "expected_output_files": ["*.gw", "*.gwb"]
    },
    {
      "name": "lowercase_names",
      "input_files": ["sample.ged"], 
      "args": ["-lf"],
      "expected_output_files": ["*.gw"]
    }
  ]
}
```

## ✅ Advantages

1. **100% Generic** - Compare any executables
2. **Fully Configurable** - Everything in JSON
3. **Pytest Integrated** - Reports, parallelization, CI/CD
4. **Complete Comparison** - stdout, stderr, files, return codes
5. **Flexible Normalization** - Ignore patterns, whitespace, etc.
6. **Easy to Extend** - New tests = JSON addition

This framework can be reused for **any project** needing to compare two versions of a program!