# Scripts Collection

Welcome to the `scripts` repository! This is a curated collection of scripts in Shell Script, Python, Ruby, and more to help you automate tasks and improve productivity.

## Contents

1. [About](#1-about)
2. [Installation](#2-installation)
3. [Usage](#3-usage)
4. [Testing](#4-testing)
5. [Contribution](#5-contribution)
6. [License](#6-license)

---

## 1. About

This repository contains various utility scripts to address common tasks efficiently. Whether you are an experienced developer or just getting started, you'll find tools to optimize your workflow.

Supported languages:

- Shell Script
- Python
- Ruby

The scripts are fully supported on Python 3.6+ and Ruby 2.4+, with partial compatibility extending back to Python 3.1 and Ruby 2.0. The repository is also tested against current stable versions of both languages.

---

## 2. Installation

### Clone the Repository

Run the following commands to clone and navigate into the repository:

```bash
git clone https://github.com/id774/scripts.git
cd scripts
```

### Set Up Environment

To simplify usage, set the `$SCRIPTS` environment variable:

```bash
export SCRIPTS='/path/to/scripts'
```

Add this line to your `.bashrc` or `.zshrc` to persist the configuration.

### Grant Permissions

Ensure all scripts have the correct permissions:

```bash
./setup_scripts.sh
```

---

## 3. Usage

Refer to the comments at the beginning of each script for usage instructions. Example:

```bash
$SCRIPTS/example_script.sh
```

---

## 4. Testing

To validate the repository's integrity, run:

```bash
./run_tests.sh
```

This script will:
- Verify the presence of Python and Ruby.
- Run all test files in the `test` directory.
- Output paths and versions for troubleshooting.

---

## 5. Contribution

We welcome contributions! Here's how you can help:
1. Fork the repository.
2. Add or improve a feature, or fix an issue.
3. Submit a pull request with clear documentation and changes.

Please ensure your code is well-structured and documented.

### Implementation Policy

See [doc/POLICY](doc/POLICY) for detailed design and development standards across all supported languages, including Shell Script, Python, and Ruby implementations.

---

## 6. License

This repository is dual licensed under the [GPL version 3](https://www.gnu.org/licenses/gpl-3.0.html) or the [LGPL version 3](https://www.gnu.org/licenses/lgpl-3.0.html), at your option.
For full details, please refer to the [LICENSE](doc/LICENSE) file.  See also [COPYING](doc/COPYING) and [COPYING.LESSER](doc/COPYING.LESSER) for the complete license texts.

Thank you for using and contributing to this repository!
