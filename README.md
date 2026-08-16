# Scripts Collection

Welcome to the `scripts` repository! This is a curated collection of scripts in Shell Script, Python, Ruby, and more to help you automate tasks and improve productivity.

## Contents

1. [About](#1-about)
2. [Installation](#2-installation)
3. [Usage](#3-usage)
4. [Testing](#4-testing)
5. [Directory Structure](#5-directory-structure)
6. [Contribution](#6-contribution)
7. [License](#7-license)

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

Every executable carries a header block stating what it does, how it is invoked
and what it expects, so the script itself is the reference. For where each kind
of script lives, see [Directory Structure](#5-directory-structure).

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

It exits with a non-zero status when any test fails.

A second layer runs nightly from `cron/bin/run_tests`, which drives
`run_tests.sh` once per configured Python and Ruby version and then applies the
repository-wide gates: the shell script validation (`test/check_scripts.sh`),
the header documentation check (`check_header_doc.py -a`), and the
compatibility check (`find_pycompat.py`). Checks that belong to the repository
as a whole, rather than to one interpreter version, are wired there instead of
into `run_tests.sh`.

---

## 5. Directory Structure

This section describes the main directories of the repository and what each one
is for. It is not a complete file listing: the top level alone holds roughly a
hundred scripts, and only the directories and the few files worth knowing about
up front are shown.

```
.
├── *.sh, *.py, *.rb          The scripts a user runs directly. One task per script.
├── setup_scripts.sh          Sets executable permissions across the repository.
├── run_tests.sh              Runs the test suite for one Python and Ruby pair.
├── installer/                Setup and installation scripts, for building a machine.
├── cron/
│   ├── bin/                  Scheduled job scripts, deployed as /etc/cron.exec.
│   └── etc/                  Their configuration files, deployed as /etc/cron.config.
├── etc/                      Configuration and data files read by the top-level scripts.
├── dot_files/                Dot files deployed into a user's home directory.
├── test/                     The test suite, plus check_scripts.sh for the shell scripts.
└── doc/
    ├── POLICY                Design and development standards for all languages.
    ├── VERSIONS              Version history of the repository.
    ├── LICENSE               License notice.
    ├── COPYING               GPL version 3 text.
    └── COPYING.LESSER        LGPL version 3 text.
```

Placement says what a file is for, not which rules apply to it: every executable
in the repository follows the same header, logging, CLI and exit code
conventions, wherever it lives. The distinction the directories draw is how a
script is invoked — by hand from the top level, once at setup time from
`installer/`, or unattended from `cron/bin/`.

That last one is why configuration is separated from code. A top-level script
that needs site-specific values reads them from `etc/<name>.conf`, and a
`cron/bin/` job reads its deployed copy under `/etc/cron.config/`, so no script
carries a hostname, a path or a credential in its body.

[doc/POLICY](doc/POLICY) is the authoritative version of this layout and of the
rules above; the summary here is only an orientation.

---

## 6. Contribution

We welcome contributions! Here's how you can help:
1. Fork the repository.
2. Add or improve a feature, or fix an issue.
3. Submit a pull request with clear documentation and changes.

Please ensure your code is well-structured and documented.

Every executable carries a structured header block stating what it is, who
wrote it, how it is driven, and what changed in each version. Read
[doc/POLICY](doc/POLICY) before adding one: it defines the sections that block
contains, when a version is bumped, and how configuration files, exit codes,
and logging are expected to behave.

### Implementation Policy

See [doc/POLICY](doc/POLICY) for detailed design and development standards across all supported languages, including Shell Script, Python, and Ruby implementations.

---

## 7. License

The parts of this repository copyrighted by id774 are dual licensed under the [GPL version 3](https://www.gnu.org/licenses/gpl-3.0.html) or the [LGPL version 3](https://www.gnu.org/licenses/lgpl-3.0.html), at your option.
For exclusions and full license details, please refer to the [LICENSE](doc/LICENSE) file.
See also [COPYING](doc/COPYING) and [COPYING.LESSER](doc/COPYING.LESSER) for the
complete license texts.

Thank you for using and contributing to this repository!
