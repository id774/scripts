# Scripts Collection

Welcome to the `scripts` repository, a curated collection of useful scripts to enhance your workflow and productivity. Ranging from simple utility scripts to more complex automation tasks, this repository aims to provide a wide array of tools for various use cases.

## Contents

- [About](#about)
- [Installation](#installation)
- [Usage](#usage)
- [Testing](#testing)
- [Contribution](#contribution)
- [License](#license)
- [Contact](#contact)

## About

This repository contains scripts in various languages like Shell, Python, Ruby, and more. Each script is designed to perform a specific task efficiently, saving you time and effort in your daily routines or complex projects.

## Installation

To use these scripts, first clone the repository:

``` html
git clone https://github.com/id774/scripts.git
cd scripts
```

We recommend setting the `$SCRIPTS` environment variable to the path of your scripts collection for easy access. You can do this by adding the following line to your .bashrc, .zshrc, or equivalent:

``` html
export SCRIPTS='/path/to/scripts'
```

Run setup_scripts.sh to set appropriate file permissions:

``` html
./setup_scripts.sh
```

For detailed instructions on how to use each script, please refer to the documentation within each script itself.

For Python scripts within this collection, we recommend using Python version 3.6 or later to ensure compatibility and take advantage of the latest language features.

## Usage

Refer to the contents of the script you wish to use in an editor and review the code and its documentation. All scripts are executable from the command line, but some may require specific permissions or environment setup.

## Testing

To run all tests in the `test` subdirectory, execute:

``` html
./run_tests.sh
```

This script checks for the presence of Python and Ruby, and executes all Python and Ruby test files located in the `test` subdirectory. It displays the paths and versions of Python and Ruby being used.

## Contribution

Contributions are welcome! If you have a useful script that you'd like to share, feel free to open a pull request. Please ensure that the script is well-written and readable, serving as its own documentation.

## License

All scripts in this repository are released under the [GNU Lesser General Public License v3 (LGPLv3)](https://www.gnu.org/licenses/lgpl-3.0.html). By contributing to this repository, you agree that your contributions will be licensed under its LGPLv3 License.


## Contact

For any questions or suggestions, please open an issue in this repository or contact the maintainer at [idnanashi@gmail.com](mailto:idnanashi@gmail.com).

