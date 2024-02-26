#!/usr/bin/env python

########################################################################
# dummy.py: Demonstration of Python 3.x features for compatibility check
#
#  Description:
#  This script is designed as a dummy Python file that incorporates various Python 3.x
#  features to test and demonstrate the functionality of the find_pycompat.py script.
#  It includes usage of f-strings, subprocess.run, async/await keywords, nonlocal statement,
#  yield from expression, matrix multiplication operator, type hints, and more. The script
#  serves as a test case to verify that the find_pycompat.py script can successfully identify
#  compatibility issues with older versions of Python.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2024-02-26
#       Initial release. Incorporates various Python 3.x features for compatibility testing.
#
#  Usage:
#  The dummy.py script is not intended to be executed for practical purposes. Instead, it
#  should be placed in a directory and used in conjunction with the find_pycompat.py script
#  to test the latter's ability to detect Python 3.x features.
#
#  Purpose and Significance:
#  The creation of this dummy script is to ensure the effectiveness and reliability of the
#  find_pycompat.py script in detecting Python 3.x specific features that may not be compatible
#  with earlier Python versions. It aids in the validation and demonstration of the find_pycompat.py
#  script's functionality in real-world scenarios.
#
########################################################################

import asyncio
import pathlib
import shutil
import subprocess

# Demonstrate the use of f-strings
name = "world"
print(f"Hello, {name}!")  # f-string for variable interpolation

# Demonstrate the use of subprocess.run and subprocess.DEVNULL
result = subprocess.run(["echo", "Hello World"], stdout=subprocess.DEVNULL)  # Run a subprocess

# Async function definition using async/await
async def hello_world():
    print("Hello, world!")  # Print statement inside async function

# Run the async function
asyncio.run(hello_world())  # Execute an asynchronous function

# Demonstrate the use of nonlocal keyword
def outer():
    x = "local"

    def inner():
        nonlocal x  # Declare x as nonlocal within nested function
        x = "nonlocal"
    inner()
    print(x)  # Prints "nonlocal"


outer()

# Demonstrate the use of 'yield from'
def generator():
    yield from range(3)  # Yield from an iterable


for i in generator():
    print(i)  # Prints numbers 0, 1, 2

# Demonstrate the use of matrix multiplication operator
import numpy as np

a = np.array([[1, 0], [0, 1]])  # Define matrix a
b = np.array([[4, 1], [2, 2]])  # Define matrix b
print(a @ b)  # Perform matrix multiplication

# Demonstrate the use of pathlib for file operations
p = pathlib.Path('.')  # Create a Path object for the current directory
for file in p.glob('*.py'):  # Iterate over Python files in the current directory
    print(file)  # Print the file names

# Demonstrate the use of type hints in function definitions
def greet(name: str) -> str:  # Type hint for argument and return type
    return "Hello, " + name


print(greet("Alice"))  # Call the function with a string argument

# Demonstrate the use of shutil.which to find the path of an executable
print(shutil.which("python"))  # Print the path of the Python executable
